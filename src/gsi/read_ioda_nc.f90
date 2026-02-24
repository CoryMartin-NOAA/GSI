subroutine read_ioda_nc(mype,val_tovs,ithin,isfcalc,&
     rmesh,jsatid,gstime,infile,lunout,obstype,&
     nread,ndata,nodata,twind,sis, &
     mype_root,mype_sub,npe_sub,mpi_comm_sub, nobs, &
     nrec_start,nrec_start_ears,nrec_start_db,dval_use,radmod)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_ioda_nc                  read ioda netcdf radiance data
!
! abstract:  This routine reads IODA v2 netCDF radiance files.
!
! program history log:
!   2025-02-13 Jules - initial version
!
!   input argument list:
!     mype     - mpi task id
!     val_tovs - weighting factor applied to super obs
!     ithin    - flag to thin data
!     isfcalc  - method to calculate surface fields within FOV
!     rmesh    - thinning mesh size (km)
!     jsatid   - platform to read (e.g., n19)
!     gstime   - analysis time in minutes from reference date
!     infile   - input ioda netcdf file
!     lunout   - unit for output data
!     obstype  - observation type (e.g., amsua)
!     twind    - time window (hours)
!     sis      - sensor/instrument/satellite string
!     mype_root- root task for this instrument
!     mype_sub - task id within instrument group
!     npe_sub  - number of tasks in instrument group
!     mpi_comm_sub - mpi communicator for instrument group
!     nobs     - observation count (output)
!     nrec_start - start record
!     nrec_start_ears - start record for EARS (not used)
!     nrec_start_db   - start record for DB (not used)
!     dval_use - flag to use extra info
!     radmod   - radiance info structure
!
!$$$ end subprogram documentation block

  use kinds, only: i_kind, r_kind, r_single, r_double
  use constants, only: r360, zero, r60inv, deg2rad, rad2deg, &
                       rearth, half, one, two, pi, quart, r60
  use obs_para, only: iwinbgn,iwinend,winlen,l4dvar,l4densvar,time_window_max
  use netcdf
  use netcdf_mod, only: nc_check
  use mpeu_util, only: die, warn
  use gsi_nstcouplermod, only: nst_gsi,nstinfo
  use satthin, only: map2tgrid2, tdiff2crit, makegrids, radthin_time_info
  use radiance_mod, only: radinfo_type
  use gridmod, only: regional, nlat, nlon, rlats, rlons, tll2xy

  implicit none

  ! Arguments
  integer(i_kind), intent(in) :: mype
  real(r_kind), intent(in) :: val_tovs
  logical, intent(in) :: ithin
  integer(i_kind), intent(in) :: isfcalc
  real(r_kind), intent(in) :: rmesh
  character(len=*), intent(in) :: jsatid, infile, obstype
  integer(i_kind), intent(in) :: lunout
  integer(i_kind), intent(inout) :: nread, ndata, nodata
  real(r_kind), intent(in) :: gstime, twind
  character(len=*), intent(inout) :: sis
  integer(i_kind), intent(in) :: mype_root, mype_sub, npe_sub, mpi_comm_sub
  integer(i_kind), intent(inout) :: nobs
  integer(i_kind), intent(inout) :: nrec_start, nrec_start_ears, nrec_start_db
  logical, intent(in) :: dval_use
  type(radinfo_type), intent(in) :: radmod

  ! Local parameters
  integer(i_kind), parameter :: maxinfo_def = 31
  real(r_kind), parameter :: tbmin = 50.0_r_kind
  real(r_kind), parameter :: tbmax = 450.0_r_kind
  character(len=*), parameter :: myname = 'read_ioda_nc'

  ! Local variables
  integer(i_kind) :: ncid, ierr, grpid_meta, grpid_val
  integer(i_kind) :: nlocs, nchans, maxinfo, nreal, nele
  integer(i_kind) :: nlocs_id, nchans_id
  integer(i_kind) :: lat_id, lon_id, time_id
  integer(i_kind) :: lza_id, saza_id, va_id, sp_id
  integer(i_kind) :: solz_id, sola_id, bt_id
  integer(i_kind) :: j, k, iob, n
  integer(i_kind) :: kidsat, instrument
  integer(i_kind) :: itx, itt, n_tbin
  integer(i_kind), pointer :: it_mesh => null()
  integer(i_kind), dimension(5) :: idate5
  integer(i_kind) :: nmind
  integer(i_kind) :: ilon, ilat

  real(r_kind) :: dlat, dlon, dlat_earth, dlon_earth
  real(r_kind) :: t4dv, sstime, tdiff, ptime
  real(r_kind) :: crit0, crit1, dist1, score
  real(r_kind) :: timeinflat = 2.0_r_kind
  logical :: iuse, ithin_time, outside

  real(r_single), allocatable :: lats(:), lons(:)
  real(r_single), allocatable :: lzas(:), sazas(:), vas(:), sps(:)
  real(r_single), allocatable :: solzs(:), solas(:)
  real(r_single), allocatable :: bts(:,:)
  character(len=20), allocatable :: datetimes(:)

  real(r_kind), allocatable :: data_all(:,:)

  ! Initialize
  kidsat = radmod%kidsat
  instrument = radmod%instrument
  sis = trim(obstype)//'_'//trim(jsatid)
  ilon = 3
  ilat = 4

  if (mype == mype_root) write(6,*) 'Reading IODA NetCDF file: ', trim(infile)

  ! Open file
  ierr = nf90_open(trim(infile), nf90_nowrite, ncid)
  if (ierr /= nf90_noerr) then
     call warn(myname, 'Failed to open file: '//trim(infile), trim(nf90_strerror(ierr)))
     return
  endif

  ! Get groups
  ierr = nf90_inq_grp_ncid(ncid, 'MetaData', grpid_meta)
  call nc_check(ierr, myname, 'MetaData group')
  ierr = nf90_inq_grp_ncid(ncid, 'ObsValue', grpid_val)
  call nc_check(ierr, myname, 'ObsValue group')

  ! Get dimensions
  ierr = nf90_inq_dimid(ncid, 'nlocs', nlocs_id)
  call nc_check(ierr, myname, 'nlocs dim')
  ierr = nf90_inquire_dimension(ncid, nlocs_id, len = nlocs)

  ierr = nf90_inq_dimid(ncid, 'nchans', nchans_id)
  if (ierr == nf90_noerr) then
     ierr = nf90_inquire_dimension(ncid, nchans_id, len = nchans)
  else
     ! For non-radiance or if nchans is not used
     nchans = 1
  endif

  if (mype == mype_root) write(6,*) 'nlocs = ', nlocs, ' nchans = ', nchans

  ! Allocate and read MetaData
  allocate(lats(nlocs), lons(nlocs), datetimes(nlocs))
  allocate(lzas(nlocs), sazas(nlocs), vas(nlocs), sps(nlocs))
  allocate(solzs(nlocs), solas(nlocs))

  lzas = zero
  sazas = zero
  vas = zero
  sps = zero
  solzs = zero
  solas = zero

  ierr = nf90_inq_varid(grpid_meta, 'latitude', lat_id)
  call nc_check(ierr, myname, 'latitude')
  ierr = nf90_get_var(grpid_meta, lat_id, lats)

  ierr = nf90_inq_varid(grpid_meta, 'longitude', lon_id)
  call nc_check(ierr, myname, 'longitude')
  ierr = nf90_get_var(grpid_meta, lon_id, lons)

  ierr = nf90_inq_varid(grpid_meta, 'dateTime', time_id)
  call nc_check(ierr, myname, 'dateTime')
  ierr = nf90_get_var(grpid_meta, time_id, datetimes)

  ! Optional metadata
  ierr = nf90_inq_varid(grpid_meta, 'sensorZenithAngle', lza_id)
  if (ierr == nf90_noerr) ierr = nf90_get_var(grpid_meta, lza_id, lzas)

  ierr = nf90_inq_varid(grpid_meta, 'sensorAzimuthAngle', saza_id)
  if (ierr == nf90_noerr) ierr = nf90_get_var(grpid_meta, saza_id, sazas)

  ierr = nf90_inq_varid(grpid_meta, 'sensorViewAngle', va_id)
  if (ierr == nf90_noerr) ierr = nf90_get_var(grpid_meta, va_id, vas)

  ierr = nf90_inq_varid(grpid_meta, 'sensorScanPosition', sp_id)
  if (ierr == nf90_noerr) ierr = nf90_get_var(grpid_meta, sp_id, sps)

  ierr = nf90_inq_varid(grpid_meta, 'solarZenithAngle', solz_id)
  if (ierr == nf90_noerr) ierr = nf90_get_var(grpid_meta, solz_id, solzs)

  ierr = nf90_inq_varid(grpid_meta, 'solarAzimuthAngle', sola_id)
  if (ierr == nf90_noerr) ierr = nf90_get_var(grpid_meta, sola_id, solas)

  ! Read ObsValue/brightnessTemperature
  allocate(bts(nchans, nlocs))
  ierr = nf90_inq_varid(grpid_val, 'brightnessTemperature', bt_id)
  call nc_check(ierr, myname, 'brightnessTemperature')
  ierr = nf90_get_var(grpid_val, bt_id, bts)

  ierr = nf90_close(ncid)

  ! Thinning setup
  call radthin_time_info(obstype, jsatid, sis, ptime, ithin_time)
  if (ptime > 0.0_r_kind) then
     n_tbin = nint(2.0_r_kind * time_window_max / ptime)
  else
     n_tbin = 1
  endif
  call makegrids(rmesh, ithin, n_tbin = n_tbin)

  ! Allocate GSI data arrays
  maxinfo = maxinfo_def
  if (dval_use) maxinfo = maxinfo + 2
  nreal = maxinfo + nstinfo
  nele = nreal + nchans

  allocate(data_all(nele, nlocs))
  data_all = zero
  iob = 0
  it_mesh => null()

  ! Loop over locations
  do n = 1, nlocs
     ! Parse time
     ! dateTime format: 2021-08-01T18:00:00Z
     read(datetimes(n)(1:4), '(I4)') idate5(1) ! year
     read(datetimes(n)(6:7), '(I2)') idate5(2) ! month
     read(datetimes(n)(9:10), '(I2)') idate5(3) ! day
     read(datetimes(n)(12:13), '(I2)') idate5(4) ! hour
     read(datetimes(n)(15:16), '(I2)') idate5(5) ! minute

     call w3fs21(idate5, nmind)

     ! Time relative to window start (hours)
     t4dv = (real(nmind - iwinbgn, r_kind) + real(0, r_kind)*r60inv)*r60inv
     sstime = real(nmind, r_kind)
     tdiff = (sstime - gstime)*r60inv ! hours from analysis time

     ! Time window check
     if (l4dvar .or. l4densvar) then
        if (t4dv < zero .or. t4dv > winlen) cycle
     else
        if (abs(tdiff) > twind) cycle
     endif

     dlat_earth = real(lats(n), r_kind)
     dlon_earth = real(lons(n), r_kind)
     if (dlon_earth < zero) dlon_earth = dlon_earth + r360
     if (dlon_earth >= r360) dlon_earth = dlon_earth - r360

     ! Thinning
     crit0 = 0.1_r_kind ! Dummy terrain
     call tdiff2crit(tdiff, ptime, ithin_time, timeinflat, crit0, crit1, it_mesh)

     iuse = .true.
     itx = 0
     itt = 0
     score = zero
     if (ithin) then
        call map2tgrid2(dlat_earth*deg2rad, dlon_earth*deg2rad, dist1, crit1, itx, ithin, itt, iuse, sis, score, it_mesh)
     endif

     if (.not. iuse) cycle

     ! Coordinate conversion
     if (regional) then
        call tll2xy(dlon_earth*deg2rad, dlat_earth*deg2rad, dlon, dlat, outside)
        if (outside) cycle
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd1(dlat, rlats, nlat, 1)
        call grdcrd1(dlon, rlons, nlon, 1)
     endif

     ! Increment observation count
     iob = iob + 1
     nread = nread + nchans

     ! Fill data_all
     data_all(1, iob) = real(kidsat, r_kind)
     data_all(2, iob) = t4dv
     data_all(3, iob) = dlon
     data_all(4, iob) = dlat
     data_all(5, iob) = real(lzas(n), r_kind) * deg2rad
     data_all(6, iob) = real(sazas(n), r_kind) * deg2rad
     data_all(7, iob) = real(vas(n), r_kind) * deg2rad
     data_all(8, iob) = real(sps(n), r_kind)
     data_all(9, iob) = real(solzs(n), r_kind) * deg2rad
     data_all(10, iob) = real(solas(n), r_kind) * deg2rad

     data_all(30, iob) = dlon_earth
     data_all(31, iob) = dlat_earth
     if (dval_use) then
        data_all(32, iob) = val_tovs
        data_all(33, iob) = real(itt, r_kind)
     endif

     ! Brightness temperatures
     do j = 1, nchans
        data_all(nreal + j, iob) = real(bts(j, n), r_kind)
        if (data_all(nreal + j, iob) > tbmin .and. data_all(nreal + j, iob) < tbmax) nodata = nodata + 1
     end do

  end do

  ndata = ndata + iob

  ! Write to lunout
  if (mype == mype_root) then
     call count_obs(iob, nele, ilat, ilon, data_all, nobs)
     write(6,*) 'Writing ', iob, ' observations to lunout'
     write(lunout) obstype, sis, nreal, nchans, ilat, ilon
     write(lunout) ((data_all(k, n), k = 1, nele), n = 1, iob)
  endif

  ! Deallocate
  if (allocated(lats)) deallocate(lats)
  if (allocated(lons)) deallocate(lons)
  if (allocated(datetimes)) deallocate(datetimes)
  if (allocated(lzas)) deallocate(lzas)
  if (allocated(sazas)) deallocate(sazas)
  if (allocated(vas)) deallocate(vas)
  if (allocated(sps)) deallocate(sps)
  if (allocated(solzs)) deallocate(solzs)
  if (allocated(solas)) deallocate(solas)
  if (allocated(bts)) deallocate(bts)
  if (allocated(data_all)) deallocate(data_all)

end subroutine read_ioda_nc
