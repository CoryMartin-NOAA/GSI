module read_ioda_mod
!$$$  module documentation block
!
! module: read_ioda_mod
!
! abstract: Placeholder module containing stub subroutines for reading
!           conventional and radiance observations from IODA-format files.
!
! subroutines:
!   read_ioda_conv  - read conventional observations from an IODA file
!   read_ioda_rad   - read radiance observations from an IODA file
!
!$$$

  use kinds, only: r_kind, i_kind
  implicit none

  private
  public :: read_ioda_conv
  public :: read_ioda_rad

contains

  subroutine read_ioda_conv(nread,ndata,nodata,infile,obstype,lunout, &
                             twind,sis,prsl_full,hgtl_full,nobs,read_rec)
!$$$  subprogram documentation block
!
! subprogram: read_ioda_conv
!
! abstract: Stub subroutine - read conventional observations from an IODA
!           formatted file.  No observations are currently read; this
!           routine is a placeholder for future implementation.
!
!   input argument list:
!     infile   - path to the IODA file
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     prsl_full - 3-d guess pressure (full grid)
!     hgtl_full - 3-d guess geopotential height (full grid)
!     read_rec - first record to read
!
!   output argument list:
!     nread    - number of observations read
!     ndata    - number of observations retained for further processing
!     nodata   - number of observations retained for further processing
!     nobs     - number of observations for this subdomain
!
!$$$
    use kinds,     only: r_kind, i_kind

    implicit none

    ! Argument declarations
    integer(i_kind),                       intent(inout) :: nread
    integer(i_kind),                       intent(inout) :: ndata
    integer(i_kind),                       intent(inout) :: nodata
    character(120),                        intent(in   ) :: infile
    character(10),                         intent(in   ) :: obstype
    integer(i_kind),                       intent(in   ) :: lunout
    real(r_kind),                          intent(in   ) :: twind
    character(20),                         intent(in   ) :: sis
    real(r_kind),  allocatable, dimension(:,:,:), intent(in) :: prsl_full
    real(r_kind),  allocatable, dimension(:,:,:), intent(in) :: hgtl_full
    integer(i_kind),                       intent(inout) :: nobs
    integer(i_kind),                       intent(inout) :: read_rec

    ! Placeholder - no observations read
    write(6,*) 'READ_IODA_CONV: placeholder called for obstype=', trim(obstype), &
               ' infile=', trim(infile)
    nread  = 0
    ndata  = 0
    nodata = 0

    return
  end subroutine read_ioda_conv


  subroutine read_ioda_rad(mype,val_dat,ithin,isfcalc,rmesh,jsatid,gstime, &
                            infile,lunout,obstype,nread,ndata,nodata,twind,sis, &
                            mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
                            read_rec,read_ears_rec,read_db_rec,dval_use,radmod)
!$$$  subprogram documentation block
!
! subprogram: read_ioda_rad
!
! abstract: Stub subroutine - read radiance observations from an IODA
!           formatted file.  No observations are currently read; this
!           routine is a placeholder for future implementation.
!
!   input argument list:
!     mype         - mpi task id
!     val_dat      - weighting factor applied to super obs
!     ithin        - flag to thin data
!     isfcalc      - flag to calculate surface fields
!     rmesh        - thinning mesh size (km)
!     jsatid       - satellite platform id
!     gstime       - analysis time in minutes from reference date
!     infile       - path to the IODA file
!     lunout       - unit to which to write data for further processing
!     obstype      - observation type to process
!     twind        - input group time window (hours)
!     sis          - satellite/instrument/sensor indicator
!     mype_root    - root task for this observation type
!     mype_sub     - mpi task id within subcommunicator
!     npe_sub      - number of mpi tasks in subcommunicator
!     mpi_comm_sub - mpi subcommunicator
!     read_rec     - first record to read
!     read_ears_rec - first EARS record to read
!     read_db_rec  - first direct broadcast record to read
!     dval_use     - logical flag indicating dval weighting is used
!     radmod       - radiance observation type structure
!
!   output argument list:
!     nread    - number of observations read
!     ndata    - number of observations retained for further processing
!     nodata   - number of observations retained for further processing
!     nobs     - number of observations for this subdomain
!
!$$$
    use kinds,         only: r_kind, i_kind
    use radiance_mod,  only: rad_obs_type

    implicit none

    ! Argument declarations
    integer(i_kind),       intent(in   ) :: mype
    real(r_kind),          intent(in   ) :: val_dat
    integer(i_kind),       intent(in   ) :: ithin
    integer(i_kind),       intent(in   ) :: isfcalc
    real(r_kind),          intent(in   ) :: rmesh
    character(11),         intent(in   ) :: jsatid
    real(r_kind),          intent(in   ) :: gstime
    character(120),        intent(in   ) :: infile
    integer(i_kind),       intent(in   ) :: lunout
    character(10),         intent(in   ) :: obstype
    integer(i_kind),       intent(inout) :: nread
    integer(i_kind),       intent(inout) :: ndata
    integer(i_kind),       intent(inout) :: nodata
    real(r_kind),          intent(in   ) :: twind
    character(20),         intent(in   ) :: sis
    integer(i_kind),       intent(in   ) :: mype_root
    integer(i_kind),       intent(in   ) :: mype_sub
    integer(i_kind),       intent(in   ) :: npe_sub
    integer(i_kind),       intent(in   ) :: mpi_comm_sub
    integer(i_kind),       intent(inout) :: nobs
    integer(i_kind),       intent(inout) :: read_rec
    integer(i_kind),       intent(inout) :: read_ears_rec
    integer(i_kind),       intent(inout) :: read_db_rec
    logical,               intent(in   ) :: dval_use
    type(rad_obs_type),    intent(inout) :: radmod

    ! Placeholder - no observations read
    write(6,*) 'READ_IODA_RAD: placeholder called for obstype=', trim(obstype), &
               ' infile=', trim(infile)
    nread  = 0
    ndata  = 0
    nodata = 0

    return
  end subroutine read_ioda_rad

end module read_ioda_mod
