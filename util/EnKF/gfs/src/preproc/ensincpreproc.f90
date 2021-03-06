program ensincpreproc

! pre-process ensemble increment data so that it can be
! read in by GSI to compute incremental balance.
! requires knowledge of GSI subdomain info (from text file subdomains.dat).

 use sigio_module, only: &
 sigio_axdata,sigio_sclose,sigio_sropen,sigio_srhead,sigio_srohdc,sigio_head,sigio_data
 use specmod, only: sptez_s,sptezv_s,init_spec_vars
 use mpi
 use omp_lib, only: omp_get_num_threads

 implicit none
 type(sigio_head) :: sighead,sighead2
 type(sigio_data) :: sigdata,sigdata2
 character(len=120) filenamein,filenameout,filenamein2
 integer nlats,nlons,nlevs,ntrac,ntrunc,k,ierr,nanals,&
         nanal,numproc,nproc,nt,num_threads,&
         ndomains,nsub,istart,jstart,ilat1,jlon1,ntask
 integer :: iunit=11
 integer :: iunit2=12
 character(len=10) datestring
 character(len=4) charnlons,charnlats
 character(len=3) charnanal
 real, dimension(:,:), allocatable :: &
 psg,subdgrd2,psga,psg1,tmpwork,tmpworkv
 real, dimension(:,:,:), allocatable :: ug,vg,tempg,subdgrd3,ug1,vg1,tempg1
 real, dimension(:,:,:), allocatable :: uga,vga,tempga
 real, dimension(:,:,:,:), allocatable :: qg,qg1,qga

 call MPI_Init(ierr)
 ! nproc is process number, numproc is total number of processes.
 call MPI_Comm_rank(MPI_COMM_WORLD,nproc,ierr)
 call MPI_Comm_size(MPI_COMM_WORLD,numproc,ierr)

 ! get nanals,datestring,nlons,nlats
 ! from command line on every task.
 call getarg(1,charnlons)
 read(charnlons,'(i4)') nanals
 call getarg(2,datestring)
 call getarg(3,charnlons)
 call getarg(4,charnlats)
 read(charnlons,'(i4)') nlons
 read(charnlats,'(i4)') nlats

 if (numproc .lt. nanals) then
    print *,'numproc =',numproc,' nanals =',nanals
    print *,'numproc too small (must be at least nanals), aborting!'
    go to 999
 end if

 filenamein = "sfg_"//datestring//"_fhr06_mem001"
 ! only need header here.
 call sigio_sropen(iunit,trim(filenamein),ierr)
 if (ierr .ne. 0) then
    print *,'cannot read file',trim(filenamein),' aborting!'
    go to 999
 end if
 call sigio_srhead(iunit,sighead,ierr)
 call sigio_sclose(iunit,ierr)

 ntrunc = sighead%jcap
 ntrac = sighead%ntrac
 nlevs = sighead%levs
 if (nproc .eq. 0) then
    print *,'nlons,nlats,nlevs,ntrunc,ntrac,nanals=',nlons,nlats,nlevs,ntrunc,ntrac,nanals
 endif

 nanal = nproc + 1
 write(charnanal,'(i3.3)') nanal

 ! for tasks > nanals, skip to end
 if (nanal .le. nanals) then

 filenamein = "sfg_"//datestring//"_fhr06_mem"//charnanal
 filenamein2 = "sanl_"//datestring//"_mem"//charnanal
 print *,nproc,nanal,trim(filenamein)

 allocate(psg(nlons,nlats+2))
 allocate(ug(nlons,nlats+2,nlevs))
 allocate(vg(nlons,nlats+2,nlevs))
 allocate(tempg(nlons,nlats+2,nlevs))
 allocate(qg(nlons,nlats+2,nlevs,ntrac))

 allocate(psga(nlons,nlats+2))
 allocate(uga(nlons,nlats+2,nlevs))
 allocate(vga(nlons,nlats+2,nlevs))
 allocate(tempga(nlons,nlats+2,nlevs))
 allocate(qga(nlons,nlats+2,nlevs,ntrac))

 call init_spec_vars(nlons,nlats,ntrunc,4)

! read each ensemble member, transform to grid

 call sigio_srohdc(iunit,trim(filenamein),sighead,sigdata,ierr)
 if (ierr /= 0) then
    print *,'failed reading',trim(filenamein2),' aborting!'
    go to 999
 endif
 call sigio_srohdc(iunit,trim(filenamein2),sighead2,sigdata2,ierr)
 if (ierr /= 0) then
    print *,'failed reading',trim(filenamein2),' aborting!'
    go to 999
 endif
!$omp parallel
 num_threads = omp_get_num_threads()
!$omp end parallel
 if (nanal .eq. 1 .or. nanal .eq. nanals) print *,num_threads,' threads'
 if (num_threads > 1) then
    ! threaded version uses more memory for temporary arrays.
    allocate(ug1(nlons,nlats,nlevs))
    allocate(vg1(nlons,nlats,nlevs))
    allocate(tempg1(nlons,nlats,nlevs))
    allocate(qg1(nlons,nlats,nlevs,ntrac))
    allocate(psg1(nlons,nlats))
   ! psg on gaussian grid
    call sptez_s(sigdata%ps,psg1,1)
   ! add pole rows, flip latitude direction (N->S to S->N).
    call fill_ns(nlons,nlats+2,psg1,psg)
    psg = exp(psg) ! convert to cb.
    print *,nanal,'psg',minval(10*psg),maxval(10*psg)
    call sptez_s(sigdata2%ps,psg1,1)
    call fill_ns(nlons,nlats+2,psg1,psga)
    psga = exp(psga) ! convert to cb.
    psga = psga - psg
    print *,nanal,'psginc',minval(10.*psga),maxval(10.*psga)
   !==> get U,V,tv,q,oz,clwmr perts on gaussian grid.
   ! also add pole rows, flip latitude direction (N->S to S->N).
   !$omp parallel do private(k,nt)
    do k=1,nlevs
       call sptezv_s(sigdata%d(:,k),sigdata%z(:,k),ug1(1,1,k),vg1(1,1,k),1)
       call filluv_ns(nlons,nlats+2,ug1(1,1,k),vg1(1,1,k),ug(1,1,k),vg(1,1,k))
       call sptez_s(sigdata%t(:,k),tempg1(1,1,k),1)
       call fill_ns(nlons,nlats+2,tempg1(1,1,k),tempg(1,1,k))
       do nt=1,ntrac
          call sptez_s(sigdata%q(:,k,nt),qg1(1,1,k,nt),1)
          call fill_ns(nlons,nlats+2,qg1(1,1,k,nt),qg(1,1,k,nt))
       enddo
    enddo
   !$omp parallel do private(k,nt)
    do k=1,nlevs
       call sptezv_s(sigdata2%d(:,k),sigdata2%z(:,k),ug1(1,1,k),vg1(1,1,k),1)
       call filluv_ns(nlons,nlats+2,ug1(1,1,k),vg1(1,1,k),uga(1,1,k),vga(1,1,k))
       call sptez_s(sigdata2%t(:,k),tempg1(1,1,k),1)
       call fill_ns(nlons,nlats+2,tempg1(1,1,k),tempga(1,1,k))
       uga(:,:,k)=uga(:,:,k)-ug(:,:,k)
       vga(:,:,k)=vga(:,:,k)-vg(:,:,k)
       tempga(:,:,k)=tempga(:,:,k)-tempg(:,:,k)
       do nt=1,ntrac
          call sptez_s(sigdata2%q(:,k,nt),qg1(1,1,k,nt),1)
          call fill_ns(nlons,nlats+2,qg1(1,1,k,nt),qga(1,1,k,nt))
          qga(:,:,k,nt)=qga(:,:,k,nt)-qg(:,:,k,nt)
       enddo
       if (nproc .eq. 0) print *,k,'tempginc',minval(tempga(:,:,k)),maxval(tempga(:,:,k))
    enddo
    deallocate(ug1,vg1,tempg1,qg1,psg1)
 else
   ! if single-threaded, don't need as much temporary space.
    allocate(tmpwork(nlons,nlats),tmpworkv(nlons,nlats))
   ! psg on gaussian grid.
    call sptez_s(sigdata%ps,tmpwork,1)
   ! add pole rows, flip latitude direction (N->S to S->N).
    call fill_ns(nlons,nlats+2,tmpwork,psg)
    psg = exp(psg) ! convert to cb.
    print *,nanal,'psg',minval(10*psg),maxval(10*psg)
    call sptez_s(sigdata2%ps,tmpwork,1)
   ! add pole rows, flip latitude direction (N->S to S->N).
    call fill_ns(nlons,nlats+2,tmpwork,psga)
    psga = exp(psga) ! convert to cb.
    psga = psga - psg
    print *,nanal,'psginc',minval(10.*psga),maxval(10.*psga)
   !==> get U,V,tv,q,oz,clwmr perts on gaussian grid.
   ! add pole rows, flip latitude direction (N->S to S->N).
    do k=1,nlevs
       call sptezv_s(sigdata%d(:,k),sigdata%z(:,k),tmpwork,tmpworkv,1)
       call filluv_ns(nlons,nlats+2,tmpwork,tmpworkv,ug(:,:,k),vg(:,:,k))
       call sptezv_s(sigdata2%d(:,k),sigdata2%z(:,k),tmpwork,tmpworkv,1)
       call filluv_ns(nlons,nlats+2,tmpwork,tmpworkv,uga(:,:,k),vga(:,:,k))
       call sptez_s(sigdata%t(:,k),tmpwork,1)
       call fill_ns(nlons,nlats+2,tmpwork,tempg(:,:,k))
       call sptez_s(sigdata2%t(:,k),tmpwork,1)
       call fill_ns(nlons,nlats+2,tmpwork,tempga(:,:,k))
       uga(:,:,k)=uga(:,:,k)-ug(:,:,k)
       vga(:,:,k)=vga(:,:,k)-vg(:,:,k)
       tempga(:,:,k)=tempga(:,:,k)-tempg(:,:,k)
       do nt=1,ntrac
          call sptez_s(sigdata%q(:,k,nt),tmpwork,1)
          call fill_ns(nlons,nlats+2,tmpwork,qg(:,:,k,nt))
          call sptez_s(sigdata2%q(:,k,nt),tmpwork,1)
          call fill_ns(nlons,nlats+2,tmpwork,qga(:,:,k,nt))
          qga(:,:,k,nt)=qga(:,:,k,nt)-qg(:,:,k,nt)
       enddo
       if (nproc .eq. 0) print *,k,'tempginc',minval(tempga(:,:,k)),maxval(tempga(:,:,k))
    enddo
    deallocate(tmpworkv,tmpwork)
 endif

! deallocate sigdata structure, close file
 call sigio_axdata(sigdata,ierr)
 call sigio_axdata(sigdata2,ierr)
 call sigio_sclose(iunit,ierr)

! partition and write out data on subdomains.

 open(iunit2,form='formatted',file='subdomains.dat',iostat=ierr)
 if (ierr /= 0) then
    print *,'error reading subdomains.dat file on task',nproc,' aborting!'
    go to 999
 end if
 read(iunit2,*) ndomains
 do nsub = 1,ndomains
    ! read subdomain info for gsi.
    read(iunit2,*) ntask,istart,jstart,ilat1,jlon1
    if (nproc .eq. 0) print *,ntask,istart,jstart,ilat1,jlon1
    write(charnlats,'(i4.4)') ntask
    filenameout='enkfincmem'//charnanal//".pe"//charnlats
    open(iunit,file=filenameout,form='unformatted')
    allocate(subdgrd2(ilat1+2,jlon1+2))
    allocate(subdgrd3(ilat1+2,jlon1+2,nlevs))
    ! subset grid for this GSI task, add buffers, take transpose (1st dim lat, 2nd dim lon).
    call subset(istart,jstart,ilat1,jlon1,nlons,nlats+2,subdgrd2,psga)
    ! write out.
    write(iunit) subdgrd2
    do k=1,nlevs
       call subset(istart,jstart,ilat1,jlon1,nlons,nlats+2,subdgrd3(1,1,k),uga(1,1,k))
    enddo
    write(iunit) subdgrd3
    do k=1,nlevs
       call subset(istart,jstart,ilat1,jlon1,nlons,nlats+2,subdgrd3(1,1,k),vga(1,1,k))
    enddo
    write(iunit) subdgrd3
    do k=1,nlevs
       call subset(istart,jstart,ilat1,jlon1,nlons,nlats+2,subdgrd3(1,1,k),tempga(1,1,k))
    enddo
    write(iunit) subdgrd3
    do k=1,nlevs
       call subset(istart,jstart,ilat1,jlon1,nlons,nlats+2,subdgrd3(1,1,k),qga(1,1,k,1))
    enddo
    write(iunit) subdgrd3
    do k=1,nlevs
       call subset(istart,jstart,ilat1,jlon1,nlons,nlats+2,subdgrd3(1,1,k),qga(1,1,k,2))
    enddo
    write(iunit) subdgrd3
    close(iunit)
    deallocate(subdgrd2,subdgrd3)
 enddo
 close(iunit2)

! deallocate arrays
 deallocate(psga,uga,vga,tempga,qga,psg,ug,vg,tempg,qg)

 endif ! nanal > nanals

999 continue
 call MPI_Barrier(MPI_COMM_WORLD,ierr)
 call MPI_Finalize(ierr)

end program ensincpreproc

subroutine fill_ns(nlon,nlat,grid_in,grid_out)
   integer, intent(in) :: nlon,nlat
   real,dimension(nlon,nlat-2),intent(in   ) :: grid_in  ! input grid
   real,dimension(nlon,nlat)  ,intent(  out) :: grid_out ! output grid
   integer i,j,jj
   real sumn,sums
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,nlat-1
      jj=nlat-j
      do i=1,nlon
         grid_out(i,j)=grid_in(i,jj)
      end do
   end do
!  Compute mean along southern and northern latitudes
   sumn=0.
   sums=0.
   do i=1,nlon
      sumn=sumn+grid_in(i,1)
      sums=sums+grid_in(i,nlat-2)
   end do
   sumn=sumn/float(nlon)
   sums=sums/float(nlon)
!  Load means into output array
   do i=1,nlon
      grid_out(i,1)    =sums
      grid_out(i,nlat) =sumn
   end do
 end subroutine fill_ns

 subroutine filluv_ns(nlon,nlat,gridu_in,gridv_in,gridu_out,gridv_out)
   integer, intent(in) :: nlon,nlat
   real,dimension(nlon,nlat-2),intent(in   ) :: gridu_in,gridv_in   ! input grid
   real,dimension(nlon,nlat)  ,intent(  out) :: gridu_out,gridv_out ! output grid
   integer i,j,jj
   real polnu,polnv,polsu,polsv,lon,pi
   real clons(nlon),slons(nlon)
   pi = 4.*atan(1.0)
!  Reverse ordering in j direction from n-->s to s-->n
   do j=2,nlat-1
      jj=nlat-j
      do i=1,nlon
         gridu_out(i,j)=gridu_in(i,jj)
         gridv_out(i,j)=gridv_in(i,jj)
      end do
   end do
!  Compute mean along southern and northern latitudes
   polnu=0
   polnv=0
   polsu=0
   polsv=0
   do i=1,nlon
      lon = float(i-1)*2.*pi/nlon
      clons(i) = cos(lon)
      slons(i) = sin(lon)
      polnu=polnu+gridu_out(i,nlat-1)*clons(i)-gridv_out(i,nlat-1)*slons(i)
      polnv=polnv+gridu_out(i,nlat-1)*slons(i)+gridv_out(i,nlat-1)*clons(i)
      polsu=polsu+gridu_out(i,2     )*clons(i)+gridv_out(i,2     )*slons(i)
      polsv=polsv+gridu_out(i,2     )*slons(i)-gridv_out(i,2     )*clons(i)
   end do
   polnu=polnu/float(nlon)
   polnv=polnv/float(nlon)
   polsu=polsu/float(nlon)
   polsv=polsv/float(nlon)
! Load means into output array.
   do i=1,nlon
      gridu_out(i,nlat)= polnu*clons(i)+polnv*slons(i)
      gridv_out(i,nlat)=-polnu*slons(i)+polnv*clons(i)
      gridu_out(i,1   )= polsu*clons(i)+polsv*slons(i)
      gridv_out(i,1   )= polsu*slons(i)-polsv*clons(i)
   end do
end subroutine filluv_ns

subroutine subset(istart,jstart,ilat1,jlon1,nlons,nlats,subdgrd,grdin)
    ! split grid into subdomain, including buffers. Flip lon and lat dims.
    integer, intent(in) :: istart,jstart,ilat1,jlon1,nlons,nlats
    real, intent(in) :: grdin(nlons,nlats)
    real, intent(out) :: subdgrd(ilat1+2,jlon1+2)
    integer i,j,ii,jj
    do j=1,jlon1+2
       jj = jstart+j-2
       if (jj == 0) jj = 1
       if (jj == nlons+1) jj = nlons
       do i=1,ilat1+2
          ii = istart+i-2
          if (ii == 0) ii = 1
          if (ii == nlats+1) ii = nlats
          subdgrd(i,j) = grdin(jj,ii)
       enddo
    enddo
end subroutine subset
