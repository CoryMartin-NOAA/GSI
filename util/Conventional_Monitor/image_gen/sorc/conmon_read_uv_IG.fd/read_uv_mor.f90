!-------------------------------------------------------------
! subroutine read_uv_mor
!
!-------------------------------------------------------------
subroutine read_uv_mor( nreal, dtype, fname, fileo, gtross, rlev, grads_info_file )

   implicit none

   !-------------
   !  interface 
   !
   integer,  intent(in) :: nreal
   character*15,  intent(in) :: dtype 
   character*200, intent(in) :: fname
   character*50,  intent(in) :: fileo
   real,          intent(in) :: gtross
   real(4),       intent(in) :: rlev
   character*50,  intent(in) :: grads_info_file

   !-------------
   !  local vars 
   !
   real(4),allocatable,dimension(:,:)  :: rdiag
   real(4),dimension(3,3000000) :: rpress
   real(4),dimension(3,3000000) :: rpressu
   real(4),dimension(3,3000000) :: rpressv
   integer,dimension(3) :: ncount,ncount_vqc,ncount_gros

   character*50 fileu,filev,fileou2,fileov2

   real rgtross
   integer nobs,ntotal,ngross,nreal_in,nlev
   integer i,ndup,nlat,nlon,npres,ntime
   integer ilat,ilon,ipres,itime,iqc,iuse,imuse,iweight,ierr,ierr2,ierr3,iobu,iogu,iobv,iogv
   real(4) :: rmiss,vqclmt,vqclmte

   data rmiss/-999.0/ 

   ncount=0
   rpress=rmiss
   ncount_vqc=0
   ncount_gros=0

   fileu   = 'out_u'
   filev   = 'out_v'
   fileou2 = 'stdout_u'
   fileov2 = 'stdout_v'


   ntotal=0
   open(unit=11,file=fname,form='unformatted')
   rewind(11)
   read(11) nobs,nreal_in

   if (nreal /= nreal_in) then
      print *,'nreal_in,nreal ',nreal_in,nreal
      stop
   endif 

   allocate(rdiag(nreal,nobs))
   read(11) rdiag


   ilat=1                          !  lat
   ilon=2                          !  lon
   ipres=4                         !  pressure
   itime=6                         !  relative time
   iqc=7                           !  qc flag
   iuse=9                          !  original data usage flag
   imuse=10                        !  data usage flag in the analsis
   iweight=11                      !  variational relative weight
   ierr=12                         !  original error from bufr error table
   ierr2=13                        !  error from read_bufr
   ierr3=14                        !  error from final adjusted
   iobu=15                         !  u obs
   iogu=16                         !  u obs-ges
   iobv=18                         !  v obs
   iogv=19                         !  v obs-ges


   call rm_dups( rdiag,nobs,nreal,ilat,ilon,ipres,itime,iweight,ndup )

   do  i=1,nobs
      if(rdiag(iweight,i) >0.0) then
         ncount(3)=ncount(3)+1
         rpressu(3,ncount(3))=rdiag(iogu,i)
         rpressv(3,ncount(3))=rdiag(iogv,i)
         rpress(3,ncount(3))=sqrt(rdiag(iogu,i)**2+rdiag(iogv,i)**2)
      endif
   enddo

   deallocate(rdiag)


!   calculate the the areacd usr2
    
   rgtross=-gtross
   nlev=nint((gtross-rgtross)/rlev)
   print *,nlev
   print *,gtross, rgtross

   do i=1,3
      if(ncount(i) ==0) ncount(i)=1
   enddo

   print *, 'fileu, filev = ', fileu, filev

   call hist(dtype,rpress,3,3000000,ncount,rgtross,gtross,rlev,fileo,ncount_vqc,ncount_gros, grads_info_file )
   call histuv(dtype,rpressu,3,3000000,ncount,rgtross,gtross,rlev,fileu,ncount_vqc,ncount_gros,fileou2, 'u' )
   call histuv(dtype,rpressv,3,3000000,ncount,rgtross,gtross,rlev,filev,ncount_vqc,ncount_gros,fileov2, 'v' )

   return 
end
