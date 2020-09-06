! **********************************************************
! *                                                        *
! * subroutine : lib_index.f90                             *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * convert from longitude & latitude to array index       *
! *                              2006/07/28 kobayashi      *
! *                                                        *
! **********************************************************


   

      SUBROUTINE lib_index ( nx, ny, nz, level, xe, ye, ze,
     &                      dxdeg, dydeg, slon, slat, radius, rcnv,
     &                      z, icpx, icpy, icpz)

      implicit none

      ! arguments
      integer(4), intent(in) :: nx              ! x grid size
      integer(4), intent(in) :: ny              ! y grid size
      integer(4), intent(in) :: nz              ! z grid size
      integer(4), intent(in) :: level(nx,ny)    ! level of sea-bottom layer
      real(8), intent(in) :: xe                 ! x position [m]
      real(8), intent(in) :: ye                 ! y position [m]
      real(8), intent(in) :: ze                 ! z position [m]
      real(8), intent(in) :: dxdeg              ! x grid interval [deg]
      real(8), intent(in) :: dydeg              ! y grid interval [deg]
      real(8), intent(in) :: slon               ! origin of longitude [deg]
      real(8), intent(in) :: slat               ! origin of latitude [deg]
      real(8), intent(in) :: radius             ! earth radius [m]
      real(8), intent(in) :: rcnv               ! deg->rad conversion factor
      real(8), intent(in) :: z(nx,ny,nz)        ! z grid position [m]
      integer(4), intent(out) :: icpx           ! x grid no.
      integer(4), intent(out) :: icpy           ! y grid no.
      integer(4), intent(out) :: icpz           ! z grid no.

      ! local variables
      integer(4) k
      real(8) lonp, latp
      real(8) gmercx, gmercy
      real(8) f

      call gmerci( xe, ye, lonp, latp, f, radius, slon, slat, rcnv )

      icpx = int( (lonp-slon) / dxdeg ) + 1
      icpy = int( (latp-slat) / dydeg ) + 1

      if( icpx < 1 .or. icpx > nx .or. icpy < 1 .or. icpy > ny )then
        icpz = 0
        return
      endif

      do k = 1, level(icpx,icpy)
        if( ze >= z(icpx,icpy,k) .and. ze < z(icpx,icpy,k+1) )then
          icpz = k
          goto 1000
        endif
      enddo
      if( level(icpx,icpy) > 0 )then
        if(ze <  z(icpx,icpy,1) ) icpz = 1
        if(ze >= z(icpx,icpy,level(icpx,icpy)))icpz=level(icpx,icpy)
      endif
 1000 continue

      END


