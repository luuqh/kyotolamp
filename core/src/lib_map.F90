! **********************************************************
! *                                                        *
! * subroutine : lib_index.f90                             *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * mapping library                                        *
! *                              1988/12/20 ishikawa       *
! *                              2006/07/28 kobayashi      *
! *                                                        *
! **********************************************************


   


      SUBROUTINE glccpi (x,y,q,p,f,r,q0,p0,rcnv)

!***********************************************************************
!
! inverse transformation of lambert conformal conical projection
!
! (input)
!    x      ; x-coordinate in the projected plane
!    y      ; y-coordinate in the projected plane
!    r      ; earth radius
!    p0     ; latitude of the origine of the projected map
!    q0     ; longuitude of the origine of the projected map
!
! (output)
!    p      ; latitude of (x,y)
!    q      ; longuitude of (x,y)
!    f      ; scaling factor at (x,y)
!
! **********************************************************************

      implicit real*8(a-h,o-z)

      if( p0 .le. 0.0 ) then
         sn = -1.0
      else
         sn =  1.0
      endif
      c1     = 0.73289
      c2     = 1.66486
      rcnv   = 3.14159 / 180.
      qusi0  = rcnv * (90.- sn * p0) * 0.5
      fact   = c1 * c2 * tan( qusi0 )**c1 / r /sin(qusi0*2.)
      c2     = c2 / fact
      rou0   = c2 * (tan(qusi0))**c1
      elm    = (sqrt( x**2 + ( sn*y - rou0)**2)/c2) ** (1./c1)
      qusi   = 2. * atan( elm )
      p      = (90. - qusi/rcnv) * sn
      rou    = c2 * ( tan(qusi*0.5) ) ** c1
      temp   = asin( x/rou )
      q      = temp / c1 / rcnv + q0
      f      = c1 * c2 * (tan(qusi*0.5))**c1 / sin(qusi) / r
      return

      END





      SUBROUTINE gmerci (x,y,q,p,f,r0,q0,p0,rcnv)

!***********************************************************************
!*    system : speedi  spwv1     program : sdp510(dpac)                *
!*---------------------------------------------------------------------*
!                                                                      *
!     gmerci : inverse transformation of lambert conformal             *
!    (sd5179)  conical projection                                      *
!                                                                      *
!     v01l02 : 88/12/20     by h.ishikawa                              *
!                                                                      *
!***********************************************************************
!                                                                      *
! (input)                                                              *
!    x      ; x-coordinate in the projected plane                      *
!    y      ; y-coordinate in the projected plane                      *
!    r0     ; earth radius ( dummy )                                   *
!    p0     ; latitude of the origine of the projected map             *
!    q0     ; longuitude of the origine of the projected map           *
!                                                                      *
! (output)                                                             *
!    p      ; latitude of (x,y)                                        *
!    q      ; longuitude of (x,y)                                      *
!    f      ; scaling factor at (x,y)                                  *
!                                                                      *
!***********************************************************************

      implicit real*8(a-h,o-z)

      f0 = ( 45. + p0 * 0.5 ) *rcnv
      ff0 = tan( f0 )
      y0  = r0 * cos( p0*rcnv ) * log( ff0 )
      q = x / r0 / cos( p0*rcnv ) / rcnv + q0
      f1= (y+y0) / r0 / cos( p0*rcnv )
      f2= exp( f1 )
      p = 2.0 * atan( f2 )/rcnv - 90.
      f = cos( p0*rcnv) / cos( p*rcnv )
      return

      END





      SUBROUTINE gpolri (x,y,ramda,phai,f,r0,oramda,ophai,rcnv)

!***********************************************************************
!*    system : speedi  spwv1     program : sdp510(dpac)                *
!*---------------------------------------------------------------------*
!                                                                      *
!     gpolri : inverse transformation of polar stereo projection       *
!    (sd5176)                                                          *
!                                                                      *
!     v01l02 : 88/12/20     by h.ishikawa                              *
!                                                                      *
!***********************************************************************
!                                                                      *
!    ( input )                                                         *
!    x             ; x- coordinate of projected point                  *
!    y             ; y- coordinate of projected point                  *
!    r0            ; earth radius (dummy)                              *
!    ophai         ; latitude of the origine of projection ( 0-90 )    *
!    oramda        ; longuitude of the origine of projection ( 0-360)  *
!    rcnv          ; pai/180( rad/deg ) ( dummy )                      *
!                                                                      *
!    ( output )                                                        *
!    phai          ; latitude of (x,y)                                 *
!    ramda         ; longuitude of ramda                               *
!    f             ; scaling factor                                    *
!                                                                      *
!***********************************************************************
      implicit real*8 ( a-h, o-z )

      if( ophai .le. 0.0 ) then ! if the origine is in the northern hemisphere,
        sn = -1.0
      else
        sn =  1.0 ! then sn is plus
      endif
      r  = 6371d0
      rcnv  = 3.141592 / 180.
      rphai = ophai
      c0 = r * ( 1d0 + sn * dsin( rphai*rcnv ) )
      rlatp = sn * 90d0  ! rlatp is the y coordinate of the north or south pole
      y0 = gpolry( 1d0,rlatp,r,oramda,ophai,rcnv)
      yy   = y-y0
      temp = ( x**2 + yy**2 ) / c0**2
      temp = dsqrt( temp )
      qusi2 =datan( temp )
      phai  = (90 -qusi2*2.0/rcnv) * sn
      if( yy .eq. 0.) then
        dram = 90d0
      else
        dram = asin( x/c0/temp )/rcnv
      endif
      if( sn .ge. 0.0 ) then
      if( yy .gt. 0d0) dram = 180d0 -dram
      else
      if( yy .lt. 0d0) dram = 180d0 -dram
      endif
      ramda = oramda + dram
      if( ramda .gt. 360d0 ) ramda=ramda-360d0
      if( ramda .lt. 0d0 ) ramda=ramda+360d0
      f = 0.5 * ( 1. + sn * dsin( rcnv * rphai ) ) / dcos(qusi2)**2
      if( ramda.gt.oramda+180.d0 )  ramda = ramda - 360.d0
      return

      END


      real*8 FUNCTION glccpx (sramda,sphai,rearth,oramda,ophai,rcnv)

!***********************************************************************
!*    system : speedi  spwv1     program : sdp510(dpac)                *
!*---------------------------------------------------------------------*
!     glccpx  : calculate x-coordinate by                              *
!    (sd5171)   lambert conformal conical projection.                  *
!               standard latitudes are 20n and 70n                     *
!               scaling factor is 1.0 at the latitude of origine       *
!     v01l02  : 88/12/20     by h.ishikawa                             *
!***********************************************************************
!   --- in ---                                                         *
!    sramda(r*8) : longitude of point  (deg.)                          *
!    sphai (r*8) : latitude  of point  (deg.)                          *
!    rearth(r*8) : radius of earth (km)                                *
!    oramda(r*8) : longitude of origin (deg.)                          *
!    ophai (r*8) : latitude  of origin (deg.)                          *
!    rcnv  (r*8) : conversion facter.(pai/180.)                        *
!***********************************************************************

      implicit real*8 ( a-h, o-z )

      if( ophai .le. 0.0 ) then
         sn = -1.0
      else
         sn =  1.0
      endif
      c1     = 0.732890014930969405
      c2     = 1.66486457136026234 * rearth
      qusi0  = rcnv * (90- sn * ophai) * 0.5
      fact   = c1 * c2 *  dtan(qusi0)**c1 / rearth / dsin(qusi0*2)
      c2     = c2 / fact
      dram   = sramda - oramda
      if( dram .gt. 180. ) dram = (dram-360.)
      if( dram .lt.-180. ) dram = (dram+360.)
      glccpx = c2 * ( dtan(rcnv*(90- sn * sphai)*0.5) ) 
     &         **c1 * dsin( rcnv * c1 * dram  )
      return

      END





      real*8 FUNCTION glccpy (sramda,sphai,rearth,oramda,ophai,rcnv)

!***********************************************************************
!*    system : speedi  spwv1     program : sdp510(dpac)                *
!*---------------------------------------------------------------------*
!     glccpy  : calculate y-coordinate                                 *
!    (sd5172)   by lambert coformal conical projection                 *
!               standard latitudes are 20n and 70n                     *
!               scaling factor is 1.0 at the latitude of origine       *
!     v01l02  : 88/12/20     by h.ishikawa                             *
!***********************************************************************
!   --- in ---                                                         *
!    sramda(r*8) : longitude of point  (deg.)                          *
!    sphai (r*8) : latitude  of point  (deg.)                          *
!    rearth(r*8) : radius of earth (km)                                *
!    oramda(r*8) : longitude of origin (deg.)                          *
!    ophai (r*8) : latitude  of origin (deg.)                          *
!    rcnv  (r*8) : conversion facter.(pai/180.)                        *
!                                                                      *
!***********************************************************************

      implicit real*8 ( a-h, o-z )

      if( ophai .le. 0.0 ) then
         sn = -1.0
      else
         sn =  1.0
      endif
      c1     = 0.732890014930969405
      c2     = 1.66486457136026234  * rearth
      qusi0  = rcnv * (90- sn * ophai) * 0.5
      fact   = c1 * c2 * dtan(qusi0)**c1 / rearth / dsin(qusi0*2)
      c2     = c2 / fact
      rou0   = c2 * tan(qusi0)**c1
      dram   = sramda - oramda
      if( dram .gt. 180. ) dram =  dram - 360.
      if( dram .lt.-180. ) dram =  dram + 360.
      glccpy = (rou0 - c2 *  (dtan(rcnv*(90- sn * sphai)*0.5))
     &         **c1 *  dcos(rcnv*c1*dram) ) * sn
      return

      END





      real*8 FUNCTION gmercx (sramda,sphai,rearth,oramda,ophai,rcnv )

!***********************************************************************
!*    system : speedi  spwv1     program : sdp510(dpac)                *
!*---------------------------------------------------------------------*
!     gmercx  : calculate x-coordinate by mercator projection.         *
!    (sd5177)     mercator projection.                                 *
!     v01l02  : 88/12/20     by h.ishikawa                             *
!***********************************************************************
!   --- in ---                                                         *
!    sramda(r*8) : longitude of point  (deg.)                          *
!    sphai (r*8) : latitude  of point  (deg.)                          *
!    rearth(r*8) : radius of earth (km)                                *
!    oramda(r*8) : longitude of origin (deg.)                          *
!    ophai (r*8) : latitude  of origin (deg.)                          *
!    rcnv  (r*8) : conversion facter.(pai/180.)                        *
!                                                                      *
!***********************************************************************

      implicit real*8 ( a-h, o-z )

      dram   = sramda - oramda
      if( dram .gt. 180. ) dram = dram - 360.
      if( dram .lt.-180. ) dram = dram + 360.
      gmercx = rearth * cos( ophai*rcnv ) * ( dram ) * rcnv
      return

      END






      real*8 FUNCTION gmercy(sramda,sphai,rearth,oramda,ophai,rcnv)

!***********************************************************************
!*    system : speedi  spwv1     program : sdp510(dpac)                *
!*---------------------------------------------------------------------*
!     gmercy  : calculate y-coordinate by mercator projection          *
!    (sd5178)   scaling factor is 1.0 at the latitude of origine       *
!     v01l02  : 88/12/20     by h.ishikawa                             *
!***********************************************************************
!   --- in ---                                                         *
!    sramda(r*8) : longitude of point  (deg.)                          *
!    sphai (r*8) : latitude  of point  (deg.)                          *
!    rearth(r*8) : radius of earth (km)                                *
!    oramda(r*8) : longitude of origin (deg.)                          *
!    ophai (r*8) : latitude  of origin (deg.)                          *
!    rcnv  (r*8) : conversion facter.(pai/180.)                        *
!***********************************************************************

      implicit real*8 ( a-h, o-z )

      f1     = ( 45. + sphai*0.5 ) * rcnv
      f0     = ( 45. + ophai*0.5 ) * rcnv
      ff     = tan( f1 )
      ff0    = tan( f0 )
      y0     = rearth * cos( ophai*rcnv ) * log( ff0)
      gmercy = rearth * cos( ophai*rcnv ) * log( ff ) - y0
      return

      END







      real*8 FUNCTION gpolrx (sramda,sphai,rearth,oramda,ophai,rcnv )

!***********************************************************************
!*    system : speedi  spwv1     program : sdp510(dpac)                *
!*---------------------------------------------------------------------*
!     gpolrx  : calculate x-coordinate of point from longitude and     *
!    (sd5174)   latitude of origin and point.                          *
!               by polar(stereo) projection.                           *
!               reference latitude is that of origine, so that the     *
!               scaling factor is 1.0 at that latitude.                *
!     v01l02  : 88/12/20     by h.ishikawa                             *
!***********************************************************************
!   --- in ---                                                         *
!    sramda(r*8) : longitude of point  (deg.)                          *
!    sphai (r*8) : latitude  of point  (deg.)                          *
!    rearth(r*8) : radius of earth (km)                                *
!    oramda(r*8) : longitude of origin (deg.)                          *
!    ophai (r*8) : latitude  of origin (deg.) ; dummy                  *
!    rcnv  (r*8) : conversion facter.(pai/180.)                        *
!***********************************************************************

      implicit real*8 ( a-h, o-z )

      if( ophai .lt. 0.0 ) then
      sn = -1.0
      else
      sn =  1.0
      endif
      rphai  = ophai
      dram   = sramda - oramda
      if( dram .gt. 180.) dram = dram-360.
      if( dram .lt. -180.) dram = dram+360.
      gpolrx = rearth * ( 1.0d0 + sn * dsin(rcnv * rphai)) * 
     &         dtan(rcnv * (45.0d0 - sn * sphai /2.0d0)) * 
     &         dsin((sramda - oramda) * rcnv)
      return

      END





      real*8 FUNCTION gpolry ( sramda,sphai,rearth,oramda,ophai,rcnv )

!***********************************************************************
!*    system : speedi  spwv1     program : sdp510(dpac)                *
!*---------------------------------------------------------------------*
!     gpolry  : calculate y-coordinate of point from longitude and     *
!    (sd5175)   latitude of origin and point.                          *
!               by polar(stereo) projection.                           *
!               reference latitude is that of the origine, so that     *
!               the scaling factor is 1.0 at that latitude.            *
!     v01l02  : 88/12/20     by h.ishikawa                             *
!***********************************************************************
!   --- in ---                                                         *
!    sramda(r*8) : longitude of point  (deg.)                          *
!    sphai (r*8) : latitude  of point  (deg.)                          *
!    rearth(r*8) : radius of earth (km)                                *
!    oramda(r*8) : longitude of origin (deg.)                          *
!    ophai (r*8) : latitude  of origin (deg.)                          *
!    rcnv  (r*8) : conversion facter.(pai/180.)                        *
!***********************************************************************

      implicit real*8 ( a-h, o-z )

      if( ophai .le. 0.0 ) then
         sn = -1.0
      else
         sn =  1.0
      endif
      rphai  = ophai
      dram = sramda - oramda
      if( dram .gt. 180. ) dram = dram -360.
      if( dram .lt. -180.) dram = dram +360.
      gpolry = rearth * ( 1.0d0 + sn * dsin(rcnv * rphai)) * 
     &         ( sn * dtan(rcnv * (45.0d0 - sn * ophai/2.0d0)) -  
     &         sn * dtan(rcnv * (45.0d0 - sn * sphai/2.0d0)) *   
     &         dcos(rcnv * (dram))     )
      return

      END