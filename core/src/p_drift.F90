! **********************************************************
! *                                                        *
! * subroutine : p_drift                                   *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! *  + calculate Stokes' drift velocity                    *
! *                              2010/01/26 luu            *
! *                                                        *
! * <<call>>                                               *
! * called by p_transport                                  *
! *                                                        *
! **********************************************************




      SUBROUTINE p_drift (xe, ye, ze, dxt, dyt, usk, vsk, wsk)

      use commons
      implicit none
      integer:: ie, je, ke
      real*8:: xe, ye, ze, usk, vsk, wsk, spzs
      real*8:: ui1, ui2, uj1, uj2, vi1, vi2, vj1, vj2
      real*8:: xi1, xi2, xj1, xj2, yi1, yi2, yj1, yj2
      real*8:: dxs, dys, dxt, dyt

      ! write(*,*)'load subroutine [p_drift]'
      ! write(*,*)'-----------------------------------'

      call p_interpol ( xe, ye, ze, ie, je, ke, usk, vsk, wsk, spzs)

      if((ie.le.1).or.(ie.ge.im).or.(je.le.1).or.(je.ge.jm).or.
     &   (ke.le.1).or.(ke.ge.kb))then

        ! write(*,*)'ie,je,ke:',ie,je,ke
        usk = 0.
        vsk = 0.
        wsk = 0.

      else
 
        dxs = dx(ie,je)*cdrift
        dys = dy(ie,je)*cdrift
        xi1 = xe - dxs/2.
        xi2 = xe + dxs/2.
        xj1 = xe
        xj2 = xe
        yi1 = ye
        yi2 = ye
        yj1 = ye - dys/2.
        yj2 = ye + dys/2.

        call p_interpol ( xi1, yi1, ze, ie, je, ke, ui1, vi1, wsk, spzs)
        call p_interpol ( xi2, yi2, ze, ie, je, ke, ui2, vi2, wsk, spzs)
        call p_interpol ( xj1, yj1, ze, ie, je, ke, uj1, vj1, wsk, spzs)
        call p_interpol ( xj2, yj2, ze, ie, je, ke, uj2, vj2, wsk, spzs)

        usk = (ui2-ui1)*dxt/dxs + (uj2-uj1)*dyt/dys
        vsk = (vi2-vi1)*dxt/dxs + (vj2-vj1)*dyt/dys
        wsk = 0.

      endif

      return

      END





