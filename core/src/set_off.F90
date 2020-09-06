! **********************************************************
! *                                                        *
! * subroutine : set_off                                   *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * remove allocatable variables                           *
! *                              2006/07/28 kobayashi      *
! *                              2010/01/26 luu            *
! *                                                        *
! * <<call>>                                               *
! * called by main                                         *
! *                                                        *
! **********************************************************



      SUBROUTINE set_off

      use commons

      deallocate ( u )
      deallocate ( v )
      deallocate ( w  )
      deallocate ( un )
      deallocate ( vn )
      deallocate ( wn )
      deallocate ( vd )
      deallocate ( aam )
      deallocate ( xmesh )
      deallocate ( ymesh )
      deallocate ( xgrid )
      deallocate ( ygrid )
      deallocate ( z )
      deallocate ( dz )
      deallocate ( alon )
      deallocate ( alat )
      deallocate ( dx )
      deallocate ( dy )
      deallocate ( h )
      deallocate ( d )
      deallocate ( level )
      deallocate ( cor )
      deallocate ( fsm )
      deallocate ( fm )
      deallocate ( fn )

      deallocate ( xp )
      deallocate ( yp )
      deallocate ( zp )
      deallocate ( xo )
      deallocate ( yo )
      deallocate ( zo )
      deallocate ( up )
      deallocate ( vp )
      deallocate ( wp )
      deallocate ( ix )
      deallocate ( iy )
      deallocate ( iz )
      deallocate ( ox )
      deallocate ( oy )
      deallocate ( oz )
      deallocate ( rand )
      deallocate ( wsx )
      deallocate ( wsy )
      deallocate ( wsz )
      deallocate ( spz )
      deallocate ( value )
      deallocate ( color )
      deallocate ( id )
      deallocate ( xd )
      deallocate ( yd )
      deallocate ( zd )

      return
      END





