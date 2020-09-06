! **********************************************************
! *                                                        *
! * subroutine : set_particles                             *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * set allocatable variables                              *
! *                              2006/07/28 kobayashi      *
! *                              2010/01/26 luu            *
! *                                                        *
! * <<call>>                                               *
! * called by main                                         *
! *                                                        *
! **********************************************************



      SUBROUTINE set_particles

      use commons

      allocate ( xp (pm) )
      allocate ( yp (pm) )
      allocate ( zp (pm) )
      allocate ( xo (pm) )
      allocate ( yo (pm) )
      allocate ( zo (pm) )
      allocate ( up (pm) )
      allocate ( vp (pm) )
      allocate ( wp (pm) )
      allocate ( ix (pm) )
      allocate ( iy (pm) )
      allocate ( iz (pm) )
      allocate ( ox (pm) )
      allocate ( oy (pm) )
      allocate ( oz (pm) )
      allocate ( rand (pm) )
      allocate ( wsx (pm) )
      allocate ( wsy (pm) )
      allocate ( wsz (pm) )
      allocate ( spz (pm) )
      allocate ( value (pm) )
      allocate ( color (pm) )
      allocate ( id (pm) )
      allocate ( xd (pm) )
      allocate ( yd (pm) )
      allocate ( zd (pm) )

      xp = 0
      yp = 0
      zp = 0
      xo = 0
      yo = 0
      zo = 0
      up = 0
      vp = 0
      wp = 0
      ix = 0
      iy = 0
      iz = 0
      ox = 0
      oy = 0
      oz = 0
      rand = 0
      wsx = 0
      wsy = 0
      wsz = 0
      spz = 0
      color = 0
      id = 0
      p_status = 1
      xd = 0
      yd = 0
      zd = 0
      value = 0.

      return
      END





