! **********************************************************
! *                                                        *
! * subroutine : set_grids                                 *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * set allocatable variables                              *
! *                              2010/01/26 luu            *
! *                                                        *
! * <<call>>                                               *
! * called by main                                         *
! *                                                        *
! **********************************************************



      SUBROUTINE set_grids

      use commons
      implicit none
        
      imm1 = im - 1
      jmm1 = jm - 1
      imm2 = im - 2
      jmm2 = jm - 2
      allocate ( u     (im,jm,kb) )
      allocate ( v     (im,jm,kb) )
      allocate ( w     (im,jm,kb) )
      allocate ( un    (im,jm,kb) )
      allocate ( vn    (im,jm,kb) )
      allocate ( wn    (im,jm,kb) )
      allocate ( vd    (im,jm,kb) )
      allocate ( aam   (im,jm,kb) )
      allocate ( us    (im,jm) )
      allocate ( vs    (im,jm) )
      allocate ( uf    (im,jm) )
      allocate ( vf    (im,jm) )
      allocate ( xmesh (im) )
      allocate ( ymesh (jm) )
      allocate ( xgrid (im,jm) )
      allocate ( ygrid (im,jm) )
      allocate ( z     (im,jm,kb) )
      allocate ( zz    (im,jm,kb+1) )
      allocate ( dz    (im,jm,kb) )
      allocate ( dzz   (im,jm,kb+1) )
      allocate ( alon  (im,jm) )
      allocate ( alat  (im,jm) )
      allocate ( dx    (im,jm) )
      allocate ( dy    (im,jm) )
      allocate ( h     (im,jm) )
      allocate ( d     (im,jm) )
      allocate ( level (im,jm) )
      allocate ( cor   (im,jm) )
      allocate ( fsm   (im,jm,kb) )
      allocate ( fm    (im,jm) )
      allocate ( fn    (im,jm) )
      allocate ( delz  (kb) )
      allocate ( dep   (kb+1) )

      u = 0. 
      v = 0. 
      w = 0. 
      un = 0. 
      vn = 0. 
      wn = 0. 
      vd = 0. 
      aam = 0. 
      us = 0. 
      vs = 0. 
      uf = 0. 
      vf = 0. 
      xmesh = 0. 
      ymesh = 0. 
      xgrid = 0. 
      ygrid = 0. 
      z = 0. 
      zz = 0.
      dz  = 0.
      dzz  = 0.
      alon = 0.
      alat = 0.
      dx = 0.
      dy = 0.
      h  = 0.
      d  = 0.
      level = 0
      cor = 0.
      fsm = 0.
      fm  = 0.
      fn = 0.
      delz  = 0.
      dep = 0.

      return
      END





