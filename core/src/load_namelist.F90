! **********************************************************
! *                                                        *
! * subroutine : set_param.f90                             *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * set parameters for model                               *
! *                              2010/02/10 luu            *
! *                                                        *
! **********************************************************




      SUBROUTINE load_namelist

      use commons
      implicit none

      integer*4:: region
      namelist /domain/ region

      read(99,nml=domain)
      rg = region

      write(*,*)'++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      write(*,*)'region: ',rg

      return
      END





