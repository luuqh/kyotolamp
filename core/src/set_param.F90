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




      SUBROUTINE set_param

      use commons
      implicit none

      write(*,*)'load subroutine [set_param]'
      write(*,*)'-----------------------------------'

      d_xp = 'lab/'
      write(*,*)trim(d_root),': input directory for setting'

      d_root = '/LARGEHS02/gh10056/luu/fab/'
      write(*,*)trim(d_root),': input directory for setting'

      d_program = trim(d_root)//'kl/lamp/'//trim(d_xp)
      write(*,*)trim(d_program),': input directory for setting'

      d_input0 = trim(d_program)//'data/'
      write(*,*)trim(d_input0),': input directory for data'

      d_input1 = trim(d_root)//'kl/set/twc/20100321/'
      write(*,*)trim(d_input1),': input directory for 2D steady flow'

      d_input2 = trim(d_root)//'e111205/' !'e111114/'
      write(*,*)trim(d_input2),': input directory for 2D tidal flow'

      d_input3 = trim(d_program)//'data/'
      write(*,*)trim(d_input3),': input directory for 3D flow'

      d_output = trim(d_program)//'data/'
      write(*,*)trim(d_output),': output directory'

      p_filename = 'tempo'
      write(*,*)trim(p_filename),': temporary file for particles'

      radius = 6375.d3
      write(*,*)radius,': earth radius [m]'

      pi = 3.141592653589793d0
      write(*,*)pi,': pi constant'

      epsilon = 0.1d0
      write(*,*)epsilon,': epsilon for mod function'

      rcnv = pi/180.d0
      write(*,*)rcnv,': deg->rad converter factor'
     
      mode_bc = 1
      write(*,*)mode_bc,': boundary (1-awaji, 2-stayland, 3-previous)'

      mode_advect = 1
      write(*,*)mode_advect,': advection (0-no, 1-yes)' 

      mode_drift = 0
      write(*,*)mode_drift,': stokes drift (0-euler, 1-lagrange=e+s)'

      mode_turbulence = 0
      write(*,*)mode_turbulence,': turbulence (0-constant, 1-from file)'

      mode_random = 1
      write(*,*)mode_random,': type of random number (1 or 2)'

      mode_randwalk = 0
      write(*,*)mode_randwalk,': random walk mode (0-no, 1-yes)'

      mode_release = 2
      write(*,*)mode_release,': release (0-no, 1-initl, 2-continuous)'
      
      mode_region = 1
      write(*,*)mode_region,': sub-regions (0: no *.p, 1: yes *.p*)'

      mode_dimension = 2
      write(*,*)mode_dimension,': dimension (1: default; 2: 2d; 3: 3d)'

      mode_dimtype = 2 ! plz check consitency of input files at [d_input1]
      write(*,*)mode_dimtype,': dimtype (2: 2d array; 3: 3d array)'

      mode_extension = 2
      write(*,*)mode_extension,': file extension (2: *.p**, 4: *.p****)'

      mode_couple = 2
      write(*,*)mode_couple,': couple 2D (0: tide, 1: flow, 2: both)'

      mode_time = 1
      write(*,*)mode_time,': time direction (1:forward, -1:backward)'

      phase_step = 0
      write(*,*)phase_step,': skip step ass. phase (0:none, >1:all)'

      period_step = 4*2*(24*13+16) !24 !diurnal tide K1
      write(*,*)period_step,': steps within 1 tidal cycle (1 period)'

      main_step = 24*150 ! 10 tidal cycle
      write(*,*)main_step,': program running time (0:tide, >1:this)'

      sm = 4
      write(*,*)sm,': interpolated velocity steps (1-no, >2-yes)'

      tflow = 3.6d3 !3.6d3   ! 60 mins
      write(*,*)tflow,': data velocity time step p [s]'

      tstep = tflow/sm
      write(*,*)tstep,': interpolated velocity time step [s]'

      tsave = 3.6d3*3 !3.6d3    ! 60 mins
      write(*,*)tsave,': time step for saving particles [s]'

      rate = 0.1d0
      write(*,*)rate,': dispersion rate'

      aam_const = 5 !10.d0  ! kobayashi 2006
      write(*,*)aam_const,': horizontal diffusion (mode_turbulence=0)'

      vd_const = 5.d-3  ! kobayashi 2006
      write(*,*)vd_const,': vertical diffusion (mode_turbulence=0)'

      cdrift = 0.1d0
      write(*,*)cdrift,': ratio (dxs=dx*cdrift) in cal.(dxs) stokes'

      p_density = 4*4 !16*16 !4*4 !8*8
      write(*,*)p_density,' : particles in cell (ds in set_particles)'

      ! file index for i/o
      u_flow1 = 14
      v_flow1 = 15
      u_flow2 = 16
      v_flow2 = 17
      uv_flow3 = 18  
	  
      ! default sizes
      im = 0
      jm = 0
      kb = 0
      nm = 0
      pm = 0
      am = 0

      ! status of flow file
      f_status = 0

      END





