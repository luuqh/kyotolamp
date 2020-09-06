! **********************************************************
! *                                                        *
! * module : commons.f90                                   *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * declare all common variables shared in the model       *
! *                              2010/01/26 luu            *
! *                                                        *
! **********************************************************




        MODULE commons

        ! sizes
        integer*4 :: im, jm, kb, nm    ! im=136, jm=131, kb=78
        integer*4 :: imm1, jmm1, imm2, jmm2
        integer*8 :: pm, am, sm, rg

        ! particles
        real*8,pointer:: xp(:), yp(:), zp(:)
        real*8,pointer:: xo(:), yo(:), zo(:)
        real*8,pointer:: xd(:), yd(:), zd(:)
        real*8,pointer:: up(:), vp(:), wp(:)
        real*8,pointer:: wsx(:), wsy(:), wsz(:), spz(:)
        real*8,pointer:: value(:)
        integer*4,pointer:: ix(:), iy(:), iz(:)
        integer*4,pointer:: ox(:), oy(:), oz(:)
        integer*4,pointer:: color(:)
        integer*8,pointer:: id(:)

        ! background current
        real*4,pointer:: u(:,:,:), v(:,:,:), w(:,:,:)
        real*4,pointer:: us(:,:),  vs(:,:), uf(:,:), vf(:,:)
        real*4,pointer:: un(:,:,:), vn(:,:,:), wn(:,:,:)
        real*4,pointer:: vd(:,:,:), aam(:,:,:)

        ! domain grid
        real*8,pointer:: xmesh(:), ymesh(:), xgrid(:,:), ygrid(:,:)
        real*8,pointer:: z(:,:,:), zz(:,:,:), dz(:,:,:), dzz(:,:,:)
        real*8,pointer:: alon(:,:), alat(:,:), dx(:,:), dy(:,:)
        real*8,pointer:: d(:,:), cor(:,:)
        real*8,pointer:: fsm(:,:,:), fm(:,:), fn(:,:)
        real*8,pointer:: delz(:), dep(:)
        real*4,pointer:: h(:,:)
        integer*4,pointer:: level(:,:)
        real*8:: slon, slat, dxdeg, dydeg
        real*8:: xmtmin, ymtmin, xmaxm, ymaxm

        ! random
        real*8:: iini
        real*4,pointer:: rand(:)

        ! parameters (see set_param.F90)
        character*256:: d_input0, d_input1, d_input2, d_input3
        character*256:: d_root, d_output, d_xp, d_program, p_filename
        integer*4:: mode_bc, mode_random, mode_randwalk
        integer*4:: mode_advect, mode_drift, mode_turbulence
        integer*4:: mode_release, mode_region, mode_dimension
        integer*4:: mode_extension, mode_couple, mode_dimtype
        integer*4:: mode_time
        integer*4:: u_flow1, v_flow1, u_flow2, v_flow2, uv_flow3
        integer*4:: f_status, p_density, p_save
        integer*4:: main_step, phase_step, period_step
        real*8:: tstep, tflow, tsave
        real*8:: radius, pi, rcnv, epsilon
        real*8:: aam_const, vd_const, rate, cdrift

        END



