! **********************************************************
! *                                                        *
! *                  k y o t o - l a m p                   *
! *        lagrangian arbitary movement of particle        *
! * ______________________________________________________ *
! *                                                        *
! * <<mission>>                                            *
! * calculation of particle tracking in lagrangian manner  *
! *                              2010/01/26 luu            *
! *                                                        *
! **********************************************************


      use commons
      implicit none
      integer*8 :: i, j, k, l, m, n, p, q, s

      write(*,*) 'START @ KYOTO LAMP '
      write(*,*) '************************************************'

      call set_param

      call load_grids

      call load_namelist

         ! initization
         n = 1
         p_save = 1
         call load_particles (rg,n)
         call save_particles (rg,n-1)
         p_save = 0
         call save_particles (rg,n)

         write(*,*)'pm:',pm

         call p_boundary

         write(*,*)'pm:',pm

         call load_flow(n)

         write(*,*)'im,jm,kb,nm:',im,jm,kb,nm
 
         do n = 1, nm

            write(*,*)'*********************************** step:', n
            write(*,*)'pm:',pm

            call load_flow(n)
         
            m = sm + 1
            do i = 1, sm

              ! incremental linear interpolation of velocity
              s = (n-1)*sm + i ! continuous index for particles
              m = m - 1        ! sub-step index
              u = u + (un-u)/m
              v = v + (vn-v)/m
              w = w + (wn-w)/m

              write(*,*)'******* sub-step ', sm-m+1,' of step ',n
   
              call load_particles(rg,s)
              write(*,*)'pm:',pm

              call p_packing
              ! write(*,*)'pm:',pm

              call set_reallocate
              write(*,*)'pm:',pm

              call p_packing
              ! write(*,*)'pm:',pm

              call p_transport(n,i)
              ! call p_packing

              call p_boundary

              if (dmod(s*tstep,tsave).lt.epsilon)then
                 p_save = 1
              else
                 p_save = 0
              endif
              write(*,*)s,s*tstep,tsave,p_save

              call save_particles (rg,s) 

            enddo

         enddo

         f_status = 2 ! close flow file

         call load_flow(n)

      ! call set_off

      write(*,*) 'FINISH @ KYOTO LAMP '
      write(*,*) '************************************************'

      stop

      END



