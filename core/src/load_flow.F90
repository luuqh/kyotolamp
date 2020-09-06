! **********************************************************
! *                                                        *
! * subroutine : load_flow                                 *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * read particles data from external file                 *
! *                              2010/01/26 luu            *
! *                                                        *
! * <<call>>                                               *
! * called by main                                         *
! *                                                        *
! **********************************************************



      SUBROUTINE load_flow(n)

      use commons
      implicit none
      character*256:: f_flow, f_flow2
      integer:: i,j,n,k

      write(*,*)'load subroutine [load_flow]'
      write(*,*)'-----------------------------------'

      ! write(*,*)'f_status:', f_status

      if (f_status.eq.1) then
        u(:,:,:) = un(:,:,:)
        v(:,:,:) = vn(:,:,:)
        w(:,:,:) = wn(:,:,:) 
        if (mode_dimension.ne.2) then !3D flow
          read (uv_flow3) un, vn, wn
          read (uv_flow3) vd, aam
          un(:,:,:) = un(:,:,:)
          vn(:,:,:) = vn(:,:,:)
          wn(:,:,:) = wn(:,:,:)
          if (mode_turbulence.eq.0) then
            aam = aam_const
            vd = vd_const
          else ! turn off (be careful)
            aam = 0.
            vd = 0.
          endif
        else
          if (mode_couple.ne.1) then
            if ((mod(n+phase_step,period_step)).eq.0) then ! repeat loading tide file
              write(*,*)'reloading tidal file at ',n
              rewind (u_flow2)
              rewind (v_flow2)
              do k = 1,4
                read (u_flow2)i
                read (v_flow2)j
              enddo			
            endif
            read (u_flow2) us
            read (v_flow2) vs
          else
            us = 0.
            vs = 0.
          endif
          do i = 1,im
            do j = 1,jm
              do k = 1,kb
                un(i,j,k) = mode_time*(us(i,j)+uf(i,j))/100.
                vn(i,j,k) = mode_time*(vs(i,j)+vf(i,j))/100.
                !! un(i,j,k) = mode_time*(us(i,j)*1.5+uf(i,j)*.8)/100.
                !! vn(i,j,k) = mode_time*(vs(i,j)*1.5+vf(i,j)*.8)/100.
                wn(i,j,k) = 0.
              enddo
            enddo
          enddo
          if (mode_turbulence.eq.0) then
            aam = aam_const
            vd = vd_const
          else ! turn off (be careful)
            aam = 0.
            vd = 0.
          endif
        endif		
      endif

      if (f_status.eq.0) then
        if (mode_dimension.ne.2) then !3D flow
          f_flow = trim(d_input3)//'flows.dat' ! model information file
          open (uv_flow3,file=trim(f_flow),form='unformatted') ! read input files
          write(*,*) f_flow
          read (uv_flow3) i, j, k, nm
          write(*,*) i, j, k, nm

        else ! 2D flow
          if (mode_couple.ge.1) then ! steady flow
            if (mode_dimtype.eq.3) then
              f_flow = trim(d_input1)//'u.uf' ! model information file
              f_flow2 = trim(d_input1)//'v.uf' ! model information file         
            else if (mode_dimtype.eq.2) then
              f_flow = trim(d_input1)//'sfu.uf' ! model information file
              f_flow2 = trim(d_input1)//'sfv.uf' ! model information file
            else
              write(*,*)'Error in loading steady flow [load_flow]'
            endif
            open (u_flow1,file=trim(f_flow),form='unformatted') ! read input files
            open (v_flow1,file=trim(f_flow2),form='unformatted') ! read input files
            write(*,*) f_flow
            write(*,*) f_flow2
            do k = 1,4
              read (u_flow1)i
              read (v_flow1)j
              write(*,*)i,j
            enddo
            if (mode_dimtype.eq.3) then
              read (u_flow1)u ! 3:00am
              read (v_flow1)v ! 3:00am
              uf(:,:) = u(:,:,1)
              vf(:,:) = v(:,:,1)
              u = 0. 
              v = 0. 
            else if (mode_dimtype.eq.2) then
              read (u_flow1)uf
              read (v_flow1)vf
            else
            endif
            close (u_flow1)
            close (v_flow1)
          endif

          if (mode_couple.ne.1) then ! tidal flow
            f_flow = trim(d_input2)//'sfu.uf' ! model information file
            f_flow2 = trim(d_input2)//'sfv.uf' ! model information file
            open (u_flow2,file=trim(f_flow),form='unformatted') ! read input files
            open (v_flow2,file=trim(f_flow2),form='unformatted') ! read input files
            write(*,*) f_flow
            write(*,*) f_flow2
            do k = 1,4
              read (u_flow2)i
              read (v_flow2)j
              write(*,*)i,j
            enddo			   
            do i = 1,phase_step
              read (u_flow2)us
              read (v_flow2)vs
            enddo
          endif
          if (main_step.le.0) then
            nm = i
          else
            nm = main_step
          endif
        endif
        f_status = 1
      endif

      if (f_status.eq.2) then
        if (mode_dimension.ne.2) then !3D flow
          close (uv_flow3)
        else
          close (u_flow1)
          close (v_flow1)
          close (u_flow2)
          close (v_flow2)
        endif
      endif
      return

      END





