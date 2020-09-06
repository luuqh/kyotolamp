! **********************************************************
! *                                                        *
! * subroutine : load_particles                            *
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



      SUBROUTINE load_particles (r,n)

      use commons
      implicit none
      real*8:: xt,yt,zt,valuet
      integer*4 :: it, jt, kt, colort
      integer*8 :: p, u_p, i, j, k, n, r, idt
      character*256:: f_p
      character*8:: f_num8
      character*2:: f_num2
      character*4:: f_num4

      write(*,*)'load subroutine [load_particles]'
      write(*,*)'-----------------------------------'

      if (p_save.eq.0) then
         f_p = trim(d_output)//trim(p_filename)//'.p'
      else
         call lib_time (tstep*(n-1),f_num8)
         f_p = trim(d_output)//f_num8//'.p' ! model information file
      endif

      if(mode_region.eq.1)then
         if(mode_extension.eq.2)then
            call lib_i2s (r,2,f_num2)
            f_p = trim(f_p)//trim(f_num2)
         else
            call lib_i2s (r,4,f_num4)
            f_p = trim(f_p)//trim(f_num4)
         endif
      endif

      u_p = 20
      open(unit=u_p,file=trim(f_p),form='unformatted') ! read input files
      write(*,*)trim(f_p)
      read(u_p)pm
      write(*,*)trim(f_p),pm

      call set_particles

      do p = 1,pm
	   read (u_p) xt,yt,zt
	   read (u_p) it,jt,kt
	   read (u_p) idt,colort
          read (u_p) valuet
	   xp(p) = xt
	   yp(p) = yt
	   zp(p) = zt
	   ix(p) = it
	   iy(p) = jt
	   iz(p) = kt
	   id(p) = idt
	   color(p) = colort
	   if ((n.eq.1).and.(mode_release.le.1)) then
	      valuet = dx(it,jt)*dy(it,jt)*h(it,jt)/p_density
	   endif
	   value(p) = valuet
      enddo
      ! write(*,*)'max & min colors:',maxval(color),minval(color)

      rewind u_p
      close (u_p)
      return

      END





