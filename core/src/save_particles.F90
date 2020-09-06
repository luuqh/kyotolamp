! **********************************************************
! *                                                        *
! * subroutine : save_particles                            *
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



      SUBROUTINE save_particles (r,n)

      use commons
      implicit none
      integer :: p, r, n, u_p, u_g
      real*8:: xt,yt,zt,valuet
      integer*4:: it,jt,kt,colort
      integer*8:: idt,count
      character*256:: f_p, f_g
      character*8:: f_num8
      character*2:: f_num2
      character*4:: f_num4
      real*8:: xp_earth, yp_earth !lon,lat
      real*8:: f !scaling factor

      write(*,*)'load subroutine [save_particles]'
      write(*,*)'-----------------------------------'

      if (p_save.eq.0) then
         f_p = trim(d_output)//trim(p_filename)//'.p'
         f_g = trim(d_output)//trim(p_filename)//'.g'
      else
         call lib_time (tstep*n,f_num8)
         f_p = trim(d_output)//f_num8//'.p' ! information file for next step
         f_g = trim(d_output)//f_num8//'.g' ! information file for graphic making
      endif

      if(mode_region.eq.1)then
         if(mode_extension.eq.2)then
            call lib_i2s (r,2,f_num2)
            f_p = trim(f_p)//trim(f_num2)
            f_g = trim(f_g)//trim(f_num2)
         else
            call lib_i2s (r,4,f_num4)
            f_p = trim(f_p)//trim(f_num4)
            f_g = trim(f_g)//trim(f_num4)
         endif
      endif

      u_p = 120
      u_g = 121
      open (unit=u_p,file=trim(f_p),form='unformatted') ! read input files
      open (unit=u_g,file=trim(f_g),form='unformatted') ! read input files
      write (u_p) pm
      write (u_g) pm
      write(*,*)trim(f_p),pm
      write(*,*)trim(f_g),pm
  
      do p = 1,pm
	   xt = xp(p)
	   yt = yp(p)
	   zt = zp(p)
	   it = ix(p)
	   jt = iy(p)
	   kt = iz(p)
	   idt = id(p)
	   colort = color(p)
	   valuet = value(p)
          write (u_p) xt,yt,zt
          write (u_p) it,jt,kt
          write (u_p) idt,colort
          write (u_p) valuet

          call gmerci ( xt, yt, xp_earth, yp_earth,
     &                  f, radius, slon, slat, rcnv)
          write (u_g) xp_earth,yp_earth,zt
          write (u_g) it,jt,kt
          write (u_g) idt,colort
          write (u_g) valuet
      enddo

      write(*,*)'max & min colors:',maxval(color),minval(color)

      rewind u_p
      rewind u_g
      close (u_p)
      close (u_g)

      count = 0
      do p = 1,pm
         if(color(p).ne.0.)count = count + 1
         ! write(*,*)color(p)
      enddo
      write(*,*)'nonzero color:',count,pm


      return

      END





