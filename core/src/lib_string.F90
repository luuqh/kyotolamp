! **********************************************************
! *                                                        *
! * subroutine : lib_is2                                   *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * convert integer to string                              *
! *                              2010/01/26 luu            *
! *                                                        *
! * <<call>>                                               *
! * called by save_particles                               *
! *                                                        *
! **********************************************************



      SUBROUTINE lib_i2s (number,maxorder,string)
      implicit none
      integer*4,intent(in) :: number,maxorder
      character(len=maxorder),intent(out) :: string
      character(len=maxorder) :: temp
      character(1) :: numchar
      integer*4:: i,n,k
      n = number
      k = 0
      do while (n.gt.0)
         i = mod(n,10)
         n = int(n/10)
         numchar = char(i+48)
         temp = numchar//temp
         k = k + 1
      enddo
      numchar = '0'
      if(k.lt.maxorder)then
         do i = 1,maxorder-k
            temp = numchar//temp
         enddo
      endif
      string = temp
      END


      SUBROUTINE lib_time (time,ddhhmmss)
      implicit none
      real*8,intent(in) :: time
      character(len=8),intent(out) :: ddhhmmss
      character(len=2) :: dd,hh,mm,ss
      character(len=4) :: d4
      integer*4:: t,d,h,m,s
      t = int(time)
      d = t/60/60/24
      h = t/60/60-d*24
      m = t/60-d*24*60-h*60
      s = t-d*24*60*60-h*60*60-m*60
      call lib_i2s (d,2,dd)
      call lib_i2s (d,4,d4)
      call lib_i2s (h,2,hh)
      call lib_i2s (m,2,mm)
      call lib_i2s (s,2,ss)
      !! ddhhmmss = dd//hh//mm//ss
      ddhhmmss = d4//hh//mm
      write(*,*)'ddhhmmss:',ddhhmmss
      END





