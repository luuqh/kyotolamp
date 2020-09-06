

      ! Merge Particles from Lamp
      ! -------------------------------------------------------------------------
      ! Written by Luu Quang Hung, Kyoto, June 22th 2010
      ! -------------------------------------------------------------------------


      PROGRAM MERGE_PARTICLES

      ! declare variables
      implicit none
      character*256::f_pathin,f_pathout,f_in,f_out
      character*8:: f_prefix
      character*2:: f_suffix
      character*4:: f_suffix4
      character*1:: f_filefix
      integer*4:: nm,nr,n,k,ip,jp,kp,color
      integer*8:: pall,pm,p,id,mark
      real*8:: xp,yp,zp,value,time,dt
      
      f_pathin = '../../lamp/lab3/data/'
      f_pathout = '../../lamp/lab3/merge/'
      f_filefix = 'g'
      nr = 401 !30 !8 !5 !25 !25 !10 !25 !308 !25 !100    ! nunber of sub-regions
      nm = 24*30 !20 !15 !5 !24*1 !15+1 !24*3 +1!*30+1 !120 !30 !36+1 !25! 25+2   ! number of time steps
      dt = 3600.  ! time step [s]

      do n = 1,nm+1
      !do n = 24*15+1,24*20+1
      !do n = 20*24+22,20*24+24
         time = dt * (n-1)
         call timing (time,f_prefix)
         f_out = trim(f_pathout)//trim(f_prefix)//'.'
         f_out = trim(f_out)//trim(f_filefix)//'a'
         open(0,file=trim(f_out),form='unformatted')
         write(*,*)f_out
         pall = 0
         !! do k = 1,nr ! read only number of particles
         do k = 1,nr ! read only number of particles
            f_in = trim(f_pathin)//trim(f_prefix)//'.'
            ! call i2s (k,2,f_suffix)
            ! f_in = trim(f_in)//trim(f_filefix)//trim(f_suffix)
            call i2s (k,4,f_suffix4)
            f_in = trim(f_in)//trim(f_filefix)//trim(f_suffix4)
            write(*,*)trim(f_in)
            open(k,file=trim(f_in),form='unformatted')
            read(k)pm
            pall = pall + pm
            close(k)
         enddo
         write(0)pall
         write(*,*)pall
         !! do k = 1,nr ! read particle by particle
         do k = 1,nr
            f_in = trim(f_pathin)//trim(f_prefix)//'.'
            ! call i2s (k,2,f_suffix)
            ! f_in = trim(f_in)//trim(f_filefix)//trim(f_suffix)
            call i2s (k,4,f_suffix4)
            f_in = trim(f_in)//trim(f_filefix)//trim(f_suffix4)
            open(k,file=trim(f_in),form='unformatted')
            read(k)pm
            write(*,*)trim(f_in),pm
            do p = 1,pm
               read(k)xp,yp,zp
               read(k)ip,jp,kp
               if (f_filefix.eq.'e') then
                  read(k)id,mark
               else
                  read(k)id,color
               endif
               read(k)value
               write(0)xp,yp,zp
               write(0)ip,jp,kp
               if (f_filefix.eq.'e') then
                  write(0)id,mark
               else
                  write(0)id,color
               endif
               write(0)value
            enddo
            close(k)
            ! write(*,*)color
         enddo
         close(0)
         write(*,*)pall
      enddo
      write(*,*)'Finish!'
      END 




      SUBROUTINE i2s (number,maxorder,string)
      implicit none
      integer*4,intent(in) :: maxorder
      integer*4,intent(in) :: number
      character(len=maxorder),intent(out) :: string
      character(len=maxorder) :: temp
      character(1) :: numchar
      integer*8:: i,n,k
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
      return
      END



      SUBROUTINE timing (time,ddhhmmss)
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
      call i2s (d,2,dd)
      call i2s (d,4,d4)
      call i2s (h,2,hh)
      call i2s (m,2,mm)
      call i2s (s,2,ss)
      !! ddhhmmss = dd//hh//mm//ss
      ddhhmmss = d4//hh//mm
      write(*,*)'ddhhmmss:',ddhhmmss
      END




