


      PROGRAM UNIFORM

      implicit none
      real*8:: x,y,z,value,time,dt
      integer*8,pointer:: list(:),nop(:)
      integer*8:: id,nm,rm,pm,pn,p,n,m,i,k,r,i0,im,check,search
      integer*4:: ix,iy,iz,color,mode
      character*80::f_pathin,f_pathout,f_in,f_out
      character*8:: f_prefix
      character*6:: f_suffix6
      character*2:: f_suffix2


      f_pathin = '../../lamp/lab3/merge/'
      f_pathout = '../../lamp/lab3/merge/'
      nm = 24*27 !120!mf 30 !36+1 !25!24+2! number of steps
      dt = 3600.! time step [s]
      mode = 2  ! 1-calculate 'eg' file only; 2-calculate all
      if (mode.eq.2) then
        rm = 1
      else
        rm = 5
      endif
      allocate ( nop (rm) )

      do r = 1,rm
         if (mode.eq.2) then
            f_suffix6 = 'ga'
         else           
            call i2s(r,2,f_suffix2)
            f_suffix6 = 'g'//trim(f_suffix2)
         endif

         ! read id of final particles
         time = nm*dt
         call timing (time,f_prefix)
         f_in = trim(f_pathin)//trim(f_prefix)//'.'//trim(f_suffix6)
         write(*,*)trim(f_in)
         open (unit=r,file=trim(f_in),form='unformatted')
         read (r) pm
         write(*,*) pm
         nop(r) = pm
      enddo
      pm = 0
      do r = 1,rm
         if (pm.lt.nop(r)) pm = nop(r)
      enddo
      allocate ( list (pm) )
      do r = 1,rm
         do p = 1,pm
            list(p) = 0
         enddo
         do p = 1,nop(r) 
            read (r) x,y,z
            read (r) ix,iy,iz
            read (r) id,color
            read (r) value
            list(p) = id
         enddo
         close(r)
         m = 0
         ! do p = 1,pm  !! progress
            ! if((p-int(p/30000)*30000).eq.0) then
            !     ! write(*,*)int(p*100/pm),'%'
            ! endif
            ! do k = p+1,pm  !! check duplication
            !   if(list(k).eq.list(p))then
            !      m = m + 1
            !      write(*,*)list(p),' is duplicated at ',k,'and',p,':',m
            !    endif
            ! enddo
         ! enddo
         if (mode.eq.2) then
            f_suffix6 = 'a'
         else           
            call i2s(r,2,f_suffix2)
            f_suffix6 = trim(f_suffix2)
         endif


         !do n = 21*24+10,nm+1
         do n = 1,nm+1
            time = (n-1)*dt
            call timing (time,f_prefix)
            f_in = trim(f_pathin)//trim(f_prefix)//'.g'//trim(f_suffix6)
            f_out = trim(f_pathout)//trim(f_prefix)//'.e'//trim(f_suffix6)
            m = nm+n+1
            open (unit=n,file=trim(f_in),form='unformatted')
            open (unit=m,file=trim(f_out),form='unformatted')
            pn = nop(r)
            write(m)pn
            read(n)pm
            write(*,*)n,trim(f_in),pm
            write(*,*)m,trim(f_out),pn
            check = 0
            k = 1
            do p = 1,pm
               read (n) x,y,z
               read (n) ix,iy,iz
               read (n) id,color
               read (n) value
               if((p-int(p/1000)*1000).eq.0) then
                  ! write(*,*)int(p*100/pm),'%'
               endif
               if(k.le.pn) then
                  if (list(k).eq.id) then !this algorithm is valid only if id is unique and sorted
                     k = k + 1
                     check = check + 1
                     write (m) x,y,z
                     write (m) ix,iy,iz                    
                     write (m) id,color
                     write (m) value
                  endif
               endif         
            enddo
            write(*,*)'compare(sorted,output,input):',check,pn,pm
            close (n)
            close (m)
         enddo
         write(*,*)'mission completed'
      enddo
      deallocate ( list, nop )
      END



      SUBROUTINE i2s (number,maxorder,string)
      implicit none
      integer,intent(in) :: number,maxorder
      character(len=maxorder),intent(out) :: string
      character(len=maxorder) :: temp
      character(1) :: numchar
      integer:: i,n,k
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





