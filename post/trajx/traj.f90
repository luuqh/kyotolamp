


      PROGRAM TRAJECTORY

      implicit none
      real*8:: x,y,z,value,time,dt
      integer*8,allocatable:: ip(:),trj(:)
      integer*8:: id,nm,pm,n0,p0,px,n,p,q,marktp,um,shift,search
      integer*4:: ix,iy,iz,color
      character*80::f_pathin,f_pathout,f_in,f_out
      character*8:: f_prefix,number,f_suffix

      ! basic settings
      f_pathin = '../../lamp/lab3/merge1/'
      f_pathout = '../../lamp/lab3/merge1/'
      f_suffix = 'a' 
      ! f_suffix = '05' 
      n0 = 0        ! first step
      nm = 24*30/6  ! number of steps
      dt = 3600.*6  ! time interval [s]

      ! read initial number of particles
      time = n0*dt
      call timing (time,f_prefix)
      f_in = trim(f_pathin)//trim(f_prefix)//'.g'//trim(f_suffix)
      write(*,*)f_in
      open(unit=0,file=trim(f_in),form='unformatted')
      read(0)p0
      allocate (ip(p0))
      allocate (trj(p0))
      do p = 1,p0
         read(0)x,y,z
         read(0)ix,iy,iz
         read(0)id,color !.ga
         read(0)value
         ip(p) = id
         trj(p) = 0
      enddo

      ! read last number of particles and then calculate the number of missing particles
      time = nm*dt
      call timing (time,f_prefix)
      f_in = trim(f_pathin)//trim(f_prefix)//'.g'//trim(f_suffix)
      write(*,*)f_in
      open(unit=nm,file=trim(f_in),form='unformatted')
      read(nm)pm
      close(nm)
      px = p0 - pm
      write(*,*)'particles in initial:',p0
      write(*,*)'particles in finalst:',pm
      write(*,*)'particles be missing:',px

      ! counting available trajectory of particles
      !! pn = p0
      do n = 1,nm
         time = n*dt
         call timing (time,f_prefix)
         f_in = trim(f_pathin)//trim(f_prefix)//'.g'//trim(f_suffix)
         open(unit=n,file=trim(f_in),form='unformatted')
         read(n)pm
         px = p0-pm
         write(*,*)n,trim(f_in),pm,px
         shift = 0
         do p = 1,pm
            read(n)x,y,z
            read(n)ix,iy,iz
            read(n)id,color !.ga format
            read(n)value
            search = 1
            if (search.eq.1) then
               do q = p+shift,p+px
                  if (search.eq.1) then
                     if (ip(q).eq.id) then 
                        trj(q) = trj(q)+1
                        if (shift.lt.q-p) then
                           shift = q-p
                        endif
                        search = 0
                     endif 
                  endif 
               enddo
            endif
         enddo
         rewind(n)
         read(n)pm  
         do p = 1,p0
            if (trj(p).eq.n-1) then
               write(*,*)ip(p),trj(p)
            endif
         enddo  
         !! pn = pm       
      enddo
      rewind(0)
      read(0)p0

      ! extracting particle trajectory
      um = nm + 1
      f_out = trim(f_pathout)//'trajmis.e'//trim(f_suffix)
      open (unit=um,file=trim(f_out),form='unformatted')
      write(*,*)trim(f_out),p0,um
      write(um)p0
      do p = 1,p0   
         write(um)ip(p)
         write(um)trj(p)
         do n = 0,trj(p)
            read(n)x,y,z
            read(n)ix,iy,iz
            read(n)id,color !.ga format
            read(n)value
            if (id.ne.ip(p)) then
               write(*,*)'Inconsistent ID:',ip(p),id,p,n
            endif
            write(um)x
            write(um)y
            write(um)z
        enddo        
      enddo
      close(um)
      do n = 0,nm
         close(n)
      enddo

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





