


      PROGRAM FLUSH


      implicit none
      integer*4,parameter:: rg=16,nm=150 !2*(27*24+6)/6
      integer*8:: kr(rg),pr(rg),id(rg),traj(rg),location(rg)
      integer*8:: idt,pm,p0,n,p,k,l,region
      integer*4:: im,jm,it,jt,kt,colort,i(rg),j(rg)
      real*4,allocatable:: topo(:,:) ! bottom topo
      real*8:: x(rg),y(rg),z(rg),xo(rg),yo(rg),zo(rg),vol(rg),volume(3,nm)
      real*8:: xt,yt,zt,dt,dx,dy,valuet
      character*250::f_pathin1,f_pathin2,f_pathin3,f_pathout
      character*250::f_in1,f_in2,f_in3,f_out,f_suffix
      data kr /6,11,12,13,18,19,20,21,22,26,27,28,29,30,34,35/
      ! basic settings
      f_pathin1 = '../../lamp/lab4/merge/'
      f_pathin2 = '../../lamp/lab4/data/'
      f_pathin3 = './'
      f_pathout = '../../lamp/lab4/flush/'
      dt = 3600 !*6.
      dx = 1./54
      dy = 1./72
      volume(:,:) = 0


      f_in3 = trim(f_pathin3)//'topo.dat' !read topography to get depth at location of particles
      open(1,file=trim(f_in3),form='unformatted')
      write(*,*)f_in3
      read(1)im
      read(1)jm
      allocate(topo(1:im,1:jm))
      read(1)topo
      close(1)
      do k = 1,rg
         n = kr(k)
         call i2s (n,2,f_suffix)
         f_in1 = trim(f_pathin1)//'trajmis.e'//trim(f_suffix) !read sorted particles
         open(unit=k,file=trim(f_in1),form='unformatted')
         read(k)pm
         pr(k) = pm
         write(*,*)k,kr(k),trim(f_in1),pm
         f_in2 = trim(f_pathin2)//'00000000.g'//trim(f_suffix) !read matrix-position of thoses particles
         open(unit=0,file=trim(f_in2),form='unformatted')
         read(0)p0
         read(0)xt,yt,zt
         read(0)it,jt,kt
         read(0)idt,colort !.ga
         read(0)valuet
         close(0)
         i(k) = it
         j(k) = jt
         write(*,*)trim(f_in2),i(k),j(k),topo(i(k),j(k))
         vol(k) = topo(i(k),j(k))*dx*dy
      enddo


      do p = 1,1 !pm
         do k = 1,rg
            read(k)id(k)
            read(k)traj(k)
         enddo
         do n = 0,nm
            do k = 1,rg
               if(n.le.traj(k))then
                  read(k)x(k)
                  read(k)y(k)
                  read(k)z(k)
               else
                  x(k) = xo(k)
                  y(k) = yo(k)
                  z(k) = zo(k)
               endif
               xo(k) = x(k)
               yo(k) = y(k)
               zo(k) = z(k)
               l = region(x(k),y(k)) !1-HB, 2-TS, 3-NP
               if (l.le.3) then
                  location(k) = l
                  volume(l,n) = volume(l,n) + vol(k)
               else
                  location(k) = 0
                  write(*,*)'Error! Out of TS/NP domain',l
               endif
               ! write(*,*)k,x(k),y(k),'@location:',l
            enddo
            write(*,*)n,nm,volume(1,n),volume(2,n),volume(3,n)
         enddo        
      enddo   
      do k = 1,rg
         close(k)
      enddo
      END




      FUNCTION region (px,py)
      ! to determine whether a point is inside a region

      implicit none
      integer*8::inout,location,region
      real*8::px,py,dx,dy,xj(7),yj(7),xn(9),yn(9),xt(9),yt(9),xh(7),yh(7)
      data xh /140.66,140.66,140.68,140.76,140.78,140.78,140.66/       ! Hakodate Bay
      data yh / 41.80, 41.73, 41.70, 41.70, 41.73, 41.80, 41.80/
      data xt /140.4,140.4,140.3,140.3,141.1,141.1,141.4,141.4,140.4/  ! Tsugaru Strait
      data yt / 38.0, 41.2, 41.4, 41.8, 41.8, 41.3, 41.3, 38.0, 38.0/
      data xn /140.3,141.1,141.1,141.4,141.4,146.0,146.0,140.3,140.3/  ! North Pacific
      data yn / 41.8, 41.8, 41.3, 41.3, 38.0, 38.0, 43.3, 43.3, 41.8/
      data xj /138.0,140.4,140.4,140.3,140.3,138.0,138.0/              ! Japan Sea
      data yj / 38.0, 38.0, 41.2, 41.4, 43.3, 43.3, 38.0/ 
      dx = 1./54.
      dy = 1./72.
      px = px + dx !kyotoLAMP's topographic/coordinate shift
      py = py + dy
      call poly(xh,yh,7,px,py,inout)  ! Hakodate Bay
      if (inout.gt.0) then
         location = 1
      else
         call poly(xt,yt,9,px,py,inout)  ! Tsugaru Strait
         if (inout.gt.0) then
            location = 2
         else
            call poly(xn,yn,9,px,py,inout)  ! North Pacific
            if (inout.gt.0) then
               location = 3
            else
               call poly(xj,yj,7,px,py,inout)  ! Japan Sea
               if (inout.gt.0) then
                  location = 4
               else
                  location = 5
               endif
            endif
         endif
      endif
      region = location
      return
      END




      SUBROUTINE poly (xx,yy,n,px,py,inout)                            
      ! to determine whether a point is inside a polygon by Randolph Franklin

      real*8:: x(200),y(200),xx(n),yy(n),px,py
      logical mx,my,nx,ny
      integer*8::o,inout
      ! output unit for printed messages
      data o/6/
      maxdim=200
      if(n.le.maxdim)go to 6
      write(o,7)
7     format('0warning:',i5,' too large. 1results invalid')
      return
6     do 1 i=1,n
        x(i)=xx(i)-px
1       y(i)=yy(i)-py
      inout=-1
      do 2 i=1,n
        j=1+mod(i,n)
        mx=x(i).ge.0.0
        nx=x(j).ge.0.0
        my=y(i).ge.0.0
        ny=y(j).ge.0.0
        if(.not.((my.or.ny).and.(mx.or.nx)).or.(mx.and.nx)) go to 2
        if(.not.(my.and.ny.and.(mx.or.nx).and..not.(mx.and.nx))) go to 3
        inout=-inout
        go to 2
3       if((y(i)*x(j)-x(i)*y(j))/(x(j)-x(i))) 2,4,5
4       inout=0
        return
5       inout=-inout
2     continue
      return
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





