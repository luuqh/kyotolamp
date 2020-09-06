

      ! Make Particles for Sea-Gearn
      ! -------------------------------------------------------------------------
      ! Written by Luu Quang Hung, Kyoto, January 9th 2010
      ! -------------------------------------------------------------------------
      ! History
      ! 2010/01/09: Initization


      PROGRAM MAKE_KYOTOLAMP_PARTICLES

      ! declare variables
      implicit none
      character*80:: f_out, f_topo, f_outr                 ! output & topo filenames
      real*8:: dx, dy, x0, y0                              ! domain sizes and position
      real*8,allocatable:: x(:), y(:), z(:)                ! position of particles in the Earth (long/lat)
      real*8,allocatable:: xx(:,:,:,:), yy(:,:,:,:), zz(:,:,:,:) ! position of particles in the Earth
      integer*4,allocatable:: pcolor(:,:,:,:)
      real*8,allocatable:: lon(:),lat(:)                   ! grid coordinate in the Earth
      real*4,allocatable:: topo(:,:)                       ! bottom topo
      real*8,allocatable:: value(:)                        ! value of volume
      real*8:: slon, slat, plon, plat                      ! map origin point
      integer*4,allocatable:: pxx(:,:), pyy(:,:), pzz(:,:) ! position of particle in domain grid
      integer*4,allocatable:: ip(:), jp(:), kp(:)          ! position of particle in domain grid
      integer*4,allocatable:: color(:)                     ! position of particle in domain grid
      integer*8,allocatable:: id(:),pwriv(:)               ! position of particle in domain grid
      !! integer*4,parameter:: rgx = 5, rgy = 5            ! number of regions, incase of riverine release, rgx=no. of river,rgy=1
      !! integer*4,parameter:: rgx = 20, rgy = 20          ! number of regions, incase of riverine release, rgx=no. of river,rgy=1
      !! integer*4,parameter:: rgx = 5, rgy = 1            ! number of regions, incase of riverine release, rgx=no. of river,rgy=1
      integer*4,parameter:: rgx = 1, rgy = 1               ! number of regions, incase of riverine release, rgx=no. of river,rgy=1
      !! integer*4,parameter:: rgx = 8, rgy = 1            ! number of regions, incase of riverine release, rgx=no. of river,rgy=1
      integer*8:: pm,pn,cr(rgx,rgy)                        ! number of particles
      integer*4:: ds, dsx, dsy                             ! densities of particles per cell
      integer*4:: im, jm, i0, j0, iz, jz                   ! domain size
      integer*4:: izr(rgx,rgy),jzr(rgx,rgy),i0r(rgx,rgy),j0r(rgx,rgy) ! size & location of regions
      integer*4:: ir,jr,pmr(rgx,rgy)                       ! index & location of region
      integer*4:: iriv(rgx),jriv(rgx),priv(rgx)            ! incase of riverine release
      integer*4:: riv_int,riv_steps                        ! incase of riverine release
      character*2:: f_num
      character*4:: f_num4
      integer*4:: is, js, gt, colortopo                    ! calculation steps
      integer*8:: i, j, k, l, m, n, p, q, c                ! index
      
      ! start
      write(*,*)'Begin MAKE_KYOTOLAMP_PARTICLES'
      f_topo = './topo.dat'
      f_out = '/LARGEHS02/gh10056/luu/fab/kl/set/00000000.p'                             ! output file

      ! grid coordinates
      im = 385
      jm = 349
      dx = 1./54.
      dy = 1./72.
      x0 = 127. + 1./6. + 206./18. - 3./54.
      y0 = 30.5 + 192./24. - 3./72.
      allocate(lon(1:im))
      allocate(lat(1:jm))
      do i = 1,im 
         lon(i) = x0 + dx*(i-1)
      enddo
      do j = 1,jm
         lat(j) = y0 + dy*(j-1)
      enddo

      ! read topo
      open(1,file=trim(f_topo),form='unformatted')
      write(*,*)f_topo
      read(1)im ! 136
      read(1)jm ! 131
      allocate(topo(1:im,1:jm))
      read(1)topo
      close(1)
      write(*,*)topo(100,210)

      write(*,*)'im,jm:',im,jm

      ! subdomain and grid-step
      !! -------- Tsugaru Strait
      !! i0 = 52 
      !! j0 = 150
      !! iz = 140 !136 !50 ! should be x*10
      !! jz = 130 !131     ! should be x*10
      !! is = 10
      !! js = jz
      !! slon = lon(1)
      !! slat = lat(1)
      !! -------- Rokkasho Domain
      ! i0 = 1
      ! j0 = 1
      ! iz = 385
      ! jz = 349
      ! is = 30
      ! js = jz
      ! slon = lon(i0)
      ! slat = lat(j0)
      !! -------- Hakodate Mount
      ! i0 = 100
      ! j0 = 215
      ! iz = 15
      ! jz = 25
      ! is = 10
      ! js = jz
      ! slon = lon(1)
      ! slat = lat(1)
      !! -------- Onishi Line
      ! i0 = 120
      ! j0 = 200
      ! iz = 1
      ! jz = 35
      ! is = 10
      ! js = jz
      ! slon = lon(1)
      ! slat = lat(1)
      !! -------- Hakodate for Park 2008
      ! i0 = 110
      ! j0 = 220
      ! iz = 15
      ! jz = 15
      ! is = 1
      ! js = 1
      ! slon = lon(1)
      ! slat = lat(1)
      !! -------- Park 2008 Blue Line
      ! i0 = 60
      ! j0 = 180
      ! iz = 10
      ! jz = 10
      ! is = 1
      ! js = 1
      ! slon = lon(1)
      ! slat = lat(1)
      !! -------- Park 2008 Red & Pink Lines
      ! i0 = 80
      ! j0 = 170
      ! iz = 10 !iz,jz should be 10
      ! jz = 10 
      ! is = 1
      ! js = 1
      ! slon = lon(1)
      ! slat = lat(1)
      !! -------- Hakodate Bay continuous !gt=2
      ! i0 = 115 !114
      ! j0 = 238 !235
      ! iz = 4 !6
      ! jz = 4 !5
      ! is = 3
      ! js = 3
      ! slon = lon(1)
      ! slat = lat(1)
      !! -------- Park continuous !gt=2
      ! i0 = 110!114
      ! j0 = 215 !235
      ! iz = 1 !6
      ! jz = 10 !5
      ! is = 1
      ! js = 3
      ! slon = lon(1)
      ! slat = lat(1)
      !! -------- Park continuous again
      ! i0 = 110
      ! j0 = 210
      ! iz = 1
      ! jz = 10
      ! is = 1
      ! js = 5
      slon = lon(1)
      slat = lat(1)
      !! -------- Hokkaido coast regions continuous !gt=2
      ! i0 = 105
      ! j0 = 220
      ! iz = 25
      ! jz = 20
      ! is = 1
      ! js = 1
      ! slon = lon(1)
      ! slat = lat(1)   
      !! -------- Hakodate Bay continuous
      i0 = 114
      j0 = 233
      iz = 7
      jz = 8
      is = 1
      js = 1
      slon = lon(1)
      slat = lat(1)   
      
      ! particle density per cell (dsx * dsy) 
      ds = 2 !16
      dsx = 1*ds !1 !1*ds
      dsy = 1*ds
      pm = dsx*iz * dsy*jz
      write(*,*)'initial particles: ',pm

      ! setting particles
      gt = 2
      select case (gt)

      case (0)
          write(*,*)'case 0: one particle'

          pm = 1
          allocate(ip(1:pm))
          allocate(jp(1:pm))
          allocate(kp(1:pm))
          allocate(x(1:pm))
          allocate(y(1:pm))
          allocate(z(1:pm))
          allocate(id(1:pm))
          allocate(color(1:pm))
          ip(:) = 0
          jp(:) = 0
          kp(:) = 1
          x(:) = 0.
          y(:) = 0.
          z(:) = 0.
          id(:) = 0
          color(:) = 1
          ip(1) = 30
          jp(1) = 30
          kp(1) = 1
          do m = 1,pm
              x(m) = lon(ip(m)+i0-1)
              y(m) = lat(jp(m)+j0-1)
              z(m) = 0.
              id(1) = m
          enddo

      case (1)
          write(*,*)'case 1: riverine continous release'

          !! riv_int = 2*60.    ! time interval of release for 2 mins (in seconds)
          !! riv_steps = 7200   ! number of steps (7200 for 2 mins = 10 days)
          riv_int = 60*60.
          riv_steps = 24*30

          iriv(1) = 115
          jriv(1) = 235
          priv(1) = 1
          iriv(2) = 117
          jriv(2) = 235
          priv(2) = 1
          iriv(3) = 119
          jriv(3) = 235
          priv(3) = 1
          iriv(4) = 115
          jriv(4) = 237
          priv(4) = 1
          iriv(5) = 117
          jriv(5) = 237
          priv(5) = 1
          iriv(6) = 119
          jriv(6) = 237
          priv(6) = 1
          iriv(7) = 117
          jriv(7) = 239
          priv(7) = 1
          iriv(8) = 119
          jriv(8) = 239
          priv(8) = 1

          pm = 0
          do m = 1,rgx
              pm = pm + priv(m)*riv_steps
          enddo
          allocate(ip(1:pm))
          allocate(jp(1:pm))
          allocate(kp(1:pm))
          allocate(x(1:pm))
          allocate(y(1:pm))
          allocate(z(1:pm))
          allocate(id(1:pm))
          allocate(color(1:pm))
          allocate(value(1:pm))
          c = 0
          do m = 1,rgx
             do k = 1,priv(m)*riv_steps
                c = c + 1
                id(c) = c
                ip(c) = iriv(m)
                jp(c) = jriv(m)
                kp(c) = 1
                x(c) = lon(ip(m))
                y(c) = lat(jp(m))
                z(c) = 0.
                color(c) = m
                value(c) = (k-1)*riv_int
             enddo
          enddo
          write(*,*)'number of released riverine: ',c

      case (2)
          write(*,*)'case 2: riverine rectangular release'

          !! riv_int = 2*60.    ! time interval of release for 2 mins (in seconds)
          !! riv_steps = 7200   ! number of steps (7200 for 2 mins = 10 days)
          riv_int = 60*60.*24/8 !24
          riv_steps = 8*3 !24 !4*30 !30 !4*2*(13*24+16) !24*30

          pn = 0
          do i = 1,iz
              do j = 1,jz
                  if(topo(i+i0-1,j+j0-1).gt.0) then
                      pn = pn + is*js
                  endif
              enddo
          enddo
          write(*,*)'non-zero topo: ',pn
          allocate(pwriv(1:pn))
          pwriv(:) = 1 !unique release
          pm = 0
          do m = 1,pn
              pm = pm + pwriv(m)*riv_steps
          enddo
          allocate(ip(1:pm))
          allocate(jp(1:pm))
          allocate(kp(1:pm))
          allocate(x(1:pm))
          allocate(y(1:pm))
          allocate(z(1:pm))
          allocate(id(1:pm))
          allocate(color(1:pm))
          allocate(value(1:pm))
          c = 0
          pn = 0
          do i = 1,iz
              do j = 1,jz
                  if (topo(i+i0-1,j+j0-1).gt.0) then 
                      do l = 1,is
                          do m = 1,js
                              pn = pn + 1
                              do k = 1,riv_steps
                                  c = c + 1
                                  id(c) = c
                                  ip(c) = i+i0-1
                                  jp(c) = j+j0-1
                                  kp(c) = 1
                                  x(c) = lon(ip(c))+(l-2)*dx/3.
                                  y(c) = lat(jp(c))+(m-2)*dy/3.
                                  z(c) = 0.
                                  color(c) = pn !int(topo(i+i0-1,j+j0-1)) !pn
                                  value(c) = (k-1)*riv_int
                              enddo
                          enddo
                      enddo
                  endif
              enddo
          enddo
          write(*,*)'number of released riverine: ',pn,c,pm


      case (3)
          write(*,*)'case 3: horizontal grid with interval [is]'

          allocate(pxx(1:iz,1:jz))
          allocate(pyy(1:iz,1:jz))
          allocate(pzz(1:iz,1:jz))
          allocate(xx(1:iz,1:jz,1:dsx,1:dsy))
          allocate(yy(1:iz,1:jz,1:dsx,1:dsy))
          allocate(zz(1:iz,1:jz,1:dsx,1:dsy))
          allocate(pcolor(1:iz,1:jz,1:dsx,1:dsy))
          pxx(:,:) = 0
          pyy(:,:) = 0
          pzz(:,:) = 0
          xx(:,:,:,:) = 0.
          yy(:,:,:,:) = 0.
          zz(:,:,:,:) = 0.
          do i = 1,iz
              do j = 1,jz
                  pxx(i,j) = i+i0-1
                  pyy(i,j) = j+j0-1
                  pzz(i,j) = 1  ! surface
                  do k = 1,dsx
                      do l = 1,dsy
                          xx(i,j,k,l) = lon(pxx(i,j)) + (k-1)*dx/dsx
                          yy(i,j,k,l) = lat(pyy(i,j)) + (l-1)*dy/dsy
                          zz(i,j,k,l) = 0. 
                          pcolor(i,j,k,l) = int(i/is)
                      enddo
                  enddo
              enddo
          enddo
          c = 0
          do i = 1,iz
              do j = 1,jz
                  if(topo(i+i0-1,j+j0-1).gt.0) then
                      c = c + 1
                  endif
              enddo
          enddo
          pm = c*dsx*dsy
          write(*,*)c,pm
          write(*,*)'total non-zero topo: ',pm
          allocate(ip(1:pm))
          allocate(jp(1:pm))
          allocate(kp(1:pm))
          allocate(x(1:pm))
          allocate(y(1:pm))
          allocate(z(1:pm))
          allocate(id(1:pm))
          allocate(color(1:pm))
          allocate(value(1:pm))
          ip(:) = 0
          jp(:) = 0
          kp(:) = 0
          x(:) = 0.
          y(:) = 0.
          z(:) = 0.
          id(:) = 0
          color(:) = 1
          c = 0
          do i = 1,iz
              do j = 1,jz
                  if(topo(i+i0-1,j+j0-1).gt.0) then
                      do k = 1,dsx
                          do l = 1,dsy
                             c = c + 1 
                             id(c) = c
                             ip(c) = pxx(i,j)
                             jp(c) = pyy(i,j)
                             kp(c) = pzz(i,j)
                             x(c) = xx(i,j,k,l)
                             y(c) = yy(i,j,k,l)
                             z(c) = zz(i,j,k,l)
                             color(c) = pcolor(i,j,k,l)
                             value(c) = dble(topo(i,j)/dsx/dsy)
                          enddo
                      enddo
                  endif
              enddo
          enddo

          write(*,*)'check:',c
          deallocate(xx,yy,zz,pcolor)
          deallocate(pxx,pyy,pzz)
      endselect      


      ! do p = 1,pm
          ! if((p-int(p/3000)*3000).eq.0) then
          !     write(*,*)int(p*100/pm),'%'
          ! endif
          ! do c = p+1,pm
          !    if (id(c).eq.id(p)) write(*,*)pm,id(c),c,p
          ! enddo
      ! enddo
      close(10)


      open(10,file=trim(f_out),form='unformatted')
      write(10)pm
      write(*,*)'pm:',pm
      do p = 1,pm
          plon = x(p)
          plat = y(p)
          call COR2POS (plon,plat,slon,slat,x(p),y(p)) !convert to metres
          write(10)x(p),y(p),z(p)
          write(10)ip(p),jp(p),kp(p)
          write(10)id(p),color(p)
          write(10)value(p)
      enddo
      close(10)

      ir = int(iz/rgx)
      jr = int(jz/rgy)
      do i = 1,rgx
         do j = 1,rgy
            i0r(i,j) = i0 + ir*(i-1) - 1
            j0r(i,j) = j0 + jr*(j-1) - 1
            izr(i,j) = i0 + ir*i - 1
            jzr(i,j) = j0 + jr*j - 1
            cr(i,j) = 0
         enddo
      enddo
      do p = 1,pm
         m = ip(p)
         n = jp(p)
         do i = 1,rgx
            do j = 1,rgy
               if((m.ge.i0r(i,j)).and.(m.le.izr(i,j))) then
                  if((n.ge.j0r(i,j)).and.(n.le.jzr(i,j))) then
                     cr(i,j) = cr(i,j) + 1
                  endif
               endif
            enddo
         enddo
      enddo
      close(10)
      write(*,*)'cr:',cr,jz,jr

      select case (gt)
      case (3)
         c = 0
         q = 0
         do i = 1,rgx
            do j = 1,rgy
               k = rgy*(i-1) + j
               if (cr(i,j).gt.0) then
                  q = q + 1
                  l = k + 10 ! file index
                  c = c + cr(i,j)

                  call i2s(k,2,f_num)
                  f_outr = trim(f_out)//f_num
                  ! call i2s(q,4,f_num4)
                  ! f_outr = trim(f_out)//f_num4
                  open(1,file=trim(f_outr),form='unformatted')
                  write(*,*)f_outr,cr(i,j)
                  write(1)cr(i,j)
                  do p = 1,pm
                     m = ip(p)
                     n = jp(p)
                     if((m.ge.i0r(i,j)).and.(m.le.izr(i,j))) then
                        if((n.ge.j0r(i,j)).and.(n.le.jzr(i,j))) then
                           k = k + 1
                           write(1)x(p),y(p),z(p)
                           write(1)ip(p),jp(p),kp(p)
                           write(1)id(p),color(p)
                           write(1)value(p)
                        endif
                     endif
                  enddo
                  close(1)
               endif
            enddo
         enddo
         write(*,*)pm,c
         write(*,*)'total non-zero regions: ',q
      case (1)
         ! out put each river point to a file
          c = 0
          do m = 1,rgx
             call i2s(m,2,f_num)
             f_outr = trim(f_out)//f_num 
             open(1,file=trim(f_outr),form='unformatted')
             pn = priv(m)*riv_steps
             write(*,*)f_outr,pn
             write(1)pn
             do k = 1,pn
                c = c + 1
                write(1)x(c),y(c),z(c)
                write(1)ip(c),jp(c),kp(c)
                write(1)id(c),color(c)
                !! write(*,*)c,pn
                write(1)value(c)
             enddo
             close(1)
          enddo
          write(*,*)'number of released riverine: ',c,pm
      case (2)
          c = 0
          do m = 1,pn
             call i2s(m,2,f_num)
             f_outr = trim(f_out)//f_num 
             ! call i2s(m,4,f_num4)
             ! f_outr = trim(f_out)//f_num4
             open(1,file=trim(f_outr),form='unformatted')
             k = riv_steps*pwriv(m)
             write(*,*)f_outr,k
             write(1)k
             do k = 1,pm
                if(color(k).eq.m) then
                   c = c + 1
                   colortopo = int(topo(ip(k),jp(k)))
                   write(1)x(k),y(k),z(k)
                   write(1)ip(k),jp(k),kp(k)
                   write(1)id(k),colortopo
                   write(1)value(k)
                endif
             enddo
             close(1)
          enddo
          write(*,*)'number of released riverine: ',c
      endselect


      ! ending
      deallocate(lon,lat,topo,x,y,z,ip,jp,kp,id,color)
      write(*,*)'Finish MAKE_KYOTOLAMP_PARTICLES'
      END 




      SUBROUTINE COR2POS (sramda,sphai,oramda,ophai,gmercx,gmercy)
      implicit real*8 ( a-h, o-z )

      rearth = 6375.d3 ! earth radius [m]
      rcnv = 3.141592653589793d0/180.d0 ! deg->rad conv factor

      dram   = sramda - oramda
      if( dram .gt. 180. ) dram = dram - 360.
      if( dram .lt.-180. ) dram = dram + 360.
      gmercx = rearth * cos( ophai*rcnv ) * ( dram ) * rcnv

      f1     = ( 45. + sphai*0.5 ) * rcnv
      f0     = ( 45. + ophai*0.5 ) * rcnv
      ff     = tan( f1 )
      ff0    = tan( f0 )
      y0     = rearth * cos( ophai*rcnv ) * log( ff0)
      gmercy = rearth * cos( ophai*rcnv ) * log( ff ) - y0

      return
      END



      SUBROUTINE i2s (number,maxorder,string)
      implicit none
      integer*4,intent(in) :: maxorder
      integer*8,intent(in) :: number
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



