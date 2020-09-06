

      ! Prepare Model Settings for Seagearn
      ! -------------------------------------------------------------------------
      ! Written by Luu Quang Hung, Kyoto, January 11th 2010
      ! -------------------------------------------------------------------------
      ! History
      ! 2010/01/11: Initization


      PROGRAM WRITE_MODEL_SETTINGS
      implicit none

      ! variables
      integer*4,parameter:: im=385,jm=349,km=78
      ! integer*4,parameter:: is=52, js=150
      ! integer*4,parameter:: iz=136,jz=131
      integer*4,parameter:: is=1, js=1
      integer*4,parameter:: iz=385,jz=349
      integer*4:: level_in(im,jm),level_out(iz,jz)
      ! real*4:: topo_in(im,jm),topo_out(iz,jz)
      integer*4:: topo_in(im,jm)
      real*4:: topo_out(iz,jz)
      real*8:: dxdeg,dydeg,slon,slat,dz(km)
      integer*4:: i,j,k

	  ! start read topo & levels
      write(*,*)'Start WRITE_MODEL_SETTINGS'
      open(2,file='./topo.uf',form='unformatted')
      read(2)i
      read(2)j
      read(2)topo_in 
      close(2)
      write(*,*)maxval(topo_in),topo_in(10,10)

      write(*,*)i,j
      open(3,file='./level.uf',form='unformatted')
      read(3)i
      read(3)j
      read(3)level_in
      close(3)
      write(*,*)i,j
      
      ! error in levels (2,1) and (3,1): 537460 -> corrected to 77
      do i=1,im
      do j=1,jm
         if(level_in(i,j).eq.537460)level_in(i,j)=77
      enddo
      enddo

      ! domain information
      dxdeg = 1./54.
      dydeg = 1./72.
      slon = 127. + 1./6. + 206./18. - 3./54. + dxdeg*(is-1)
      slat = 30.5 + 192./24. - 3./72. + dydeg*(js-1)

	  ! depth of each layer
      do k=1,5
         dz(k)=4.d2
      enddo
      do k=6,41
         dz(k)=5.d2
      enddo
      dz(42)=6.d2
      dz(43)=7.d2
      dz(44)=8.d2
      dz(45)=9.d2
      do k=46,52
         dz(k)=10.d2
      enddo
      do k=53,62
         dz(k)=20.d2
      enddo
      do k=63,64
         dz(k)=50.d2
      enddo
      dz(65)=100.d2
      do k=66,67
         dz(k)=200.d2
      enddo
      do k=68,69
         dz(k)=300.d2
      enddo
      do k=70,71
         dz(k)=400.d2
      enddo
      do k=72,78
         dz(k)=500.d2
      enddo

      topo_out(1:iz,1:jz)=real(topo_in(is:is+iz-1,js:js+jz-1))/100
      level_out(1:iz,1:jz)=level_in(is:is+iz-1,js:js+jz-1)
      do k=1,78
         dz(k)=dz(k)/100
      enddo

      ! save to file
      open(9,file='../grids.dat',form='unformatted')
      write(9)iz,jz,km !integer*4
      write(9)slon,slat,dxdeg,dydeg !real*8
      write(9)dz !real*8
      write(9)topo_out  !real*4
      write(9)level_out !integer*4

      close(9)

      ! open(9,file='../grids.dat',status='new')
      ! write(9,*)iz,jz,km
      ! write(9,*)slon,slat,dxdeg,dydeg
      ! do k = 1,km
      !  write(9,*)dz(k)
      ! enddo
      ! do j = 1,jz
      !  write(9,*)(dble(topo_out(i,j)),i=1,iz)
      ! enddo
      ! do j = 1,jz
      !   write(9,*)(level_out(i,j),i=1,iz)
      ! enddo
      ! close(9)

      ! end
      write(*,*)'Finish WRITE_MODEL_SETTINGS'
      END 

