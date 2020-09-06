! **********************************************************
! *                                                        *
! * subroutine : load_grids                                *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * read bathymetry data from external file                *
! *                              2006/07/28 kobayashi      *
! *                              2010/01/26 luu            *
! *                                                        *
! * <<call>>                                               *
! * called by main                                         *
! *                                                        *
! **********************************************************



      SUBROUTINE load_grids

      use commons
      implicit none
      character*256:: f_grid
      integer :: i, j, k, u_grid
      real*8:: gmercx, gmercy

      write(*,*)'load subroutine [load_grids]'
      write(*,*)'-----------------------------------'

      f_grid = trim(d_input0)//'grids.dat' ! model information file
      u_grid = 20
      open(u_grid,file=trim(f_grid),status='old',form='unformatted') ! read input files
      read(u_grid)im,jm,kb !integer*4

      write(*,*)f_grid

      call set_grids

      read(u_grid)slon,slat,dxdeg,dydeg   !real*8
      read(u_grid)delz    !real*8
      read(u_grid)h       !real*4   
      read(u_grid)level   !integer*4
      close(u_grid)

      write(*,*)im,jm,kb

      do j = 1,jm
        do i = 1,im
          alon(i,j) = slon + dble(i-1) * dxdeg
          alat(i,j) = slat + dble(j-1) * dydeg
          xgrid(i,j) = gmercx( alon(i,j), alat(i,j), radius
     &                       , slon, slat, rcnv )
          ygrid(i,j) = gmercy( alon(i,j), alat(i,j), radius
     &                       , slon, slat, rcnv )
          fm(i,j) = 1.0d0
         enddo
      enddo
      dep(1) = 0.0d0 ! depth of vertical grid
      dep(2) = delz(1)
      do k = 2, kb
        dep(k+1) = dep(k) + delz(k)
      enddo

      z = 0.0d0 ! vertical grid position
      do j = 1,jm
        do i = 1,im
          if( level(i,j) > 0 )then
            do k = 1,kb
              z(i,j,k) = dep(k)
            enddo
            z(i,j,level(i,j)) = h(i,j)
          endif
        enddo
      enddo

      zz = 0.0d0 ! vertical grid position (middle point)
      do j = 1,jm
        do i = 1,im
          do k = 1,kb-1
             zz(i,j,k) = z(i,j,k) + (z(i,j,k+1)-z(i,j,k))*0.5d0
          enddo
          zz(i,j,kb) = z(i,j,kb) + (z(i,j,kb)-z(i,j,kb-1))*0.5d0
          zz(i,j,0) = z(i,j,1) - (zz(i,j,1)-z(i,j,1))
        enddo
      enddo
      dz = 0.0d0 ! vertical grid interval
      dzz = 0.0d0
      do j = 1,jm
        do i = 1,im
          do k = 1,kb-1
            dz(i,j,k) = z(i,j,k+1) - z(i,j,k)
            dzz(i,j,k) = zz(i,j,k+1) - zz(i,j,k)
          enddo
          dz(i,j,kb) = dz(i,j,kb-1)
          dzz(i,j,kb) = dzz(i,j,kb-1)
          dzz(i,j,0) = zz(i,j,1) - zz(i,j,0)
        enddo
      enddo
      fsm = 0.0d0 ! sea or land flag
      do j = 1,jm
        do i = 1,im
          do k = 1,kb
            if( k < level(i,j) )then
              fsm(i,j,k) = 1.0d0
            else
              fsm(i,j,k) = 0.0d0
            endif
          enddo
        enddo
      enddo
      do j = 2,jm-1
        do i = 2, im-1
          dx(i,j) = xgrid(i+1,j) - xgrid(i,j)
          dy(i,j) = ygrid(i,j+1) - ygrid(i,j)
        end do
      end do
      do j= 1,jm
        dx(1,j) = dx(2,j)
        dx(im,j) = dx(imm1,j)
        dy(1,j) = dy(2,j)
        dy(im,j) = dy(imm1,j)
      end do
      do i = 1,im
        dx(i,1) = dx(i,2)
        dx(i,jm) = dx(i,jmm1)
        dy(i,1) = dy(i,2)
        dy(i,jm) = dy(i,jmm1)
      end do
      do i = 1,im
        fm(i,1)  = fm(i,2)
        fm(i,jm) = fm(i,jmm1)
        fn(i,1)  = fn(i,2)
        fn(i,jm) = fn(i,jmm1)
      end do
      do j = 1,jm
        fm(1,j)  = fm(2,j)
        fm(im,j) = fm(imm1,j)
        fn(1,j)  = fn(2,j)
        fn(im,j) = fn(imm1,j)
      end do
      do j = 1,jm
        do i=1,im
          ! volscl(i,j) = fm(i,j) * fm(i,j)
        enddo
      enddo

      do i=1,im
        xmesh(i) = xgrid(i,1)
      enddo
      do j=1,jm
        ymesh(j) = ygrid(1,j)
      enddo
      xmtmin = xgrid(2,1)
      ymtmin = ygrid(1,2)
      xmaxm = xgrid(imm1,1)
      ymaxm = ygrid(1,jmm1)

      return
      END




