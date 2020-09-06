! **********************************************************
! *                                                        *
! * subroutine : p_boundary                                *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * boundary & surface conditions of particle position     *
! *                              2006/07/28 kobayashi      *
! *                              2010/01/26 luu            *
! *                                                        *
! * <<setting>>                                            *
! * mode_bc =                                              *
! *         1:detailed near wall calculation scheme        *
! *         2:stay on land (ex.oil spill)                  *
! *         3:back to previous position                    *
! *         delete "perfect reflection"                    *
! *                                                        *
! * <<call>>                                               *
! * called by main                                         *
! *                                                        *
! **********************************************************



      SUBROUTINE p_boundary

      use commons
      implicit none
      integer:: ie, je, ke, ieo, jeo, keo
      integer:: k, j, p, i_error1, i_error2, n_error,stop1,stop2,dstep
      real*8:: xsa1, xsa2, ysa1, ysa2, zsa1, zsa2
      real*8:: xeo, yeo, zeo, ui, vi, wi
      real*8:: t

      write(*,*)'load subroutine [p_boundary]'
      write(*,*)'-----------------------------------'


! *** relation between particle and each cells           
! ----------------------------------------------------------

      do k = 1, pm
        call lib_index( im,jm,kb,level,xp(k),yp(k),zp(k),
     &           dxdeg,dydeg,slon,slat,radius,rcnv,z,ix(k),iy(k),iz(k) )

        ! xo(k) = xp(k)
        ! yo(k) = yp(k)
        ! zo(k) = zp(k)

        call lib_index( im,jm,kb,level,xo(k),yo(k),zo(k),
     &           dxdeg,dydeg,slon,slat,radius,rcnv,z,ox(k),oy(k),oz(k))
      enddo

! *** lateral boundary conditions          
! ----------------------------------------------------------

      select case (mode_bc)
      case (1)

! [ detailed near wall calculation scheme ]
! (imasato and awaji, bulletin on coastal oceanography, 20(1),1982)
! ++++++++++++++++++++++++++++++++++++++

      do k = 1,pm

        if ( (ix(k) .ge. 2 .and. ix(k) .le. imm1) .and.
     &       (iy(k) .ge. 2 .and. iy(k) .le. jmm1) .and.
     &       (iz(k) .ge. 1 .and. iz(k) .le. kb-1) ) then

          xeo = xo(k)
          yeo = yo(k)
          zeo = zo(k)
          ieo = ox(k)
          jeo = oy(k)
          keo = oz(k)
          ie = ix(k)
          je = iy(k)
          ke = iz(k)

          if(ke.le.0) ke = 1

          if( fsm(ie,je,ke) .gt. 0.5 )then
            ! if( zp(k) .lt. z(ie,je,1) ) zp(k) = z(ie,je,1)
            ! if( zp(k) .gt. h(ie,je) ) zp(k) = h(ie,je)
            ! zsa1 = zp(k) - z(ie,je,ke  )
            ! zsa2 = z(ie,je,ke+1) - zp(k)
          endif

          if( fsm(ie,je,ke) .eq. 0 )then  ! if 1fsm=0
            xp(k)  = xeo  		! back to previous position
            yp(k)  = yeo
            zp(k)  = zeo
            ie = ieo
            je = jeo
            ke = keo

! 1/10 time step cal. start
! +++++++++++++++++

            stop1 = 0
            stop2 = 0
            i_error1 = 0
            i_error2 = 0
            n_error = 10
            dstep = 20       

            do while (stop2.eq.0)

            do while (stop1.eq.0)

            t = tstep / dstep    ! detailed time step
 
            do j = 1, dstep       ! 1/10 time step cal. start
  
              ! xeo = xp(k)  ! previous position
              ! yeo = yp(k)
              ! zeo = zp(k)
              ! ieo = ie
              ! jeo = je
              ! keo = ke

              xsa1 = (xp(k)-xmesh(ie))    ! interporation of velocity
              xsa2 =-(xp(k)-xmesh(ie+1))
              ysa1 = (yp(k)-ymesh(je))
              ysa2 =-(yp(k)-ymesh(je+1))
              zsa1 = zp(k) - zz(ie,je,ke  )
              zsa2 = zz(ie,je,ke+1) - zp(k)

              if(ke.le.0) ke = 1

              ui = (u(ie,je,ke)*xsa2
     &           + u(ie+1,je,ke)*xsa1)/dx(ie,je)
              vi = (v(ie,je,ke)*ysa2
     &           + v(ie,je+1,ke)*ysa1)/dy(ie,je)
              wi = ( w(ie,je,ke)*zsa2 + w(ie,je,ke+1)*zsa1 )
     &           / dzz(ie,je,ke)

              call lib_random (iini, rand, 3)

              xp(k) = xp(k) + ui*t +
     &                  sqrt(24.0*aam(ie,je,ke)*t*10.)*(0.5 - rand(1))
              yp(k) = yp(k) + vi*t +
     &                  sqrt(24.0*aam(ie,je,ke)*t*10.)*(0.5 - rand(2))
!              zp(k) = zp(k) - wi*t +
!     &                  sqrt(24.0*vd(ie,je,ke)*t*10.)*(0.5 - rand(3))

            enddo ! do j=1,10

            call lib_index (im,jm,kb,level
     &            ,xp(k),yp(k),zp(k)
     &            ,dxdeg,dydeg,slon,slat,radius,rcnv,z,ie,je,ke)

            ix(k) = ie
            iy(k) = je
            iz(k) = ke
 
            if (  ie .le. 1 .or. ie .ge. im .or.
     &        je .le. 1 .or. je .ge. jm ) then
                stop1 = 0

                xp(k)  = xeo  ! back to previous position
                yp(k)  = yeo
                ! zp(k)  = zeo
                ie = ieo
                je = jeo
                ke = keo

                i_error1 = i_error1 + 1
                if (i_error1.gt.n_error) stop1 = 1
            else

              if( fsm(ie,je,ke) .gt. 0.5 )then
                ! if( zp(k) .lt. z(ie,je,1) ) zp(k) = z(ie,je,1)
                ! if( zp(k) .gt. h(ie,je) ) zp(k) = h(ie,je)
              endif

              if( fsm(ie,je,ke) .eq. 0 )then
                i_error1 = i_error1 + 1              

                xp(k)  = xeo ! back to previous position
                yp(k)  = yeo
                ! zp(k)  = zeo
                ie = ieo
                je = jeo
                ke = keo

                if (i_error1.gt.n_error) stop1 = 1
              else
                stop1 = 1
              endif
 
            endif !if ie<=1

          enddo ! while stop1

          if(i_error1.ge.n_error) then
            stop2 = 0
            dstep = 500
            i_error2 = i_error2 + 1
            if (i_error2.ge.n_error) then
              stop2 = 1
              xp(k)  = xeo ! back to previous position
              yp(k)  = yeo
              ! zp(k)  = zeo
              ie = ieo
              je = jeo
              ke = keo
            endif
          else
            stop2 = 1
          endif

          enddo ! while stop2

         endif  ! if 1fsm=0

       else ! if ix(k)>=2
          xp(k) = xo(k) !back to previous position
          yp(k) = yo(k)
          ! zp(k) = zo(k)
       endif    ! if ix(k)>=2
      enddo     ! do k=1,pm

      case(2)

! [ stay on land ]
! ++++++++++++++++++++++++++++++++++++++

      do k = 1, pm
        if( (ix(k) .ge. 2 .and. ix(k) .le. imm1) .and.
     &    (iy(k) .ge. 2 .and. iy(k) .le. jmm1) ) then
          if ( fsm( ix(k)  ,iy(k),  iz(k) ) .eq. 0 ) then
            ! lab_p(k) = 0
          endif ! fsm
        endif ! ix,iy
      enddo ! pm

      case(3) 

! [ back to previous position ]
! ++++++++++++++++++++++++++++++++++++++


      do k = 1, pm
        if( (ix(k) .ge. 2 .and. ix(k) .le. imm1) .and.
     &    (iy(k) .ge. 2 .and. iy(k) .le. jmm1) ) then

          if ( fsm( ix(k)  ,iy(k),  iz(k) ) .eq. 0 ) then
            xp(k) = xo(k)
            yp(k) = yo(k)
            zp(k) = zo(k)

            call lib_index ( im,jm,kb,level,xp(k),yp(k)
     &              ,zp(k),dxdeg,dydeg,slon,slat,radius,rcnv,z
     &              ,ix(k),iy(k),iz(k) )

          endif ! fsm
        endif  ! ix,iy
      enddo  ! pm

      end select 

! *** vertical boundary condition           
! ----------------------------------------------------------

      do k = 1, pm
        if( (ix(k) .ge. 2 .and. ix(k) .le. imm1) .and.
     &    (iy(k) .ge. 2 .and. iy(k) .le. jmm1) ) then

! [ surface ]
! ++++++++++++++++++++++++++++++++++++++
          if( zp(k).lt.0.0 ) then
            zp(k) = 0.0
          endif
          if ( zp(k) .gt. h(ix(k),iy(k))) then ! bottom
            ! zp(k) = 2.0 - zp(k)  !perfect reflection
            ! zp(k) = 1.0
            zp(k) = h(ix(k),iy(k))
          endif
        endif
      enddo                   ! pm

      return
      END







