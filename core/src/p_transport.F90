! **********************************************************
! *                                                        *
! * subroutine : p_transport                               *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! *  + decision of advection with linear interpolation     *
! *  + decision of particle new position                   *
! *                              2006/07/28 kobayashi      *
! *                              2010/01/26 luu            *
! *                                                        *
! * <<setting>>                                            *
! * if p_status=2(large particle), it has settling velocity*
! * if p_status=3(seabed sediment), it doesn't move anymore*
! * pm : number of particles                            *
! *                                                        *
! * <<call>>                                               *
! * called by main                                         *
! *                                                        *
! **********************************************************




      SUBROUTINE p_transport(lstep,sstep)

      use commons
      implicit none
      integer:: ie, je, ke, ke2, k, p, lstep, sstep
      real*8:: xe, ye, ze, usk(pm), vsk(pm), wsk(pm)
      real*8:: sum_rand, avg_rand, std_rand(12), sd_rand
      real*8:: dkp, znew, zdc, zdif, zwid, aam1d(pm), rstep

      write(*,*)'load subroutine [p_transport]'
      write(*,*)'-----------------------------------'

! [ set stability at particle position ]
! ++++++++++++++++++++++++++++++++++++++

      do p = 1,pm

! *** advection mesh linear interpolation
! ----------------------------------------------------------

        xo(p) = xp(p)
        yo(p) = yp(p)
        zo(p) = zp(p)
        xe = xp(p)
        ye = yp(p)
        ze = zp(p)

        ! write(*,*)p,id(p),pm,xp(p),yp(p),zp(p)

        call p_interpol ( xe, ye, ze, ie, je, ke, 
     &                    wsx(p), wsy(p), wsz(p), spz(p))
        ix(p) = ie
        iy(p) = je
        iz(p) = ke
        aam1d(p) = aam(ie,je,ke)

        ! calculate Stokes' drift velocity
        call p_drift ( xe, ye, ze, xd(p), yd(p), usk(p), vsk(p), wsk(p))

        ! write(*,*)'flow velocity : ',wsx(p),wsy(p)
        ! write(*,*)'stokes drift : ',usk(p),vsk(p)

      enddo

      ! write(*,*)'advection mesh linear interpolation ok'


! *** diffuskion with randon walk
! ----------------------------------------------------------

! [ random walk method for horizontal diffuskion ]
! ++++++++++++++++++++++++++++++++++++++

      call lib_random (iini, rand, pm)

      do p = 1,pm

        up(p) = 0. 

        if(mode_advect.eq.1) then
          up(p) = up(p) + wsx(p)* tstep
        endif

        if(mode_drift.eq.1) then
          up(p) = up(p) + usk(p)* tstep
        endif

        if(mode_randwalk.eq.1) then
          up(p) = up(p) + sqrt(24.0*aam1d(p)*tstep)*(0.5-rand(p))
        endif

      enddo

      call lib_random (iini, rand, pm)

      do p = 1,pm

        vp(p) = 0. 

        if(mode_advect.eq.1) then
          vp(p) = vp(p) + wsy(p)* tstep
        endif

        if(mode_drift.eq.1) then
          vp(p) = vp(p) + vsk(p)* tstep
        endif

        if(mode_randwalk.eq.1) then
          vp(p) = vp(p) + sqrt(24.0*aam1d(p)*tstep)*(0.5-rand(p))
        endif

      enddo

      ! write(*,*)'diffuskion with randon walk'

! [ random walk method for vertical diffuskion ]
! ++++++++++++++++++++++++++++++++++++++

! standard normal distribution random number
! +++++++++++++++++

      select case ( mode_random )
      case(1)
        call lib_random (iini, rand, pm)
      case(2)
        do p = 1, pm
          call lib_random (iini, std_rand, 12)
          sum_rand = 0.
          do k = 1, 12
            sum_rand = sum_rand + dble(std_rand(k))
          enddo
          rand(p) = -6.0 + sum_rand
        enddo
      end select

! average of random number
! +++++++++++++++++

      sum_rand = 0.
      do p = 1, pm
        sum_rand = sum_rand + rand(p)
      enddo

! standard deviation of lib_random number
! +++++++++++++++++

      avg_rand = sum_rand / pm
      sum_rand = 0.
      do p = 1, pm
        sum_rand = sum_rand + (rand(p)-avg_rand)**2
      enddo
      sd_rand = sqrt(sum_rand/pm)

      do p = 1,pm
        ie = ix(p)
        je = iy(p)
        ke = iz(p)
        dkp = (vd(ie,je,ke)*(z(ie,je,ke+1)-zp(p))
     &        +vd(ie,je,ke+1)*(zp(p)-z(ie,je,ke)) )/dz(ie,je,ke)
        znew = zp(p)+(vd(ie,je,ke+1)-vd(ie,je,ke)) /dz(ie,je,ke)*tstep

        if(znew.lt. 0.) then
          ke2 = 1
          goto 25
        elseif(znew.ge. h(ie,je))then
          ke2 = level(ie,je)
          goto 25
        endif

        do k=1,level(ie,je)
          if((znew.ge.z(ie,je,k) ).and.(znew.lt.z(ie,je,k+1)))ke2=k
        enddo

   25   continue

        zdc = 0.5d0*((vd(ie,je,ke+1 )-vd(ie,je,ke ))/dz(ie,je,ke)
     &        + (vd(ie,je,ke2+1)-vd(ie,je,ke2))/dz(ie,je,ke2))*tstep


! standard normal distribution lib_random number
! +++++++++++++++++

        select case ( mode_random )
        case(1)
          zdif=sqrt(24.0*dkp*tstep)*(0.5-rand(p))
        case(2)
          zdif=sqrt( 2.0*dkp*tstep)*rand(p)
        end select

        zwid = zdc+zdif

        wp(p) = 0.

        if(mode_advect.eq.1) then
          wp(p) = wp(p) + spz(p)*tstep
        endif

        if(mode_randwalk.eq.1) then
          wp(p) = wp(p) + zwid
        endif

      enddo

      ! write(*,*)'vertical ok'


! *** save positions
! --------------------------------------

      rstep = (lstep*sm + sstep)*tstep

      do p=1,pm

! set new position
! +++++++++++++++++

        if ((mode_release.le.1).or.
     &     ((mode_release.eq.2).and.(value(p).le.rstep)))then

          xp(p) = xp(p) + up(p)
          yp(p) = yp(p) + vp(p)
          ! zp(p) = zp(p) - wp(p)

          xd(p) = xp(p) - xo(p)
          yd(p) = yp(p) - yo(p)
          zd(p) = 0 !zp(p) - zo(p)

        else

          xd(p) = 0
          yd(p) = 0
          zd(p) = 0

        endif

      enddo
      return

      END





