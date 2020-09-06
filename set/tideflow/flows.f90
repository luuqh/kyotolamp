

      ! Prepare Tidal Current Data for KyotoLamp
      ! -------------------------------------------------------------------------
      ! Written by Luu Quang Hung, Kyoto, Nov 11th 2011
      ! -------------------------------------------------------------------------
      ! History
      ! 2011/11/11: Create programs




      PROGRAM TIDE_FLOW
      implicit none


      ! declare variables
      integer*4,parameter:: cm=5,im=385,jm=349           ! number of tidal component
      integer*4:: nm,i,j,k,n                             ! temporary index
      integer*4:: tuse(cm),reverse,stop                  ! setting of tidal compnents
      character*80,parameter:: path_in1='./data/'        ! path for data of tidal ellipses
      character*80,parameter:: path_out='../'            ! path for output folder
      character*80:: f_in1,f_in2,f_out1,f_out2           ! file of input and output
      character*2:: tname(cm)                            ! tidal name
      real*4:: u2(im,jm),v2(im,jm)                       ! 2D tidal velocity
      real*4:: au(im,jm,cm),av(im,jm,cm)                 ! 2D tidal phase constants
      real*4:: pu(im,jm,cm),pv(im,jm,cm)                 ! 2D tidal phase constants
      real*8:: tphase(cm),tfreq(cm),f(cm),dt             ! tidal frequency
      real*4,parameter:: pi=3.14159265                   ! pi constant
      data tname /'k1','o1','m2','s2','mf'/              ! data of tidal name
      data tfreq /1.0027,0.9295,1.9322,2.,0.0732/        ! data of frequency [cycles/day]
      data tuse /1,1,1,0,1/                              ! data of tidal components
      data tphase /0,0,0,0,0/                            ! data of initial tidal phases in [0,2pi]


      ! start program
      write(*,*)'BEGIN KYOTO LAMP SETTING - TIDAL FLOWS'
      nm = 4*2*(13*24+16)/1 !24*1 !24*15./6 ! Mf = 13 days 15 hours 52 minutes
      dt = 3600.*1 !*6
      reverse = 1  !+1,time-forward, -1,time-backward


      write(*,*)'Read Input Files:'
      !! select case
        do k = 1,cm           
          f_in1 = trim(path_in1)//trim(tname(k))//'u.uf'
          f_in2 = trim(path_in1)//trim(tname(k))//'v.uf'
          open(1,file=trim(f_in1),form='unformatted')
          open(2,file=trim(f_in2),form='unformatted')
          read(1)i
          read(1)j
          read(1)u2
          read(1)v2
          au(:,:,k) = u2(:,:)
          pu(:,:,k) = v2(:,:)
          write(*,*)trim(f_in1),i,j,u2(10,10)
          close(1)
          read(2)i
          read(2)j
          read(2)u2
          read(2)v2
          av(:,:,k) = u2(:,:)
          pv(:,:,k) = v2(:,:)
          write(*,*)trim(f_in1),i,j,u2(10,10)
          close(2)
        enddo


        f_out1 = trim(path_out)//'sfu.uf'
        f_out2 = trim(path_out)//'sfv.uf'
        open(3,file=trim(f_out1),form='unformatted')
        open(4,file=trim(f_out2),form='unformatted')
        do n = 1,2
           write(n+2)im
           write(n+2)jm
           write(n+2)1
           write(n+2)nm
        enddo
        write(*,*)'Frequencies:'
        do n = 1,cm
          f(n) = tfreq(n)*2*pi/86400.
          write(*,*)1/tfreq(n),f(n)
        enddo


        write(*,*)'Write Output Files:'
        stop = 2
        do n = 1,nm
          u2(:,:) = 0
          v2(:,:) = 0
          do k = 1,cm           
            do i = 1,im
              do j = 1,jm
                u2(i,j) = u2(i,j)+ tuse(k)* au(i,j,k)* cos( f(k)*dt*(n-1)*reverse+ tphase(k)+ pu(i,j,k))
                v2(i,j) = v2(i,j)+ tuse(k)* av(i,j,k)* cos( f(k)*dt*(n-1)*reverse+ tphase(k)+ pv(i,j,k))
                !if ((i.eq.117).and.(j.eq.233).and.(stop.ge.0).and.(tuse(k).gt.0)) then
                !  write(*,*)tname(k),pu(i,j,k)*180/pi,pv(i,j,k)*180/pi
                !  write(*,*)au(i,j,k),au(i,j,k)*cos(f(k)*dt*(n-1)+pu(i,j,k)),av(i,j,k),av(i,j,k)*cos(f(k)*dt*(n-1)+pv(i,j,k))
                !endif
              enddo
            enddo
          enddo
          stop = stop - 1
          write(3)u2
          write(4)v2
          write(*,*)n,u2(10,10)
        enddo
      !! case default
        !! write(*,*)'no option selected'
      !! end select
      close(3)
      close(4)

      write(*,*)'MISSION COMPLETED'
      END 






