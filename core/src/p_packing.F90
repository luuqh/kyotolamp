! **********************************************************
! *                                                        *
! * subroutine : p_packing                                 *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * packing region to exclude the out-of-area particles    *
! *                              2006/07/28 kobayashi      *
! *                              2010/01/26 luu            *
! *                                                        *
! * <<call>>                                               *
! * called by main                                         *
! *                                                        *
! **********************************************************



      SUBROUTINE p_packing

      use commons
      implicit none
      integer*4:: isave(pm), icount, p, it, max 

      write(*,*)'load subroutine [p_packing]'
      write(*,*)'-----------------------------------'

      icount = 0
      do p = 1,pm
        if(xp(p).lt.xmtmin.or.xp(p).gt.xmaxm.or.
     &    yp(p).lt.ymtmin.or.yp(p).gt.ymaxm)then

        else

          ! write(*,*)ix(p),iy(p),iz(p)

          if((ix(p).ge.1).and.(ix(p).le.im).and.
     &       (iy(p).ge.1).and.(iy(p).le.jm).and.
     &       (iz(p).ge.1).and.(iz(p).le.kb)) then

          if (fsm( ix(p),iy(p),iz(p)).eq.1) then
            if((xp(p).ge.0).or.(xp(p).lt.0).or.
     &         (yp(p).ge.0).or.(yp(p).lt.0).or.
     &         (zp(p).ge.0).or.(zp(p).lt.0))then
              icount = icount + 1
              isave(icount) = p
              ! write(*,*)p,icount 
            endif !xp
          endif !fsm
          endif !ix
        endif
      enddo
      write(*,*)icount 

      if (icount.lt.pm) then
        max = icount
        do p = 1,max
          it = isave(p)
          id(p) = id(it)
          xp(p) = xp(it)
          yp(p) = yp(it)
          zp(p) = zp(it)
          ix(p) = ix(it)
          iy(p) = iy(it)
          iz(p) = iz(it)
          color(p) = color(it)
          value(p) = value(it)
        enddo
        do p = max+1,pm
          id(p) = 0
          xp(p) = 0.
          yp(p) = 0.
          zp(p) = 0.
          ix(p) = 0
          iy(p) = 0
          iz(p) = 0
          color(p) = 0
          value(p) = 0.
        enddo
        write(*,*)pm,max
        pm = max
      endif

      return

      END