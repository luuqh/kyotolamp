! **********************************************************
! *                                                        *
! * subroutine : lib_random                                *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * generate random real number                            *
! *                              2006/07/28 kobayashi      *
! *                              2010/01/26 luu (eds)      *
! *                                                        *
! * <<call>>                                               *
! * called by various subroutine!                          *
! *                                                        *
! **********************************************************



      SUBROUTINE lib_random (ii,ix,n)

      real*4:: ix(n)
      real*8:: a,c,m,xx
      a = 32771d0
      c=1234567891d0
      m=2147483648d0
      xx=ii
      do i = 1,n
        xx=dmod(a*xx+c,m)
        ix(i)=sngl(xx/m)
      enddo
      ii=idint(xx)
      return

      END


