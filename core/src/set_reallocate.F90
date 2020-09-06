! **********************************************************
! *                                                        *
! * subroutine : set_reallocate                            *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * reallocate arrays                                      *
! *                              2010/03/26 luu            *
! *                                                        *
! * <<call>>                                               *
! * called by save_particles                               *
! *                                                        *
! **********************************************************



      SUBROUTINE set_reallocate

      use commons
      implicit none
      real*8,pointer:: dummy_r8(:)
      real*4,pointer:: dummy_r4(:)
      integer*4,pointer:: dummy_i4(:)
      integer*8,pointer:: dummy_i8(:)
      integer*8:: newsize, oldsize

      write(*,*)'load subroutine [set_reallocate]'
      write(*,*)'-----------------------------------'

      am = pm
      pm = am !!+ bm
      oldsize = am
      newsize = pm

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = xp(1:oldsize)
      deallocate(xp)
      xp => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = yp(1:oldsize)
      deallocate(yp)
      yp => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = zp(1:oldsize)
      deallocate(zp)
      zp => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = xo(1:oldsize)
      deallocate(xo)
      xo => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = yo(1:oldsize)
      deallocate(yo)
      yo => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = zo(1:oldsize)
      deallocate(zo)
      zo => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = up(1:oldsize)
      deallocate(up)
      up => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = vp(1:oldsize)
      deallocate(vp)
      vp => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = wp(1:oldsize)
      deallocate(wp)
      wp => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_i4 (newsize) )
      dummy_i4 (1:oldsize) = ix(1:oldsize)
      deallocate(ix)
      ix => dummy_i4
      nullify(dummy_i4)

      allocate ( dummy_i4 (newsize) )
      dummy_i4 (1:oldsize) = iy(1:oldsize)
      deallocate(iy)
      iy => dummy_i4
      nullify(dummy_i4)

      allocate ( dummy_i4 (newsize) )
      dummy_i4 (1:oldsize) = iz(1:oldsize)
      deallocate(iz)
      iz => dummy_i4
      nullify(dummy_i4)

      allocate ( dummy_i4 (newsize) )
      dummy_i4 (1:oldsize) = ox(1:oldsize)
      deallocate(ox)
      ox => dummy_i4
      nullify(dummy_i4)

      allocate ( dummy_i4 (newsize) )
      dummy_i4 (1:oldsize) = oy(1:oldsize)
      deallocate(oy)
      oy => dummy_i4
      nullify(dummy_i4)

      allocate ( dummy_i4 (newsize) )
      dummy_i4 (1:oldsize) = oz(1:oldsize)
      deallocate(oz)
      oz => dummy_i4
      nullify(dummy_i4)

      allocate ( dummy_r4 (newsize) )
      dummy_r4 (1:oldsize) = rand(1:oldsize)
      deallocate(rand)
      rand => dummy_r4
      nullify(dummy_r4)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = wsx(1:oldsize)
      deallocate(wsx)
      wsx => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = wsy(1:oldsize)
      deallocate(wsy)
      wsy => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = wsz(1:oldsize)
      deallocate(wsz)
      wsz => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = spz(1:oldsize)
      deallocate(spz)
      spz => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = value(1:oldsize)
      deallocate(value)
      value => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_i4 (newsize) )
      dummy_i4 (1:oldsize) = color(1:oldsize)
      deallocate(color)
      color => dummy_i4
      nullify(dummy_i4)

      allocate ( dummy_i8 (newsize) )
      dummy_i8 (1:oldsize) = id(1:oldsize)
      deallocate(id)
      id => dummy_i8
      nullify(dummy_i8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = xd(1:oldsize)
      deallocate(xd)
      xd => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = yd(1:oldsize)
      deallocate(yd)
      yd => dummy_r8
      nullify(dummy_r8)

      allocate ( dummy_r8 (newsize) )
      dummy_r8 (1:oldsize) = zd(1:oldsize)
      deallocate(zd)
      zd => dummy_r8
      nullify(dummy_r8)


      return
      END




