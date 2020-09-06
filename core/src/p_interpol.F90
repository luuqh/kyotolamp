! **********************************************************
! *                                                        *
! * subroutine : p_interpol                              *
! *                                                        *
! **********************************************************
! *                                                        *
! * <<mission>>                                            *
! * interpolate velocity from grid to selected point       *
! *                              2010/02/20 luu            *
! *                                                        *
! * <<call>>                                               *
! * called by p_transport                                  *
! *                                                        *
! **********************************************************



      SUBROUTINE p_interpol (xe, ye, ze, ie, je, ke, ui, vi, wi, spzi)

      use commons
      implicit none
      integer:: ie, je, ke, kew
      real*8:: xe, ye, ze, ui, vi, wi, spzi
      real*8:: xg1, xg2, yg1, yg2, zg1, zg2, zzg1, zzg2
      real*8:: u1, u2, u12, u34, u56, u78, u1234, u5678
      real*8:: v1, v2, v12, v34, v56, v78, v1234, v5678
      real*8:: u3, u4, u5, u6, u7, u8, v3, v4, v5, v6, v7, v8
      real*8:: w1, w2

! [ treatment of particle above highest z-mesh in the ocean model ]
! ++++++++++++++++++++++++++++++++++++++
! above 1st level means the interaction to atmosphere;
! now reflect to domain at boundary condition.

      call lib_index( im,jm,kb,level,xe,ye,ze,
     &         dxdeg,dydeg,slon,slat,radius,rcnv,z,ie,je,ke)

      ! write(*,*)'xe,ye,ze: ',xe,ye,ze
      ! write(*,*)'ie,je,ke: ',ie,je,ke

      xg1 =  (xe - xmesh(ie))
      xg2 = -(xe - xmesh(ie+1))
      yg1 =  (ye - ymesh(je))
      yg2 = -(ye - ymesh(je+1))
      zg1 =   ze - z(ie,je,ke)
      zg2 = - ze + z(ie,je,ke+1)
      if( ze >= z(ie,je,ke) .and. ze < zz(ie,je,ke) )then
        kew = ke-1
      else
        kew = ke
      endif
      zzg1 =  ze - zz(ie,je,kew)
      zzg2 = -ze + zz(ie,je,kew+1)

! [ x-direction interpolation ]
! ++++++++++++++++++++++++++++++++++++++

! time interpolation
! +++++++++++++++++

      u1 = u(ie  ,je  ,ke  )*(1.-rate) +un(ie  ,je  ,ke  )*rate
      u2 = u(ie+1,je  ,ke  )*(1.-rate) +un(ie+1,je  ,ke  )*rate
      u3 = u(ie  ,je+1,ke  )*(1.-rate) +un(ie  ,je+1,ke  )*rate
      u4 = u(ie+1,je+1,ke  )*(1.-rate) +un(ie+1,je+1,ke  )*rate
      u5 = u(ie  ,je  ,ke+1)*(1.-rate) +un(ie  ,je  ,ke+1)*rate
      u6 = u(ie+1,je  ,ke+1)*(1.-rate) +un(ie+1,je  ,ke+1)*rate
      u7 = u(ie  ,je+1,ke+1)*(1.-rate) +un(ie  ,je+1,ke+1)*rate
      u8 = u(ie+1,je+1,ke+1)*(1.-rate) +un(ie+1,je+1,ke+1)*rate

! space interpolation
! +++++++++++++++++

      u12 = ( u1*xg2 + u2*xg1 ) / dx(ie,je)
      u34 = ( u3*xg2 + u4*xg1 ) / dx(ie,je)
      u56 = ( u5*xg2 + u6*xg1 ) / dx(ie,je)
      u78 = ( u7*xg2 + u8*xg1 ) / dx(ie,je)
      u1234 = ( u12*yg2 + u34*yg1) / dy(ie,je)
      u5678 = ( u56*yg2 + u78*yg1) / dy(ie,je)
      ui = ( u1234*zg2 + u5678*zg1 ) / dz(ie,je,ke)

! [ y-direction interpolation ]
! ++++++++++++++++++++++++++++++++++++++

! time interpolation
! +++++++++++++++++

      v1 = v(ie  ,je  ,ke  )*(1.-rate)+vn(ie  ,je  ,ke  )*rate
      v2 = v(ie+1,je  ,ke  )*(1.-rate)+vn(ie+1,je  ,ke  )*rate
      v3 = v(ie  ,je+1,ke  )*(1.-rate)+vn(ie  ,je+1,ke  )*rate
      v4 = v(ie+1,je+1,ke  )*(1.-rate)+vn(ie+1,je+1,ke  )*rate
      v5 = v(ie  ,je  ,ke+1)*(1.-rate)+vn(ie  ,je  ,ke+1)*rate
      v6 = v(ie+1,je  ,ke+1)*(1.-rate)+vn(ie+1,je  ,ke+1)*rate
      v7 = v(ie  ,je+1,ke+1)*(1.-rate)+vn(ie  ,je+1,ke+1)*rate
      v8 = v(ie+1,je+1,ke+1)*(1.-rate)+vn(ie+1,je+1,ke+1)*rate

! space interpolation
! +++++++++++++++++

      v12 = ( v1*xg2 + v2*xg1 ) / dx(ie,je)
      v34 = ( v3*xg2 + v4*xg1 ) / dx(ie,je)
      v56 = ( v5*xg2 + v6*xg1 ) / dx(ie,je)
      v78 = ( v7*xg2 + v8*xg1 ) / dx(ie,je)
      v1234 = ( v12*yg2 + v34*yg1) / dy(ie,je)
      v5678 = ( v56*yg2 + v78*yg1) / dy(ie,je)
      vi = (v1234*zg2 + v5678*zg1 ) / dz(ie,je,ke)

! [ z-direction interpolation ]
! ++++++++++++++++++++++++++++++++++++++

      w1 = w(ie,je,kew  )*(1.-rate) + wn(ie,je,kew  )*rate
      w2 = w(ie,je,kew+1)*(1.-rate) + wn(ie,je,kew+1)*rate
      wi = ( w1*zzg2 + w2*zzg1) / dzz(ie,je,kew)
      spzi = sqrt(ui**2 + vi**2 + wi**2)


      END




