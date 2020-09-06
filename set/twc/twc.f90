

        PROGRAM TWC
        use netcdf
        implicit none
        integer*4,parameter:: im=385,jm=349,km=1,ia=117,ja=220,dj=15
        character*100 :: path_in,path_out,file_in,file_u,file_v,dimname,vaname
        character*20:: vaxtypename(6),snapfile(2)
        character*8 :: date
        integer :: ncid,nstat,ndims,ngatt,nrecdim,nvars,i_dim,vaxtype,vadim,file_mode
        integer :: nm,n,m,d,days,number
        real :: var2(im,jm),var1(dj)

        path_in = '/LARGEHS02/gh10056/r52975/rokka_data/exp2007/22/'
        path_out = './'
        file_u = trim(path_out)//'sfu.uf'
        file_v = trim(path_out)//'sfv.uf'
        snapfile(1) = './su.uf'
        snapfile(2) = './sv.uf'
        write(*,*)'START PROGRAM'
        write(*,*)'************************************************************'
        file_mode = 1 !1 !0-none;1-basic(hcl,sfu,sfv);2-major(basic+u,v,w,t,s,rhoo);3-all
        ! days = 853 ! start from 2008/01/23 to 2010/05/25
        days = 360 ! start from 2009/12/27 to 2010/12/22
        nm = 8*days
        write(*,*)file_u
        write(*,*)file_v
        write(*,*)'im,jm,km,nm:',im,jm,km,nm
        open(10,file=trim(file_u),form='unformatted')
        open(11,file=trim(file_v),form='unformatted')
        if (file_mode>0) then
           do n = 10,11
              write(n)im
              write(n)jm
              write(n)km
              write(n)nm
           enddo
        else
           do n = 10,11
              write(n)ia
              write(n)ja
              write(n)dj
              write(n)nm
           enddo
        endif
        do d = 1,days
           write(*,*)'--------------------------------------------'
           write(*,*)'STEP',d,'of',days
           write(*,*)'--------------------------------------------'
           call rokkalendar (d,date)
           write(*,*)'DATE: ',date
           file_in = trim(path_in)//trim('rm_rokka22_')//trim(date)//'.nc'
           write(*,*)'FILE: ',file_in
           nstat = nf90_open(trim(file_in),nf90_nowrite,ncid)
           if(nstat /= nf90_noerr) write(*,*) trim(nf90_strerror(nstat))
           nstat = nf90_inquire(ncid,ndims,nvars,ngatt,nrecdim)
           if(nstat /= nf90_noerr) write(*,*) trim(nf90_strerror(nstat))
           write(*,*) 'ndims (dimensions):',ndims,' nvars (variables):',nvars
           write(*,*)'DIMENSIONS'
           do n = 1,ndims
              nstat = nf90_inquire_dimension(ncid,n,dimname,i_dim)
              write(*,*)n,dimname,i_dim
           enddo
           vaxtypename(1) = 'char 8-bit'
           vaxtypename(2) = 'byte 8-bit'
           vaxtypename(3) = 'short integer 16-bit'
           vaxtypename(4) = 'long integer 32-bit'
           vaxtypename(5) = 'float/real 32-bit'
           vaxtypename(6) = 'double 64-bit'
           write(*,*)'VARIABLES'
           do n = 1,nvars
              nstat = nf90_inquire_variable(ncid,n,vaname,vaxtype,vadim)
              write(*,*) n,vaname,vadim,'dimensions ',vaxtypename(vaxtype)
           enddo
           write(*,*)'EXPORTS'
           if(file_mode>=0)then
              do n = 10,11
                 do m = 1,8
                    number = d*8 + m
                    nstat = nf90_get_var(ncid,n,var2,start=(/1,1,m/))
                    if(file_mode>0)then
                       ! write(n)var2
                       write(*,*)'surface velocity',n,var2(10,10)                       
                       if(number.eq.683)then !21/3/2010
                       !! if((d.eq.11).and.(m.eq.6))then !step=94 obtained from analysis contours_insan_twc_hakodatestream.pro
                       !! if((d.eq.34).and.(m.eq.1))then !step=273 obtained from analysis contours_insan_twc_hakodatestream.pro
                          write(n)var2
                       endif
                    else
                       var1(1:dj) = var2(ia,ja+1:ja+dj)
                       write(n)var1
                       write(*,*)'surface speed:',n,var1(int(dj/2))
                       !! if((d.eq.33).and.(m.eq.4))then !step=268 obtained from analysis contours_insan_twc_hakodateline.pro
                       if((d.eq.34).and.(m.eq.1))then !step=273 obtained from analysis contours_insan_twc_hakodatestream.pro
                          open(n+10,file=trim(snapfile(n-9)),form='unformatted')
                          write(n+10)im
                          write(n+10)jm
                          write(n+10)1
                          write(n+10)1
                          write(n+10)var2
                          close(n+10)
                       endif
                    endif
                 enddo
              enddo
           endif
           nstat = nf90_close(ncid)
           if(nstat /= nf90_noerr) write(*,*) trim(nf90_strerror(nstat))
        enddo
        close(10)
        close(11)
        write(*,*)'************************************************************'
        write(*,*)'FINISH!'
        END



        SUBROUTINE i2s (number,maxorder,string)
        implicit none
        integer*4,intent(in) :: number,maxorder
        character(len=maxorder),intent(out) :: string
        character(len=maxorder) :: temp
        character(1) :: numchar
        integer*4:: i,n,k
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
        END





        SUBROUTINE rokkalendar (number,string)
        implicit none
        integer*4,intent(in) :: number
        character*8,intent(out) :: string
        character*2 :: dd,mm
        character*4 :: yyyy
        integer*4 :: months(12),d,m,y,day,month,year
        integer*4 :: n,k
        data months /31,28,31,30,31,30,31,31,30,31,30,31/
        year = 2009 !2008  !origin date
        month = 12 !1
        day = 27 !22 
        y = year
        m = month
        d = day + number
        do while (d.gt.months(m))
            d = d - months(m)
            m = m + 1
            if (m.gt.12) then
                y = y + 1
                m = m - 12
            endif
        enddo
        write(*,*)y,m,d
        call i2s(y,4,yyyy)
        call i2s(m,2,mm)
        call i2s(d,2,dd)
        string = yyyy//mm//dd
        END





