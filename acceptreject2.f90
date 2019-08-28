 program acceptreject2
    implicit none
    real::r,x,y,accept_int,exact
    integer::i,n,point
   write(*,*)"value of n"
   read(*,*)n
open(unit=27, file='acceptreject2.dat')
6 point=0
do i=1,n
    call random_number(r)
    if(y .lt. 4/(1+x**2))then
        point=point+1
    end if
end do
accept_int=1.0d0*0.5d0*(real(point)/real(n))
exact=4*atan(1.0d0)
write(27,*)n,"",accept_int,"",abs(accept_int-exact)
n=n*10
if (n .le. 100000000) goto 6
end program acceptreject2
