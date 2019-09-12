program bruteforce2
    implicit none
    real*8::r,func,L,sigma,variance,brute_int,exact
    integer::i,n
    L=1.0d0
    exact=4.0d0*atan(1.0d0)
open(unit=26,file='bruteforce2.dat')
write(*,*) func(1.0d0)
write(*,*)"value of n"
read(*,*)n
16 brute_int=0.0d0
sigma=0.0d0
do i=1,n
    call random_number(r)
    brute_int=brute_int+func(r)
    sigma=sigma+func(r)*func(r)
end do
brute_int=brute_int/real(n)
sigma=sigma/real(n)
variance=sigma-brute_int*brute_int
brute_int=brute_int*L
variance=sqrt(variance/real(n))*L
write(*,*)n,"",brute_int,"",variance
write(26,*)n,"",brute_int,"",abs(brute_int-exact)
n=n*10
if(n .le. 10000000) goto 16
end program bruteforce2
function func(x) result(y)
implicit none
real*8::x,y
y=4.0d0*(1.0d0/(1.0d0+x**2))
end function

