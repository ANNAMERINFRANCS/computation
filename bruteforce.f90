program bruteforce
    implicit none
    real::r,x,func,L,sigma,variance,brute_int,exact
    integer::i,n
    L=3.0d0
    exact=exp(3.0d0)-1
open(unit=25,file='bruteforce.dat')
write(*,*)"value of n"
read(*,*)n
15 brute_int=0.0d0
sigma=0.0d0
do i=1,n
    call random_number(r)
    x=3.0d0*r
    brute_int=brute_int+func(x)
    sigma=sigma+func(x)*func(x)
end do
brute_int=brute_int/real(n)
sigma=sigma/real(n)
variance=sigma-brute_int*brute_int
brute_int=brute_int*L
variance=sqrt(variance/real(n))*L
write(*,*)n,"",brute_int,"",abs(brute_int-exact)
write(25,*)n,"",brute_int,"",abs(brute_int-exact)
n=n*10
if(n .le. 10000000) goto 15
end program bruteforce
real*4 function func(x)
implicit none
real*4::x
func=exp(x)
end function

