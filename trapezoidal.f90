program trapezoidal
implicit none
real*8::h,a,b,area,sums,f,results,x,error
integer::n,i
a=0.0d0
b=1.0d0
n=1000

error=100.0
open(unit=26,file='trapezoidal.dat')
do while(abs(error)>1e-10)
n=n+1000
h=(b-a)/real(n)
sums=(f(a)+f(b))/2
do i=1,n-1
x=a+h*i
sums=sums+f(x)
end do

results=h*sums
error=4*atan(1.0d0)-results

write(26,*)n,"integral value",results,error
end do

end program trapezoidal

function f(x) result(y)
real*8::x,y
y=4*(1.0d0/(1+x**2))
end function f



