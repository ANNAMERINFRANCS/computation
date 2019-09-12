
program trap2
    implicit none
    real*8::h,a,b,area,sums,f,results,x,exact,error
    integer::n,i
    a=0.0d0
    b=3.0d0
    n=1000
    error=100.0
    open(unit=22,file='trap2.dat')
    do while(abs(error)>1e-10)
    n=n+10000
    h=(b-a)/real(n)
    sums=(f(a)+f(b))/2
    do i=1,n-1
    x=a+h*i
    sums=sums+f(x)
    end do
    
    results=h*sums
    exact=exp(3.0d0)-1
        error=exact-results
    
        write(*,*)n,"integral value",results,error
        write(22,*)n,"integral value",results,error
    end do
    
    end program trap2
    
    function f(x) result(y)
    real*8::x,y
    y=exp(x)
    end function f
    
    
    
    
