program exponential
    implicit none
    real*8:: r,func
      integer:: i,n,m
      integer, allocatable,dimension(:) ::bin

      write(*,*) "value of n"
      read(*,*) n
      write(*,*) "no of bins"
      read(*,*) m

      open(34,file="gaussbin.dat")
      open(35,file="gaussno.dat")

      allocate(bin(-m:m))

      bin=0
      do i=1,n
        r=func(0.0d0,1.0d0) 
        write(35,*) r
              if (abs(r) .lt. m) then     
                bin(int(r))=bin(int(r))+1
                 end if
      end do
      write(*,*) bin

      do i=-m,m
        write(34,*) i, bin(i)
      end do
    end program exponential

    real*8 function func(x,y)
      real*8::x,y,z,a,b
      5 call random_number(a)
      call random_number(b)
      a=2*a-1
      b=2*b-1
      z=a*a+b*b
      if(z==0.0d0 .or. z>=1.0d0) goto 5
      z= sqrt(-2*log(z)/z)
      z=a*z
      z=z*y+x
      end function func
      
