program exponential
    implicit none
    real*8:: r
      integer:: i,n,m
      integer, allocatable,dimension(:) ::bin

      write(*,*) "value of n"
      read(*,*) n
      write(*,*) "no of bins"
      read(*,*) m

      open(32,file="expbin.dat")
      open(33,file="expno.dat")

      allocate(bin(0:m-1))

      bin=0
      do i=1,n
        call random_number(r)
        r=-log(1-r) 
        write(33,*) r
              if (abs(r) .lt. m) then     
                bin(int(r))=bin(int(r))+1
                 end if
      end do
      write(*,*) bin

      do i=0,m-1
        write(32,*) i, bin(i)
      end do

end program exponential