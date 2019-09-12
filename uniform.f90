program uniformdist
    implicit none
    real*8::r
    integer::i,n,m
    integer,allocatable,dimension(:)::bin
write(*,*)"value of n"
read(*,*)n
write(*,*)"no of bins"
read (*,*)m
open(30,file="uniformbin.dat")
      open(31,file="uniformno.dat")

      allocate(bin(0:m-1))

      bin=0
      do i= 1,n
        call random_number(r)
        write(31,*)r
        r=m*r
        bin(int(r))=bin(int(r))+1

      end do
      write(*,*) bin
      
      do i=0,m-1
        write(30,*) i,bin(i)
      end do
end program Uniformdist


