program scatterplot
    implicit none
    real*8::p,avg,sigma,sum1,sumsq,avgsq,c_k
    real*8,allocatable,dimension(:)::q
    integer::i,j,n,k
    sum1=0.0d0
sumsq=0.0d0
open(unit=8,file='ck.dat')
open(unit=9,file='scatterplot.dat')

write(*,*)"no of random numbers"
read(*,*)n
do i=1,n
    call random_number(p)
    sum1=sum1+p
    sumsq=sumsq+(p*p)
end do
avg=sum1/real(n)
avgsq=sumsq/real(n)
sigma=sqrt((avgsq)-(avg*avg))
write(*,*)"The mean is",avg
write(*,*)"The standard deviation is",sigma
allocate(q(n))
q=0.0d0
call random_number(q)
do j=1,50
    sum1=0.0d0
    k=j-1
    do i=1,n-k
        sum1=sum1+(q(i)*q(i+k))
    end do
    sum1=sum1/real(n-k)
    c_k=(sum1-(avg*avg))/(sigma*sigma)
    write(8,*),k,c_k
end do
do i=1,n-1
    write(9,*)q(i),q(i+1)
end do
end program scatterplot


