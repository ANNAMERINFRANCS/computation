program scatter
    implicit none
    real*8::p,avg,sigma,sum1,sumsq,avgsq
    integer::i,n,l

    open(unit=28,file="meanerror.dat")
    
do l=2,8
    n=10**l
    sum1=0.0d0
     sumsq=0.0d0

do i=1,n
    call random_number(p)
    sum1=sum1+p
    sumsq=sumsq+(p*p)
end do
avg=sum1/real(n)
avgsq=sumsq/real(n)
sigma=sqrt((avgsq)-(avg*avg))
write(*,*) n
write(*,*)"The mean is",avg
write(*,*)"The standard deviation is",sigma
write(28,*)1.0d0/sqrt(real(n)), abs(avg-0.5d0)
end do
end program scatter


