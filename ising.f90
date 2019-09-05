program ising
  implicit none
  real*8::E,M,mag,q
  integer::i,j,k,L,p,N,a,b,c,d,r,s,xx,yy,zz
  real*8::J_ising=1.0
  integer,dimension(:,:,:),allocatable::spin
  write(*,*)"no. of lattice points in one dimension"
  read*,L
  allocate(spin(L,L,L))
E=0.0d0
M=0.0d0
N=L*L*L
spin=1
print*,spin        
do i=1,L
   do j=1,L
      do k=1,L
         a=i+1;b=i-1;c=j+1;d=j-1;r=k+1;s=k-1
         if(i==L)a=1
         if(i==1)b=L
         if(j==L)c=1
         if(j==1)d=L
         if(k==L)r=1
         if(k==1)s=L
         M=M+spin(i,j,k)
         E=E-J_ising*float((spin(i,j,k))*(spin(a,j,k)+spin(b,j,k)+spin(c,i,k)+spin(d,i,k)+spin(r,i,j)+spin(s,i,j)))
      end do
   end do
end do
mag=M/float(N)
E=E*0.5d0
print*,'initial energy E=',E
write(*,*),'Energy per spin=',E/float(N)
print*,'initial magnetization M=',M
WRITE(*,*),'M per spin=',M/float(N)

open(unit40,file='isingequ.dat')
call random_number(q); i=int(q*float(L))+1
call random_number(q); j=int(q*float(L))+1
call random_number(q); k=int(q*float(L))+1
do xx=1,L
   do yy=1,L
      do zz=1,L
         a=i+1,b=1-i,c=i+1,d=1-i,r=1+i,s=1+i
         if(i==L)a=1; if(i==1)b=L
         if(j==L)c=1; if(j==L)d=L
         if(k==L)r=1; if(k==L)s=L
         Ef=-J_ising*float((spin(i,j,k))*(spin(a,j,k)+spin(b,j,k)+spin(c,i,k)+spin(d,i,k)+spin(r,i,j)+spin(s,i,j)))
         end if
      end do
   end do
end do
deallocate(spin)
end program ising
