program ising
  implicit none
  real::E,M,mag
  integer::i,j,k,L,p,N,a,b,c,d,r,s
  real::J_ising=1.0
  integer,dimension(:,:,:),allocatable::spin
  write(*,*)"no. of lattice points in one dimension"
  read*,L
  allocate(spin(L,L,L))
E=0.0
M=0.0
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
         E=E+J_ising*float((spin(i,j,k))*(spin(a,j,k)+spin(b,j,k)+spin(c,i,k)+spin(d,i,k)+spin(r,i,j)+spin(s,i,j)))
      end do
   end do
end do
mag=M/float(N)
E=E*0.5d0
print*,'initial energy E=',E
write(*,*),'Energy per spin=',E/float(N)
print*,'initial magnetization M=',M
WRITE(*,*),'M per spin=',M/float(N)
deallocate(spin)
end program ising
