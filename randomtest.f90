program randomtest
    implicit none
    real::r
    integer::i

    do i=1,6
        call random_number(r)
        write(*,*)r
    end do
    
end program randomtest