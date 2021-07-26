subroutine PrintArrayTest(BasePlane, Side)
    
implicit none
integer :: i,j
integer :: Side
integer, dimension(Side,Side,3) :: BasePlane   

write(*,*) "5"
do i=1, Side
    write(*,*) (BasePlane(i,j,1), j=1,Side )
end do
    
end 