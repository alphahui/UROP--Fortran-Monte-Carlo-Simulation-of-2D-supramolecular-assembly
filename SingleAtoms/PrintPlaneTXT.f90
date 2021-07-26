subroutine PrintPlaneTXT(Side,BasePlane,filename,t)
    
implicit none
    character(len=16) filename
    integer :: i,j,t
    integer :: Side
    integer, dimension(Side,Side,3) :: BasePlane    

write(filename,fmt='(I,A4)') t,".txt"
open(unit=90,file=filename)
do i=1, Side
write(90,'(100I2)')  BasePlane(i,1:Side,1)
end do 
    
end