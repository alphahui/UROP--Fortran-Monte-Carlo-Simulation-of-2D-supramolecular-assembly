subroutine PrintPlaneTXT(Side,BasePlane,filename,t,SimuCycle)
    
implicit none
    character(len=50) filename, filename2
    integer :: i,j,t,SimuCycle
    integer :: Side
    integer, dimension(Side,Side,3) :: BasePlane    


write(filename,'(a)') "./Simulation result"
open(1,file=filename,status='new')
print*, 'ok'
close(1)

write(filename,'(a)') "./Simulation result/Simulation1"
open(2,file=filename,status='new')
close(2)

write(filename,'(a,i0,a)') "./Simulation result/Simulation1/",t,".txt"
open(unit=90,file=filename)
do i=1, Side
write(90,'(100I2)')  BasePlane(i,1:Side,1)
end do 
close(90)
    
end