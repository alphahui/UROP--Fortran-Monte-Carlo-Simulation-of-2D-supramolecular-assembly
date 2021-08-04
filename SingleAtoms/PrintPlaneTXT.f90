subroutine PrintPlaneTXT(Side,BasePlane,filename,t,SimuCycle)
    
implicit none
    character(len=100) filename,file
    integer  i,j,t,SimuCycle
    integer :: Side
    integer, dimension(Side,Side,3) :: BasePlane    
    real(kind=8) :: time



write(file,'(a,i0,a,i0,a)') "./Simulation_result/Simulation_",SimuCycle,"/",t,".txt"
open(unit=90,file=file)
do i=1, Side
write(90,'(100I2)')  BasePlane(i,1:Side,1)
end do 

    
end