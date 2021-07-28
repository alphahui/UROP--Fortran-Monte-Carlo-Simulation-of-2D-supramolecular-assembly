subroutine PrintLog(AtomsAddedInt,AtomsAdded,Temp,t,SimuCycle)
    implicit none
    
    character(len=100) filename
    integer :: i,j,n,nb,SimuCycle                        !coordinate of plane
    integer :: input,AtomsAddedInt,TotalBonding !record input, int of number of atoms added
    integer t                       !cycle passed
    real(kind=16) :: r,time, Temp, TimeAdded,RandomT                      !to store random number generated, simulation time
    real :: AtomsAdded                          !record the atoms added
    real(kind=4):: TransRate(7), TotalTrans

    write(filename,fmt="(a,i0,a)") "./Simulation_result/Simulation_",SimuCycle,"/Simulation.txt"
    open(900,file=filename)
    write(900,*) "Cycle=", t,"Atoms Added= ", AtomsAddedInt, "System Atoms Added=", AtomsAdded 
    write(900,*) "Temperature=", Temp  
    write(900,*)

end