subroutine PrintLog(AtomsAddedInt,AtomsAdded,Temp,t)
    implicit none
    
    integer :: i,j,n,nb                        !coordinate of plane
    integer :: input,AtomsAddedInt,TotalBonding !record input, int of number of atoms added
    integer t                       !cycle passed
    real(kind=16) :: r,time, Temp, TimeAdded,RandomT                      !to store random number generated, simulation time
    real :: AtomsAdded                          !record the atoms added
    real(kind=4):: TransRate(7), TotalTrans


    open(900,file="Simulation.txt")
    write(900,*) "Cycle=", t,"Atoms Added= ", AtomsAddedInt, "System Atoms Added=", AtomsAdded 
    write(900,*) "Temperature=", Temp  
    write(900,*)

end