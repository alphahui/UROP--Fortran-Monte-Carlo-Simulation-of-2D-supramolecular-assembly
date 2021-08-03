subroutine PrintLog(AtomsAddedInt,AtomsAdded,Temp,t,SimuCycle,MoveX,MoveY,LocX,LocY,r,TotalTrans,TransToMeet,time)
    implicit none
    
    character(len=100) filename
    integer :: SimuCycle,MoveX,MoveY,LocX,LocY                        !coordinate of plane
    integer :: AtomsAddedInt,t !record input, int of number of atoms added            !cycle passed
    real(kind=16) :: Temp, TimeAdded                    !to store random number generated, simulation time
    real :: AtomsAdded                         !record the atoms added
    real(kind=4):: TotalTrans
    real(kind=8):: TransToMeet,r,time

    write(filename,fmt="(a,i0,a)") "./Simulation_result/Simulation_",SimuCycle,"/Simulation.txt"
    open(900,file=filename)
    write(900,*) "Cycle=", t,"Atoms Added= ", AtomsAddedInt, "System Atoms Added=", AtomsAdded 
    write(900,*) "Temperature=", Temp
    write(900,*) "TotalTrans=",TotalTrans , "r generated=",r,"TransToMeet",TransToMeet  
    write(900,*) "Atoms Moved=",MoveX,MoveY,"Moved to=",LocX,LocY
    write(900,*)

end