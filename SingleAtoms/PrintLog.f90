subroutine PrintLog(AtomsAddedInt,AtomsAdded,Temp,t,SimuCycle,TotalTrans,HopCount,MoveX,MoveY,LocX,LocY,OutputWhenAtomsMoved)
    implicit none
   

    character(len=200) filename
    integer :: SimuCycle,HopCount,MoveX,MoveY,LocX,LocY                         !coordinate of plane
    integer :: AtomsAddedInt,t !record input, int of number of atoms added            !cycle passed
    real(kind=16) :: Temp, TimeAdded                    !to store random number generated, simulation time
    real :: AtomsAdded                         !record the atoms added
    real(kind=4):: TotalTrans
    real(kind=8):: TransToMeet,r,time
    logical :: OutputWhenAtomsMoved

    write(filename,fmt="(a,i0,a)") "./Simulation_Result/Simulation_",SimuCycle,"/Simulation.txt"
    open(900,file=filename)
    write(900,*) "Cycle=", t,"Atoms Added= ", AtomsAddedInt, "System Atoms Added=", AtomsAdded 
    write(900,*) "Temperature=", Temp
    write(900,*) "TotalTrans=",TotalTrans,"  atoms moved ",HopCount ,"  times"
    if (OutputWhenAtomsMoved==.true.)then
        write(900,*) "atoms moved=" ,MoveX,MoveY,"  Atoms moved to=",LocX,LocY
    end if
    write(900,*)

end