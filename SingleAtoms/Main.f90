program Main
use parameters

implicit none

    character(len=200)  filename                                    !set file name
    
    integer, dimension(Side,Side,3) :: BasePlane                    !initial plane of each time (1:0=empty 1=atom 2: atom tag)
    integer :: i,j,n,nb,LargestIslandSize,OverallLargestIslandSize  !i,j: coordinate of plane n: dummy nb: number of bonding                                                                     !LargestIslandSize: largest island size in 1 simulation OverallLargestIslandSize: LargestIslandSize but after all simulation
    integer :: input,AtomsAddedInt,TotalBonding,TimeMove                     !input :record input, int of number of atoms added
    integer :: t, SimuCycle, TotSimuCycle,MoveX,MoveY,LocX,LocY,HopCount                              !t :cycle passed Simucycle: record current round of simulation  TotSimucycle : record the number of simulation wanted to do 
    integer :: ScatterIslands,NoOfIsland
    real(kind=16) :: Temp                                         !r :to store random number generated Temp : for temperature
    real(kind=8) :: AtomsAdded,AverageLargestIslandSize,AverageNumberOfIsland,AverageNumberOfScatterIsland,r,time,TransToMeet,AvgHopCount,AvgT                        
    real(kind=4):: TransRate(7)
    integer:: NextTxtPrinted                                     !NextTxtPrinted :to get the next cycle which prints the plane
    integer, dimension(NoOfAtoms) :: IslandSize                    !IslandSize: number of islands vary in size for 1 simulation before final process (not all islands are catergorzied)
    logical :: AtomsMoved, EndSimulation                                           !AtomsMoved: record rather atoms are moved in the cycle
    integer, dimension(NoOfAtoms) :: IslandSizeData                 !IslandSizeData: proccesed IslandSize Data 
    real, dimension(NoOfAtoms) :: FinalIslandSize                   !FinalIslandSize: IslandSize for all simulation combined
    integer:: TempRecordMovedCycle(CycleRecord)
    real, dimension(NoOfAtoms,2) :: AtomsTagTransRate                 ! dimension 1: atom tag from BasePlane 2 1: number of bonding 2: trasrate
    real :: TotalTrans, CurrentIslandData(3),PreviousIslandData(3)      !1: island size 2:island number 3:scatter number
    integer,dimension(MaxTime)::LargestIslandSizeCycle,NumberIslandCycle,NumberScatterCycle
    
    
    call random_seed 
    
!----------------------------------------------------------------------------------------
!---------------initialization------------------------------
!check rather number of atoms are normal 
   
    if (NoOfAtoms == 0) then
    print*, "There is no atoms to be added"
    stop
    end if
    
    if (NoOfAtoms <0) then
    print*, "The number is negative and invalid"
    stop
    end if 
    
    if (NoOfAtoms >= Side*Side) then
    print*, "There is too many atoms"
    stop
    end if 



!ask setting for simulation
    print*, "What are the settings? (1 for Default which can be edit in Main.F90, 2 for customization)"
300 read*, input
    If  (input ==1) then 
        goto 102 
    else if (input ==2) then
        goto 200
    else
        print*, "please enter a valid value. (1 for Default which can be edit in Main.F90, 2 for customization) "
        goto 300
    end if
   
    
200 print*, "Do you want the atoms to be added ",AtomsAddedPerCycle ,"per cycle,which can be edited in parameters.F90? (1 for yes, 2 for no and atoms will be all added at beginning)"
400 read*, input
    If  (input ==1) then 
        AtomsAddedOverTime =.true.
    else if (input ==2) then
        AtomsAddedOverTime =.false.
    else
        print*, "please enter a valid value. (1 for yes, 2 for no and atoms will be all added at beginning)"
        goto 400
    end if
!currently not used and not tested
700 print*, "Do you want result output only when atoms moved? (1 for yes, 2 for no)"
800 read*, input
    If  (input ==1) then 
        OutputWhenAtomsMoved =.true.
    else if (input ==2) then
        OutputWhenAtomsMoved =.false.
    else
        print*, "please enter a valid value. (1 for yes, 2 for no and atoms will be all added at beginning)"
        goto 800
    end if

    print*, "How many simulation do you want to run for this variable?"
    read*, TotSimuCycle
!currently not made
500 print*, "Do you want the temperature to be increased by",TempIncPerCycle ,"per cycle,which can be edited in parameters.F90? (1 for yes, 2 for no and the temperature will be", Tc,")"
600 read*,  input
    If  (input ==1) then 
        TempIncreaseOverTime=.true. 
        
    else if (input ==2) then
        TempIncreaseOverTime=.false.
        
    else
        print*, "please enter a valid value. (1 for yes, 2 for no and the temperature will be", Tc,")"
        goto 600
    end if  

501 print*,"Do you want the simulations to be stopped when there is little change?(1 for yes, 2 for no)"
    print*, "(The percentage difference can be changed in parameters.f90)"
502 read*, input
    If  (input ==1) then 
        StopAtOptimal=.true. 
        goto 100
    else if (input ==2) then
        StopAtOptimal=.false.
        goto 100
    else
        print*, "please enter a valid value. (1 for yes, 2 for no and the temperature will be", Tc,")"
        goto 502
    end if  

    
       
!Default settings
102 AtomsAddedOverTime= .false.
TempIncreaseOverTime= .false.
OutputWhenAtomsMoved=.false.
TotSimuCycle =1


!------------Very beginning before all simulation-------
100 SimuCycle=1

do n=1, NoOfAtoms
    FinalIslandSize(n)=0
end do
do n=1, CycleRecord
   TempRecordMovedCycle(n)=0
end do
AverageLargestIslandSize=0
AverageNumberOfIsland=0
AverageNumberOfScatterIsland=0  


!-------------------Start of a simulation-----------------
101 if (SimuCycle <= TotSimuCycle) then
    
!initialize array to all empty
    do i=1, Side
            do j=1, Side
                
                BasePlane(i,j,1)=0
                
            end do
    end do

    do nb=1, 7
        TransRate(nb)=fc*exp(-(Ed+(nb-1)*Eb)/(kB*Tc))
        print*,TransRate(nb)
    end do 
    do n=1,3
        CurrentIslandData(n)=0
        PreviousIslandData(n)=0 
    end do
    do n=1,MaxTime
        LargestIslandSizeCycle(n)=0
        NumberIslandCycle(n)=0
        NumberScatterCycle(n)=0
    end do
    t=1
    NextTxtPrinted=1   
    AtomsAdded =0
    Temp=Tc
    AtomsAddedInt=0
    TimeMove=1
    time =0
    HopCount =0
    EndSimulation=.false.

   

write(filename,'(a,i0)') "mkdir Simulation_result\Simulation_",SimuCycle
call system(filename)

    !Basic info of the simulation
    write(filename,"(a,i0,a)") "./Simulation_Result/Simulation_",SimuCycle,"/Simulation.txt"
    open(900,file=filename)
    write(900,*) "-----Setting of Simulation-----"
    write(900,*) "Side of plane=", Side,"Total Atoms=", NoOfAtoms
    If (AtomsAddedOverTime == .true.) then
        write(900,*) "Atoms IS added over time, with atoms per cycle=", AtomsAddedPerCycle
    else
        write(900,*) "Atoms are all added at once"
    end if  
    write(900,*) "Initial temperature =",Tc, "Diffusion energy=", Ed,"Bonding energy=",Eb
        write(900,*) "Transition rate:"
        do nb=1, 7
        write(900,*) TransRate(nb)
        end do 
    write(900,*)
    write(900,*) "-----Cycle Start-----"
    

    !-----------------end of initialization----------------------------------------
    !check number of neigbour atom, each atom rather attmept transition with transrate, random location jump
    !--------------------cycle----------------------------------------------


   
    10 if (t<= MaxTime) then !determine whether we have passed cycle limit
        !---------------start of cycle t--------------------------
        if (StopAtOptimal==.true.)then
            if (t==NextTxtPrinted) then

                call FindNoOfIslands(Side,BasePlane,AtomsAddedInt,SizeAsIsland,IslandSize,LargestIslandSize,SimuCycle,ScatterIslands,NoOfIsland,HopCount,StopAtOptimal)
                LargestIslandSizeCycle(t)=LargestIslandSize
                NumberIslandCycle(t)=NoOfIsland
                NumberScatterCycle(t)=ScatterIslands

                if (t .ne. 1)then
                    EndSimulation =.false.
                    CurrentIslandData(1)=LargestIslandSize
                    CurrentIslandData(2)=NoOfIsland
                    CurrentIslandData(3)=ScatterIslands
                    call DetermineWhetherStop(CurrentIslandData,PreviousIslandData,PercentageIslandSize,PercentageIslandNumber,PercentageScatterNumber,EndSimulation)
                    if (EndSimulation==.true.) then
                        print*,"percentage difference greater than set"
                        t=t-1
                        call PrintPlaneTXT(Side,BasePlane,t,SimuCycle)
                        goto 120 !end of simulation
                    end if
                end if

                PreviousIslandData(1)=LargestIslandSize
                PreviousIslandData(2)=NoOfIsland
                PreviousIslandData(3)=ScatterIslands
            end if
        end if

         !Add Atoms  
    if (AtomsAddedInt < NoOfAtoms) then
        if  (AtomsAddedOverTime == .false.) then    !setting for atoms to all be added at once
        !add the molecules to the BasePlane and ResultPlane all at once
            do while (AtomsAdded < NoOfAtoms)
                
                2 call random_number(r)
                
                r=r*Side+1
                i=int(r)
                call random_number(r)
                r=r*Side+1
                j=int(r) 
                
                if (i <= Side .AND. i >=1 .AND. j <= Side .AND. j >=1) then
                    if (BasePlane(i,j,1) == 0) then
                        BasePlane(i,j,1) = 1
                        AtomsAdded = AtomsAdded + 1
                        
                    else
                        goto 2
                    end if
                else 
                    goto 2   
                end if 

            end do

            AtomsAddedInt= AtomsAdded
            call GetTransRate(Side,BasePlane,TransRate,AtomsTagTransRate,TotalTrans,NoOfAtoms )
     
        
        else if  (AtomsAddedOverTime == .true.) then  !Atoms added one by one

            if (AtomsAdded < NoOfAtoms ) then
        !Add atoms per cycle
                call AddAtoms(Side,AtomsAdded,AtomsAddedPerCycle,AtomsAddedInt,BasePlane,NoOfAtoms)

            end if 

            call GetTransRate(Side,BasePlane,TransRate,AtomsTagTransRate,TotalTrans,NoOfAtoms )
         
        end if    !done adding all atoms
    end if

    11 if (time < t) then
    !----------------start of time inside cycle t-------------------------


    !Get the time to start the transition
    12 call random_number(r)
    if (r<=lowlimit .or. r>=uplimit) goto 12   
    time=time+(log(r))/(-TotalTrans)   
 
    !get the transiton rate to find the random atom to move
    call random_number(r)
    TransToMeet= r*TotalTrans    
  
    !do transisiton
    if (AtomsAddedInt>0) then
        AtomsMoved=.false.
        call FindAtomToBeMoved(BasePlane,Side,AtomsTagTransRate,TransToMeet,MoveX,MoveY,NoOfAtoms)
        call MakeTransitionNew(BasePlane,Side,AtomsTagTransRate,MoveX,MoveY,TotalTrans,TransToMeet,LocX,LocY,NoOfAtoms)
        !moving the atom and updating the tag
        
        BasePlane(MoveX,MoveY,1)=0
        BasePlane(LocX,LocY,1)=1
      
        if(MoveX .ne. LocX .and. MoveY .ne. LocY)then
            AtomsMoved=.true.
            HopCount =HopCount +1
        end if

        call GetTransRate(Side,BasePlane,TransRate,AtomsTagTransRate,TotalTrans,NoOfAtoms )

    end if


  

    !------------------output------------------

    !create txt

    !------------------currently not used and not tested------------------
    !if settings allow output onlu when atoms are moved   
    if (OutputWhenAtomsMoved == .true.) then
      
    if (t==MaxTime .OR. t == 1) then
        if (AtomsMoved == .false.) then

        call PrintLog(AtomsAddedInt,AtomsAdded,Temp,t,SimuCycle,TotalTrans,HopCount,MoveX,MoveY,LocX,LocY,OutputWhenAtomsMoved)
        call PrintPlaneTXT(Side,BasePlane,t,SimuCycle)

        end if
    end if

    if (AtomsMoved == .true.) then

        call PrintLog(AtomsAddedInt,AtomsAdded,Temp,t,SimuCycle,TotalTrans,HopCount,MoveX,MoveY,LocX,LocY,OutputWhenAtomsMoved)
        call PrintPlaneTXT(Side,BasePlane,t,SimuCycle)
           
        TempRecordMovedCycle(TimeMove)= t
        TimeMove = TimeMove+1
        
    end if 

    end if 
    !------------------------------------------------------------------

    !currently used to output text
    !if setting have output every n cycle
    if (OutputWhenAtomsMoved==.false.) then
    if (t==NextTxtPrinted) then

        NextTxtPrinted=NextTxtPrinted+TimeInterval
        call PrintPlaneTXT(Side,BasePlane,t,SimuCycle)
        call PrintLog(AtomsAddedInt,AtomsAdded,Temp,t,SimuCycle,TotalTrans,HopCount,MoveX,MoveY,LocX,LocY,OutputWhenAtomsMoved)
    end if 
    end if


    goto 10 !end of cycle go back to start interms of time

    end if 

    print"(a,i0,a,i0,a,f,a)", "simulation= ",SimuCycle,'  cycle= ',t, "time=", time,'  rendered'
    t=t+1
    goto 10

    end if
    

    !-------------------end of 1 Simulation--------------------------------
   120 print*, "end of simulation",SimuCycle

    call FindNoOfIslands(Side,BasePlane,AtomsAddedInt,SizeAsIsland,IslandSize,LargestIslandSize,SimuCycle,ScatterIslands,NoOfIsland,HopCount,StopAtOptimal)
    call PrintIslandSizeTXT(AtomsAddedInt,IslandSize,LargestIslandSize,IslandSizeData,NoOfAtoms,TotSimuCycle,SimuCycle,TimeInterval,LargestIslandSizeCycle,NumberIslandCycle,NumberScatterCycle,MaxTime,t)
    call RecordSimulationData(LargestIslandSize,IslandSizeData,NoOfAtoms,SimuCycle,FinalIslandSize)
    
    if(OutputWhenAtomsMoved==.true.) then
        call PrintMoveTimeRawData(TempRecordMovedCycle,CycleRecord,t,SimuCycle,TimeMove)
    end if
    
    !To compare largest island size in the round and the largest island of all simulation combined
    If (SimuCycle == 1) then
        OverallLargestIslandSize =LargestIslandSize
        
    else
        if (LargestIslandSize > OverallLargestIslandSize) then
            OverallLargestIslandSize=LargestIslandSize
        end if 
    end if

    !To calculate average island size of all simulation
    AverageLargestIslandSize=(LargestIslandSize+AverageLargestIslandSize*(SimuCycle-1))/SimuCycle
    AverageNumberOfIsland=(NoOfIsland+AverageNumberOfIsland*(SimuCycle-1))/SimuCycle
    AverageNumberOfScatterIsland=(ScatterIslands+AverageNumberOfScatterIsland*(SimuCycle-1))/SimuCycle
    AvgHopCount = (HopCount+AvgHopCount*(SimuCycle-1))/SimuCycle
    AvgT=(t+AvgT*(SimuCycle-1))/SimuCycle  
    SimuCycle = SimuCycle +1


    goto 101 !return to initialize new simulation

end if 

!------------------ end of all simulation -------------

call PrintFinalSizesRawData(OverallLargestIslandSize,FinalIslandSize,NoOfAtoms,)
call PrintFinalSimulationLog(Side,NoOfAtoms,SizeAsIsland,TimeInterval,MaxTime,AtomsAddedPerCycle,TempIncPerCycle,Ed,Eb,Tc,fc,kB,TransRate,OverallLargestIslandSize,AverageLargestIslandSize,TotSimuCycle,AtomsAddedOverTime, TempIncreaseOverTime, OutputWhenAtomsMoved,AverageNumberOfIsland,AverageNumberOfScatterIsland,AvgHopCount,AvgT)



end 