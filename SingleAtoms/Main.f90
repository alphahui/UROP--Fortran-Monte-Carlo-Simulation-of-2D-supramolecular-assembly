program Main
use parameters

implicit none

    character(len=100)  filename                                      !set file name
    
    integer, dimension(Side,Side,3) :: BasePlane                    !initial plane of each time (1:0=empty 1=atom)
    integer :: i,j,n,nb,LargestIslandSize,OverallLargestIslandSize  !i,j: coordinate of plane n: dummy nb: number of bonding                                                                     !LargestIslandSize: largest island size in 1 simulation OverallLargestIslandSize: LargestIslandSize but after all simulation
    integer :: input,AtomsAddedInt,TotalBonding,TimeMove                     !input :record input, int of number of atoms added
    integer :: t, SimuCycle, TotSimuCycle                              !t :cycle passed Simucycle: record current round of simulation  TotSimucycle : record the number of simulation wanted to do 
    integer :: ScatterIslands,NoOfIsland
    real(kind=16) :: r,Temp                                         !r :to store random number generated Temp : for temperature
    real(kind=8) :: AtomsAdded,AverageLargestIslandSize,AverageNumberOfIsland,AverageNumberOfScatterIsland                         
    real(kind=4):: TransRate(7)
    integer:: NextTxtPrinted                                        !NextTxtPrinted :to get the next cycle which prints the plane
    integer, dimension(NoOfAtoms) :: IslandSize                    !IslandSize: number of islands vary in size for 1 simulation before final process (not all islands are catergorzied)
    logical :: AtomsMoved                                           !AtomsMoved: record rather atoms are moved in the cycle
    integer, dimension(NoOfAtoms) :: IslandSizeData                 !IslandSizeData: proccesed IslandSize Data 
    real, dimension(NoOfAtoms) :: FinalIslandSize                   !FinalIslandSize: IslandSize for all simulation combined
    integer:: TempRecordMovedCycle(MaxTime)
    
    call RANDOM_SEED  
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

500 print*, "Do you want the temperature to be increased by",TempIncPerCycle ,"per cycle,which can be edited in parameters.F90? (1 for yes, 2 for no and the temperature will be", Tc,")"
600 read*,  input
    If  (input ==1) then 
        TempIncreaseOverTime=.true. 
        goto 100
    else if (input ==2) then
        TempIncreaseOverTime=.false.
        goto 100
    else
        print*, "please enter a valid value. (1 for yes, 2 for no and the temperature will be", Tc,")"
        goto 600
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
do n=1, MaxTime
   TempRecordMovedCycle=0
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

    t=1
    NextTxtPrinted=1   
    AtomsAdded =0
    Temp=Tc
    AtomsAddedInt=0
    TimeMove=1
    AtomsMoved=.false.
    do nb=1, 7
        TransRate(nb)=fc*exp(-(Ed+(nb-1)*Eb)/(kB*Tc))
    end do 
    
    
write(filename,'(a,i0)') "mkdir Simulation_result\Simulation_",SimuCycle
call system(filename)
    !Basic info of the simulation
    write(filename,"(a,i0,a)") "./Simulation_result/Simulation_",SimuCycle,"/Simulation.txt"
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

    do nb=1, 7
    print*, TransRate(nb)
    end do 

   
    10 if (t<= MaxTime) then !determine whether we have passed cycle limit
    !Calculate transition rate of different number of bonding

    !possibility to move
    !Add Atoms  
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
                    !write(*,*) "3"
                    goto 2   
                end if 
            end do
            AtomsAddedInt= AtomsAdded
        
        else if  (AtomsAddedOverTime == .true.) then  !Atoms added one by one
            if (AtomsAdded < NoOfAtoms ) then
        !Add atoms per cycle
                !print*, 'atom added per cycle',AtomsAddedInt
                call AddAtoms(Side,AtomsAdded,AtomsAddedPerCycle,AtomsAddedInt,BasePlane,NoOfAtoms)
    
            else 
            end if 
        end if    !done adding all atoms
    
    !Get number of bonding of each atom ,total bonding, individual transrate and total transrate


    !Get the time to start the transition
        

    !do transisiton
    if (t >= 1) then
    
    if (AtomsAddedInt>0) then
    
    call DoTransition(BasePlane,TransRate,Side,AtomsAddedInt,AtomsMoved)
    else
    end if
    end if 

  

    !------------------output------------------
    !create txt
        
    if (OutputWhenAtomsMoved == .true.) then
      
    if (t==MaxTime .OR. t == 1) then

        call PrintLog(AtomsAddedInt,AtomsAdded,Temp,t,SimuCycle)
        call PrintPlaneTXT(Side,BasePlane,filename,t,SimuCycle)
    end if
    if (AtomsMoved == .true.) then

        call PrintLog(AtomsAddedInt,AtomsAdded,Temp,t,SimuCycle)
        call PrintPlaneTXT(Side,BasePlane,filename,t,SimuCycle)
           
        TempRecordMovedCycle(TimeMove)= t
        print*,"moved", TimeMove,TempRecordMovedCycle(TimeMove),t
        TimeMove =TimeMove+1
        
    end if 

    end if 
    !call  PrintLog(AtomsAddedInt,AtomsAdded,Temp,t)
        print"(a,i0,a,i0,a)", "simulation= ",SimuCycle,'  cycle= ',t,'  rendered'
    


    if (OutputWhenAtomsMoved==.false.) then
    if (t==NextTxtPrinted) then

        
        NextTxtPrinted=NextTxtPrinted+TimeInterval
        call PrintPlaneTXT(Side,BasePlane,filename,t,SimuCycle)

    end if 
    end if




    !Add time   
    t=t+1

    goto 10 !end of cycle go back to start
    end if

    !-------------------end of 1 Simulation--------------------------------
    print*, "end of simulation",SimuCycle

    call FindNoOfIslands(Side,BasePlane,AtomsAddedInt,SizeAsIsland,IslandSize,LargestIslandSize,SimuCycle,ScatterIslands,NoOfIsland)
    call PrintIslandSizeTXT(AtomsAddedInt,IslandSize,LargestIslandSize,IslandSizeData,NoOfAtoms,TotSimuCycle,SimuCycle)
    call RecordSimulationData(LargestIslandSize,IslandSizeData,NoOfAtoms,SimuCycle,FinalIslandSize)
    
    if(OutputWhenAtomsMoved==.true.) then
        call PrintMoveTimeRawData(TempRecordMovedCycle,MaxTime,t,SimuCycle,TimeMove)
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
    SimuCycle = SimuCycle +1


    goto 101 !return to initialize new simulation

end if 

!------------------ end of all simulation -------------

call PrintFinalSizesRawData(OverallLargestIslandSize,FinalIslandSize,NoOfAtoms)
call PrintFinalSimulationLog(Side,NoOfAtoms,SizeAsIsland,TimeInterval,MaxTime,AtomsAddedPerCycle,TempIncPerCycle,Ed,Eb,Tc,fc,kB,TransRate,OverallLargestIslandSize,AverageLargestIslandSize,TotSimuCycle,AtomsAddedOverTime, TempIncreaseOverTime, OutputWhenAtomsMoved,AverageNumberOfIsland,AverageNumberOfScatterIsland  )

end 