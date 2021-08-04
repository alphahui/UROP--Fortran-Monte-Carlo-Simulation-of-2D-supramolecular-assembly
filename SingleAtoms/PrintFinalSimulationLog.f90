subroutine PrintFinalSimulationLog(Side,NoOfAtoms,SizeAsIsland,TimeInterval,MaxTime,AtomsAddedPerCycle,TempIncPerCycle,Ed,Eb,Tc,fc,kB,TransRate,OverallLargestIslandSize,AverageLargestIslandSize,TotSimuCycle,AtomsAddedOverTime, TempIncreaseOverTime, OutputWhenAtomsMoved,AverageNumberOfIsland,AverageNumberOfScatterIsland,AvgHopCount  )
    implicit none
    integer :: Side,NoOfAtoms,SizeAsIsland,TimeInterval,OverallLargestIslandSize,TotSimuCycle,n, MaxTime
    real :: AtomsAddedPerCycle,TempIncPerCycle
    real(kind=8):: Ed,Eb,Tc,fc,kB,AverageLargestIslandSize,AverageNumberOfIsland,AverageNumberOfScatterIsland,AvgHopCount	
    real(kind=4):: TransRate(7)	
    logical :: AtomsAddedOverTime, TempIncreaseOverTime, OutputWhenAtomsMoved


    open(unit=90,file= "./Simulation_result/FinalSimulationLog.txt")
    write(90,*)"-----------Basic parameters---------------"
    write(90,fmt="(a,i0,a,i0,a,i0)") "Side= ",Side,"  NoOfAtoms= ",NoOfAtoms,"  TotSimucycled= ",TotSimucycle
    write(90,fmt="(a,i0,a,i0,a,i0)") "MaxTime= ",MaxTime,"  TimeInterval= ", TimeInterval,"  SizeAsIsland= ",SizeAsIsland
    write(90,*)
    write(90,*)"-----------parameters---------------"
    write(90,*)"Ed=",Ed,"Eb=",Eb
    write(90,*)"Tc=",Tc,"fc=",fc
    write(90,*)"kB=",kB
    write(90,*)"Transition rate:"
    do n=1,7
        write(90,*) n-1 ,"bonding:  ",TransRate(n)
    end do
    write(90,*)
    write(90,*)"-----------settings---------------"
    write(90,*)"AtomsAddedOverTime=",AtomsAddedOverTime,"  TempIncreaseOverTime=",TempIncreaseOverTime,"  OutputWhenAtomsMoved=", OutputWhenAtomsMoved
    write(90,*)"AtomsAddedPerCycle=",AtomsAddedPerCycle,"  TempIncPerCycle=",TempIncPerCycle
    write(90,*)
    write(90,*)"-----------Results---------------"
    write(90,fmt="(a,F0.3,a,i0)")"AverageLargestIslandSize= ",AverageLargestIslandSize,"  OverallLargestIslandSize= ",OverallLargestIslandSize	
    write(90,fmt="(a,F0.3,a,F0.3)")"AverageNumberOfIsland= ",AverageNumberOfIsland,"  AverageNumberOfScatterIsland= ",AverageNumberOfScatterIsland
    write(90,fmt="(a,F0.3)")"AvgHopCount= ",AvgHopCount
    end