subroutine PrintMoveTimeRawData(TempRecordMovedCycle,CycleRecord,t,SimuCycle,TimeMove)
    implicit none
   
    integer :: t, SimuCycle,j,CycleRecord,TimeMove,n
    integer, dimension(CycleRecord):: TempRecordMovedCycle

    character(len=200) file
    integer,allocatable:: RecordMovedCycle(:)
    integer,allocatable:: RecordMovedIntervalCycle(:)

    TimeMove=TimeMove-1
    allocate(RecordMovedCycle(TimeMove))

    do n=1, TimeMove
        RecordMovedCycle(n)=TempRecordMovedCycle(n)
    end do
    
    write(file,'(a,i0,a)') "./Simulation_Result/Simulation_",SimuCycle,"/CycleAtomsMoved.txt"

    open(unit=90,file=file)

    do j=1, TimeMove
    write(90,'(5i20)') j, RecordMovedCycle(j)
    end do

    TimeMove=TimeMove-1
    allocate(RecordMovedIntervalCycle(TimeMove))

    do n=1,TimeMove
        RecordMovedIntervalCycle(n)=RecordMovedCycle(n+1)-RecordMovedCycle(n)
    end do

    write(file,'(a,i0,a)') "./Simulation_result/Simulation_",SimuCycle,"/CycleAtomsMovedInterval.txt"

    open(unit=100,file=file)

    do j=1, TimeMove
    write(100,'(5i20)') j, RecordMovedIntervalCycle(j)
    end do

    end