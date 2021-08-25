subroutine DetermineWhetherStop(CurrentIslandData,PreviousIslandData,PercentageIslandSize,PercentageIslandNumber,PercentageScatterNumber,EndSimulation)
    implicit none
    integer :: n
    real::CurrentIslandData(3),PreviousIslandData(3),PercentageDifference(3) 
    real(kind=4)::PercentageIslandSize,PercentageIslandNumber,PercentageScatterNumber
    logical :: EndSimulation

    do n=1,3
        PercentageDifference(n) = (CurrentIslandData(n)-PreviousIslandData(n))/PreviousIslandData(n)
    end do
    print*,"%IslandSize", PercentageDifference(1),"%IslandNumber", PercentageDifference(2),"%ScatterNumber", PercentageDifference(3)
    if (abs(PercentageDifference(1))<=PercentageIslandSize .and. abs(PercentageDifference(2))<=PercentageIslandNumber .and.  abs(PercentageDifference(3))<=PercentageScatterNumber)then
        EndSimulation=.true.
    end if

    end
