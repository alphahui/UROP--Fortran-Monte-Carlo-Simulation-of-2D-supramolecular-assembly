subroutine RecordSimulationData(LargestIslandSize,IslandSizeData,NoOfAtoms,SimuCycle,FinalIslandSize)
    integer :: n,k,i,LargestIslandSize,NoOfAtoms,SimuCycle
    integer, dimension(NoOfAtoms) :: IslandSize
    integer, dimension(LargestIslandSize) :: IslandSizeData
    real, dimension(NoOfAtoms) :: FinalIslandSize

    do n =1, LargestIslandSize
        FinalIslandSize(n) = ( IslandSizeData(n) +FinalIslandSize(n)*(SimuCycle-1) )/SimuCycle

    end do

    do n=LargestIslandSize+1, NoOfAtoms
        FinalIslandSize(n) = (FinalIslandSize(n)*(SimuCycle-1))/SimuCycle


    end do

    end