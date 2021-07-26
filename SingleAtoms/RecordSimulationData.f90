subroutine RecordSimulationData(LargestIslandSize,IslandSizeData,NoOfAtoms,SimuCycle,FinalIslandSize)
    integer :: n,k,i,LargestIslandSize,NoOfAtoms,SimuCycle
    integer, dimension(NoOfAtoms) :: IslandSize
    integer, dimension(LargestIslandSize) :: IslandSizeData
    real, dimension(NoOfAtoms) :: FinalIslandSize

    do n =1, LargestIslandSize
        FinalIslandSize(n) = ( IslandSizeData(n) +FinalIslandSize(n)*(SimuCycle-1) )/SimuCycle

        if (FinalIslandSize(n) .NE. 0) then
            print*, n,FinalIslandSize(n)
        end if 

    end do
    print*, 'size exceed current simulation data'
    do n=LargestIslandSize+1, NoOfAtoms
        FinalIslandSize(n) = (FinalIslandSize(n)*(SimuCycle-1))/SimuCycle

        if (FinalIslandSize(n) .NE. 0) then
            print*, n,FinalIslandSize(n)
        end if 

    end do

    end