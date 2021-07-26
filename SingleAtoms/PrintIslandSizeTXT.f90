subroutine PrintIslandSizeTXT(AtomsAddedInt,IslandSize,LargestIslandSize,IslandSizeData,NoOfAtoms,TotSimuCycle,SimuCycle )
    implicit none
    integer :: AtomsAddedInt,n,k,i,LargestIslandSize,NoOfAtoms,TotSimuCycle,SimuCycle 
    character(len=100):: filename
    integer, dimension(NoOfAtoms) :: IslandSize
    integer, dimension(LargestIslandSize) :: IslandSizeData



do n=1, LargestIslandSize
    IslandSizeData(n)=0
end do

do n=1, LargestIslandSize
    do k=1, AtomsAddedInt

        if (IslandSize(k)==n)then
            IslandSizeData(n)=IslandSizeData(n)+1
        end if

    end do
end do



write(filename,fmt="(A31,I2,A17)") "./SimulationResult/Simiulation",SimuCycle,"/SizesRawData.txt"
open(unit=90,file=filename,status="new")
do i=1, LargestIslandSize
write(90,'(100I5)') i, IslandSizeData(i)
end do

end