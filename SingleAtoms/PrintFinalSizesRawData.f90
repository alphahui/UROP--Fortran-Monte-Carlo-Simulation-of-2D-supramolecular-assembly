subroutine PrintFinalSizesRawData(OverallLargestIslandSize,FinalIslandSize,NoOfAtoms)
    implicit none
    integer :: i,OverallLargestIslandSize,NoOfAtoms
    real :: j
    real, dimension(NoOfAtoms) :: FinalIslandSize

    open(unit=90,file= "FinalSizesRawData.txt")

    do j=1, OverallLargestIslandSize
    write(90,'(2F10.3)') j, FinalIslandSize(j)
    end do

    end