subroutine PrintFinalSizesRawData(OverallLargestIslandSize,FinalIslandSize,NoOfAtoms)
    implicit none
  
    character(len=200) Filename
    integer :: i,OverallLargestIslandSize,NoOfAtoms
    real :: j
    real, dimension(NoOfAtoms) :: FinalIslandSize
    write(Filename,fmt="(a)") "./Simulation_Result/FinalSizesRawData.txt"
    open(unit=90,file= Filename)

    do j=1, OverallLargestIslandSize
    write(90,'(2F10.3)') j, FinalIslandSize(j)
    end do

    end