subroutine PrintIslandSizeTXT(AtomsAddedInt,IslandSize,LargestIslandSize,IslandSizeData,NoOfAtoms,TotSimuCycle,SimuCycle,TimeInterval,LargestIslandSizeCycle,NumberIslandCycle,NumberScatterCycle,MaxTime,t)
    implicit none
  
    integer :: AtomsAddedInt,n,k,i,LargestIslandSize,NoOfAtoms,TotSimuCycle,SimuCycle,MaxTime,TimeInterval,Time,t 
    integer,dimension(MaxTime)::LargestIslandSizeCycle,NumberIslandCycle,NumberScatterCycle
    character(len=200) filename
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

write(filename,fmt='(a,I0,A)') './Simulation_Result/Simulation_', SimuCycle,'/SizesRawData.txt'
open(unit=900,file=filename)

do i=1, LargestIslandSize
write(900,'(100I10)') i, IslandSizeData(i)
end do

write(filename,fmt='(a,I0,A)') './Simulation_Result/Simulation_', SimuCycle,'/LargestIslandSizeData.txt'
open(unit=800,file=filename)
Time=1
801 write(800,'(100I10)') Time, LargestIslandSizeCycle(Time)

if(Time<t)then
    Time=Time+TimeInterval
    goto 801
end if

write(filename,fmt='(a,I0,A)') './Simulation_Result/Simulation_', SimuCycle,'/NumberOfIslandData.txt'
open(unit=700,file=filename)
Time=1
701 write(700,'(100I10)') Time,NumberIslandCycle(Time)

if(Time<t)then
    Time=Time+TimeInterval
    goto 701
end if

write(filename,fmt='(a,I0,A)') './Simulation_Result/Simulation_', SimuCycle,'/NumberOfScatterData.txt'
open(unit=600,file=filename)
Time=1
601 write(600,'(100I10)') Time,NumberScatterCycle(Time)

if(Time<t)then
    Time=Time+TimeInterval
    goto 601
end if

end