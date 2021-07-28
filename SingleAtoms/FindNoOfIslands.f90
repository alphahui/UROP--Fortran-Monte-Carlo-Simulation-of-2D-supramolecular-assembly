subroutine FindNoOfIslands(Side,BasePlane,AtomsAddedInt,SizeAsIsland,IslandSize,LargestIslandSize,SimuCycle)
    implicit none 
    character(len=100) filename
    integer :: i,j, p,q,x,y,m,Side,AtomsAddedInt,NoIsland,n,TimesToAdd,TagToAdd,SimuCycle
    integer :: TagToCheck,k,FinalTag,LargestIslandSize,NoOfIsland,SizeAsIsland,ScatterIslands
    integer, dimension(Side,Side,3) :: BasePlane 
    integer, dimension(Side,Side,1) :: IslandTag
    integer, dimension(AtomsAddedInt) :: IslandSize
    integer :: ArrayOfX(6)=(/-1,-1,+0,+1,+1,+0/)    
    integer :: ArrayOfY(6)=(/+0,+1,+1,+0,-1,-1/) 
    logical :: IfNeiAtomTagged,IfTagIsEmpty

    NoIsland = 0
    do i=1,Side
        do j=1,Side
            IslandTag(i,j,1)=0
        end do
    end do


!call PrintArrayTest(BasePlane, Side)
 !Initiate Tag to island
    do i=1,Side
        do j=1,Side
            !Find atom on plane
            if (BasePlane(i,j,1)==1)then
                TagToAdd=0
                !Check neighbour
                !print*, "Atoms Checking",i,j
                do n=1,6
                    p=i+ArrayOfX(n)
                    q=j+ArrayOfY(n)
                    p=Output(p, Side)
                    q=Output(q, Side)

                    if (BasePlane(p,q,1)==1)then
                        !See if the neigbour is tagged
                        If (IslandTag(p,q,1) .NE. 0 ) then
                            !See if there is a tag already
                            If (TagToAdd ==0)then
                                    TagToAdd = IslandTag(p,q,1)
                                    !print*, "First Tag", TagToAdd
                            else if (TagToAdd .NE. 0) then
                                !Change Tag
                                if (TagToAdd .GE. IslandTag(p,q,1)) then
                                    TagToAdd = IslandTag(p,q,1)
                                    !print*, "New Tag", TagToAdd
                                end if 

                            end if 
                        
                        end if 
                        
                    end if 

                end do

                !Decide the tag to add
                if (TagToAdd .NE. 0) then
                    IslandTag(i,j,1)=TagToAdd
                    !print*,"due to neigbour",TagToAdd
                else if (TagToAdd ==0)then
                    NoIsland=NoIsland+1
                    IslandTag(i,j,1)=NoIsland
                    !print*,"new tag made",NoIsland
                end if

                
            end if 
        end do
    end do 


!print*, "----------serach through all tag-----------"
!Search through all island tags
do m=1,NoIsland
    !Search through the plane for Side times to ensure all neigbour checked
    !print*,"Current tag checking",m
    do k=1,Side

        !print*,"The cycle checking",k
        do i=1,Side
            do j=1,Side
                
                if (BasePlane(i,j,1)==1)then 
                    if (IslandTag(i,j,1) == m) then
                        !print*,'Atom in check with tag',i,j
                        do n=1,6
                            p=i+ArrayOfX(n)
                            q=j+ArrayOfY(n)
                            p=Output(p, Side)
                            q=Output(q, Side)
        
                            if (BasePlane(p,q,1)==1) then
                                !See if the neigbour is tagged
                                !print*, "Neigbour checking",p,q,IslandTag(p,q,1)

                                If (IslandTag(p,q,1) .GE. m ) then
                                    IslandTag(p,q,1) = m
                                    !print*, "Atom changed",p,q,IslandTag(p,q,1)
                                end if 
                                
                            end if 
        
                        end do

                    end if 
                end if 
    
            end do
        end do

    end do

end do

!Finalize  island size 
FinalTag =1
IslandSize(1)=0
do k=1,NoIsland

    IfTagIsEmpty=.true.

    do i=1,Side
        do j=1,Side

                if (BasePlane(i,j,1)==1) then
                    if (IslandTag(i,j,1)==k)then

                        IfTagIsEmpty=.false.
                        IslandSize(FinalTag)=IslandSize(FinalTag)+1

                    end if
                end if 
               
        end do
    end do

    If (IfTagIsEmpty==.false.) then
    
    FinalTag= FinalTag+1
    IslandSize(FinalTag)=0
    end if 
end do

!Conclude largest island and number of island 
NoOfIsland=0
LargestIslandSize=0
FinalTag=FinalTag -1
do n=1, FinalTag

    if (IslandSize(n) .GE. SizeAsIsland )then
        NoOfIsland=NoOfIsland+1

        if (LargestIslandSize .LE. IslandSize(n) ) then
            LargestIslandSize=IslandSize(n)
        end if 
         
    end if 

end do
ScatterIslands = FinalTag-NoOfIsland
write(filename,"(a,i0,a)") "./Simulation_result/Simulation_",SimuCycle,"/Simulation.txt"
open(900,file=filename)
write(900,*) "-----Islands Result-----"
write(900,*) 'Number of Islands=',NoOfIsland,"Largest Island",LargestIslandSize,"Atoms"
write(900,*) 'Number of Scatter Islands', ScatterIslands

    contains
function Output(x, Side)
integer x,Output,Side

if(x<1) Output=x+Side
if(x>=1 .and. x<=Side) Output=x
if(x>Side) Output=x-Side

end function
    

end