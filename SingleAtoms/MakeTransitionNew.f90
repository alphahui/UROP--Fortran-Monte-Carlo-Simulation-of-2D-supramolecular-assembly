subroutine MakeTransitionNew(BasePlane,Side,AtomsTagTransRate,MoveX,MoveY,TotalTrans  )
    implicit none
    integer :: Side,n,i,j,nb,tag,p,q,MoveX,MoveY,NeigTag
    integer, dimension(Side,Side,3) :: BasePlane 
    real, dimension(NoOfAtoms,2) :: AtomsTagTransRate 
    real :: TotalTrans
    integer :: ArrayOfX(6)=(/-1,-1,+0,+1,+1,+0/)    !to get x coordinate of neighbour atoms
    integer :: ArrayOfY(6)=(/+0,+1,+1,+0,-1,-1/)    !to get y coordinate of neighbour atoms
    integer :: 
    TotalTrans=TotalTrans-AtomsTagTransRate(BasePlane(MoveX,MoveY,2),3)
    nb=AtomsTagTransRate(BasePlane(MoveX,MoveY,2),2)
    NeigTag=1

    do n=1,6
        i=ArrayOfX(n)+MoveX
        j=ArrayOfY(n)+MoveY
        i=Output(Side,i)
        j=Output(Side,j)
        if (nb==6)then
            if (BasePlane(i,j,1)==0)then
            end if
        else
            
        end if


    end do


        contains
function Output(x, Side)
integer x,Output,Side

if(x<1) Output=x+Side
if(x>=1 .and. x<=Side) Output=x
if(x>Side) Output=x-Side

end function
    end