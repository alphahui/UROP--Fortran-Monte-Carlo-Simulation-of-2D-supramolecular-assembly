subroutine UpdateCurrentLoc(BasePlane,Side,MoveX,MoveY,AtomsTagTransRate,TransRate,TotalTrans,NoOfAtoms)
    implicit none
    integer :: MoveX,MoveY,Side,i,j,n,NoOfAtoms,tag,nb
    integer, dimension(Side,Side,3) :: BasePlane 
    real, dimension(NoOfAtoms,2) :: AtomsTagTransRate 
    integer :: ArrayOfX(6)=(/-1,-1,+0,+1,+1,+0/)    !to get x coordinate of neighbour atoms
    integer :: ArrayOfY(6)=(/+0,+1,+1,+0,-1,-1/)    !to get y coordinate of neighbour atoms
    real(kind=4):: TransRate(7)
    real :: TotalTrans

  
    do n=1,6
        i=ArrayOfX(n)+MoveX
        j=ArrayOfY(n)+MoveY
        i=Output(i,Side)
        j=Output(j,Side)

        if(BasePlane(i,j,1)==1)then
            tag=BasePlane(i,j,2)
            
            AtomsTagTransRate(tag,1)=AtomsTagTransRate(tag,1) - 1
            TotalTrans= TotalTrans - AtomsTagTransRate(tag,2)
            
            nb= AtomsTagTransRate(tag,1)
            AtomsTagTransRate(tag,2)=TransRate(nb)*(7 - nb)
            TotalTrans = TotalTrans + AtomsTagTransRate(tag,2)
            
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