subroutine MakeTransitionNew(BasePlane,Side,AtomsTagTransRate,MoveX,MoveY,TotalTrans,TransToMeet,LocX,LocY,NoOfAtoms)
    implicit none
    integer :: Side,n,i,j,nb,tag,MoveX,MoveY,LocX,LocY,NoOfAtoms
    integer, dimension(Side,Side,3) :: BasePlane 
    real, dimension(NoOfAtoms,2) :: AtomsTagTransRate 
    real :: TotalTrans
    integer :: ArrayOfX(6)=(/-1,-1,+0,+1,+1,+0/)    !to get x coordinate of neighbour atoms
    integer :: ArrayOfY(6)=(/+0,+1,+1,+0,-1,-1/)    !to get y coordinate of neighbour atoms
    real(kind=8) :: TransToMeet
    
    TotalTrans=TotalTrans-AtomsTagTransRate(BasePlane(MoveX,MoveY,2),3)
    tag= BasePlane(MoveX,MoveY,2)
    nb=AtomsTagTransRate(tag,1)

        

    
        if (nb==7)then
            LocX=MoveX
            LocY=MoveY
        else
            do n=1,6
                i=ArrayOfX(n)+MoveX
                j=ArrayOfY(n)+MoveY
                i=Output(i,Side)
                j=Output(j,Side)
               
                if(BasePlane(i,j,1)==0)then
                    TransToMeet =TransToMeet - AtomsTagTransRate(tag,2)/(7-nb)
                     
                    if(TransToMeet<0)then

                        LocX=i
                        LocY=j
                      
                        goto 100

                    end if

                
                end if

            end do
        end if




        contains
function Output(x, Side)
integer x,Output,Side

if(x<1) Output=x+Side
if(x>=1 .and. x<=Side) Output=x
if(x>Side) Output=x-Side

end function
   100 end