subroutine FindAtomToBeMoved(BasePlane,Side,AtomsTagTransRate,TransToMeet,MoveX,MoveY,NoOfAtoms)
    implicit none
    integer :: Side,n,i,j,nb,tag,p,q,MoveX,MoveY,NoOfAtoms
    integer, dimension(Side,Side,3) :: BasePlane 
    real, dimension(NoOfAtoms,2) :: AtomsTagTransRate 
    real(kind=8) :: TransToMeet, SumCurrentTrans

    SumCurrentTrans=0
    
    do i =1, Side
        do j=1 , Side

            If (BasePlane(i,j,1)==1)then

                SumCurrentTrans=SumCurrentTrans+AtomsTagTransRate(BasePlane(i,j,2),2)
            
            end if

            If (SumCurrentTrans >=TransToMeet)then

                MoveX = i
                MoveY = j
                TransToMeet= TransToMeet -( SumCurrentTrans-AtomsTagTransRate(BasePlane(i,j,2),2) ) 
                
                goto 1
            end if
            
        end do
    end do
    
    1 end