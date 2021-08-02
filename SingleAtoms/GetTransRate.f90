subroutine GetTransRate(Side,BasePlane,TransRate,AtomsTagTransRate,TotalTrans )
    implicit none
    integer :: Side,n,i,j,nb,tag,p,q
    integer, dimension(Side,Side,3) :: BasePlane     
    integer :: ArrayOfX(6)=(/-1,-1,+0,+1,+1,+0/)    !to get x coordinate of neighbour atoms
    integer :: ArrayOfY(6)=(/+0,+1,+1,+0,-1,-1/)    !to get y coordinate of neighbour atoms
    real (kind=4), dimension(7) :: TransRate
    real, dimension(NoOfAtoms,2) :: AtomsTagTransRate                ! dimension 1: atom tag from BasePlane 2 1: number of bonding 2: trasrate
    real :: TotalTrans

    tag =1
    TotalTrans=0

    do i=1,Side
        do j=1,Side

            If (BasePlane(i,j,1)==1) then
                BasePlane(i,j,2)=tag
                nb =1

                do n=1,6
                    p= i+ArrayOfX(n)
                    q= j+ArrayOfY(n)
                    p=Output(p,Side)
                    q=Output(q,Side)
                    If (BasePlane(p,q,1)==1)then
                        nb=nb+1
                    end if
                end do
                
                AtomsTagTransRate(tag,1)= nb
                AtomsTagTransRate(tag,2)= TransRate(nb)
                TotalTrans = TotalTrans+AtomsTagTransRate(tag,2)
                tag=tag+1

            end if
        
        end do
        
    end do


    contains
    function Output(x, Side)
    integer x,Output,Side
    
    if(x<1) Output=x+Side
    if(x>=1 .and. x<=Side) Output=x
    if(x>Side) Output=x-Side
    
    end function

    end 