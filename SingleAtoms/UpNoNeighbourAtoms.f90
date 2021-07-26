!this update the number of neighbour atoms of each atom for determining the individual transition rate and total
subroutine UpNoNeiAtoms(Side,BasePlane,i,j,TransRate,MovedOrNot,NeigbourLocation,TagLocation,nb)  
implicit none
    integer :: i,j, p,q,Side
    integer(kind=4) ::n,nb,TagLocation
    integer, dimension(Side,Side,3) :: BasePlane     
    integer :: ArrayOfX(6)=(/-1,-1,+0,+1,+1,+0/)    !to get x coordinate of neighbour atoms
    integer :: ArrayOfY(6)=(/+0,+1,+1,+0,-1,-1/)    !to get y coordinate of neighbour atoms
    real(kind=4):: r
    real (kind=4), dimension(7) :: TransRate
    logical :: MovedOrNot
    real, dimension(7,2) :: NeigbourLocation  
    
    call RANDOM_SEED
!------start loop--------
    nb=1
    MovedOrNot=.false.
    TagLocation=0
        !determine whether there is an atoms
            !looping neighbour atoms
            do n=1, 6
                !get the coordinate of neighbour slot/ atom
                p= i+ArrayOfX(n)
                q= j+ArrayOfY(n)
                p=Output(p,Side)
                q=Output(q,Side)
                !print*, Output(p,Side),Output(q,Side)
                    !determine if there is a neighbour atom and add no bonding to slot 2
                    if (BasePlane(p,q,1)==1) then     !p,q neighbour coordinate
                        nb=nb+1                        !update total bonding
                        !print*, "yes", nb, TotalBonding
                    else if (BasePlane(p,q,1)==0) then
                        TagLocation=TagLocation+1
                        NeigbourLocation(TagLocation,1)=p  
                        NeigbourLocation(TagLocation,2)=q  
                        !print*,"NeigbourLocation",p,q
                    end if          
            end do
            
            !update transition rate of the atom and total trans rate
            !print*, 'nb',nb
            !print*, BasePlane(i,j,3), TotalTrans

            call random_number(r)
            !print*, 'r', r,"TransRate(nb)",TransRate(nb)

            if (r<TransRate(nb))then
                MovedOrNot=.true.
            else if (r>TransRate(nb)) then
                MovedOrNot=.false.
            end if 
            !print*,"MovedOrNot",MovedOrNot
!function for calculating boundary
contains
function Output(x, Side)
integer x,Output,Side

if(x<1) Output=x+Side
if(x>=1 .and. x<=Side) Output=x
if(x>Side) Output=x-Side

end function
    
    
    
    
end
    
    
