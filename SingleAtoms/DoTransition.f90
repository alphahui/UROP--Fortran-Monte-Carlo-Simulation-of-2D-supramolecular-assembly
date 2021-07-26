subroutine DoTransition(BasePlane,TransRate,Side,AtomsAddedInt,AtomsMoved)
    implicit none
    integer :: i,j,Side
    integer :: AtomsAddedInt,x,y,m
    integer, dimension(Side,Side,3) :: BasePlane    
    integer, dimension(Side,Side) ::MovedTag
    real (kind=4) :: r,RandomLoc
    real (kind=4), dimension(7) :: TransRate
    logical :: MovedOrNot
    real, dimension(7,2) :: NeigbourLocation  
    integer(kind=4) ::n,nb,TagLocation
    logical :: AtomsMoved
    
    call RANDOM_SEED
    AtomsMoved = .false.
    do i=1,Side
        do j=1,Side
            MovedTag(i,j)=0
        end do
    end do
    
    
    do n=1, AtomsAddedInt
        
    1000 call random_number(r)
            r=r*Side+1
            i=int(r)
        call random_number(r)
            r=r*Side+1
            j=int(r) 
            
            if (MovedTag(i,j)==0) then
                if(BasePlane(i,j,1)==1) then
                   
                    call UpNoNeiAtoms(Side,BasePlane,i,j,TransRate,MovedOrNot,NeigbourLocation,TagLocation,nb)   
                !get info on the atoms neigbour number of atoms and space and neigbour trans rate
                    
                    if (MovedOrNot==.true.)then
                        !print*, "checking atom"
                        !print*,'nb',nb,"TagLocation",TagLocation
                        if (nb==7) then 
                        MovedTag(i,j)=1
                    
                         else if (nb==6) then 
                        BasePlane(i,j,1)=0      !empty the atom location
                        NeigbourLocation(1,1)=i
                        NeigbourLocation(1,2)=j    !get x,y coordinate of the only empty spot
                        BasePlane(i,j,1)=1      !move the atom to the empty spot
                        MovedTag(i,j)=1
                        AtomsMoved = .true.
                        else 
                    
                        call random_number(r)
                        RandomLoc=int(r*TagLocation)+1
                        !print*, "RandomLoc",RandomLoc
                        !print*,'atoms to be moved',i,j
                        BasePlane(i,j,1)=0      !empty the atom location
                        i=NeigbourLocation(RandomLoc,1)
                        j=NeigbourLocation(RandomLoc,2)    !get x,y coordinate of the only empty spot
                        BasePlane(i,j,1)=1      !move the atom to the empty spot
                        MovedTag(i,j)=1
                        AtomsMoved = .true.
                        end if
                    else
                        MovedTag(i,j)=1 
                    end if
                else
                    goto 1000
                end if
            else 
                goto 1000
            end if
               !print*, "atoms",n,'moved'
    end do
    
    !print*, 'end of transition'
    
    end
