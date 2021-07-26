subroutine AddAtoms(Side,AtomsAdded,AtomsAddedPerCycle,AtomsAddedInt,BasePlane,NoOfAtoms)
implicit none
    integer :: i,j,NoOfAtoms 
    integer :: Side
    integer, dimension(Side,Side,3) :: BasePlane 
    Integer :: AtomsAddedInt
    real(kind=4) :: Random
    real ::AtomsAdded, AtomsAddedPerCycle, AtomsAfterAdded, AtomsToAdd
    
   !-------------------------------------------------------
call RANDOM_SEED
!call PrintArrayTest(BasePlane, Side)
!print*, 'atom in added per cycle',AtomsAddedInt
!determine whether we have exceed atoms limit 
    if (AtomsAdded + AtomsAddedPerCycle <=NoOfAtoms) then
        AtomsAfterAdded = AtomsAdded + AtomsAddedPerCycle
    else if (AtomsAdded + AtomsAddedPerCycle > NoOfAtoms) then
        AtomsAfterAdded = NoOfAtoms
    else
    end if
!--------------start adding atoms-----------
    
   do while(AtomsAdded<AtomsAfterAdded) 
       
    AtomsToAdd=AtomsAfterAdded - AtomsAddedInt !calculate number of atoms to add
    
        If (AtomsToAdd < 1) then !no atom to add 
        AtomsAdded = AtomsAfterAdded
        else if (AtomsToAdd >= 1) then !there is atoms to add
         !get random coordinate to add atom
            2 call random_number(Random)
            Random=Random*Side+1
            i=int(Random) 
            call random_number(Random)
            Random=Random*Side
            j=int(Random)+1 
            
        !check rather location have atom or not
            if (i <= Side .AND. i >=1 .AND. j <= Side .AND. j >=1) then
                if (BasePlane(i,j,1) == 0) then
                     !print*, i,j,"chosen"
                    BasePlane(i,j,1) = 1
                    !print*, "addatom"
                else
                    goto 2
                end if
            else 
                goto 2   
            end if 
        !update number of atoms added    
        AtomsAddedInt = AtomsAddedInt +1
        AtomsAdded = AtomsAdded +1
        else
        end if
    
   end do
!-------------end of adding atoms-----------------
   
 !update number of atoms added  
   AtomsAdded= AtomsAfterAdded

   !print*, "atoms added", AtomsAddedInt
end