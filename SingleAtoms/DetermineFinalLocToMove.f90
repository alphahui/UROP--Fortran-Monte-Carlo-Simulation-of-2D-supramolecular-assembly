subroutine DetermineFinalLocToMove(RandomTrans,NeigbourTrans,SpaceTag,x,y)
    implicit none
    integer :: n,TagChosen,i,j,x,y
    integer (kind=4):: TagUsed(1)
    real SpaceTag
    real (kind=4) :: RandomTrans,r,CurrentTrans
    real, dimension(7,3) :: NeigbourTrans    

    do n=1,SpaceTag
        TagUsed(n)=0
    end do
   CurrentTrans=0



    1000 If (CurrentTrans<RandomTrans) then
    !Choose which empty location to use
    call random_seed
    call random_number(r)
    r=(r*SpaceTag)+1
    TagChosen= int(r)
    
        If (TagUsed(TagChosen)==0) then
            i=NeigbourTrans(TagChosen,1)
            j=NeigbourTrans(TagChosen,2)
            CurrentTrans=CurrentTrans+ NeigbourTrans(TagChosen,3)
            TagUsed(TagChosen)=1

            go to 1000
        else 
            go to 1000
        end if 
    else
        i=NeigbourTrans(TagChosen,1)
        j=NeigbourTrans(TagChosen,2)

        x=i
        y=j

        go to 2000
    end if
    

2000 end