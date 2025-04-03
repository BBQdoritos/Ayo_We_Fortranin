module dynllist

    implicit none

    ! Define a node type for the linked list
    type :: node
        character(len = 1) :: data
        ! type(node), pointer :: previous => null()
        type(node), pointer :: next => null() 
    end type node

    contains

    ! just make it so that a singly linked list works, if you need a doubly, do so later

! this would need the tail and the node to add 
    subroutine append (linkedList, value)
        class(linkedList), intent(inout) :: linkedList
        character, intent(in) :: value

        type(node), pointer :: node_ptr
        typ
    

    end subroutine append



    subroutine cleanUp (linkedList)
        type(node), intent(inout) :: linkedList

        if (associated(linkedList%data)) then
            deallocate(linkedList%data)



    end subroutine cleanUp

    subroutine print_list(head)
        type(node), pointer :: head   ! Head pointer of the list
        type(node), pointer :: current

        current => head
        do while (associated(current))
            print *, current%data
            current => current%next
        end do
    end subroutine print_list





end module dynllist