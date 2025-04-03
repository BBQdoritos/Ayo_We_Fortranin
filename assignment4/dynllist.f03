module dynllist

    implicit none

    ! Define a node type for the linked list
    type :: node
        character(len = 1) :: data
        type(node), pointer :: next 
    end type node

    contains




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