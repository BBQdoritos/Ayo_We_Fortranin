module dynllist

! https://www.physicstom.com/fortranlinkedlist/

    implicit none
    public :: Node, append, printList

    ! Define a node type for the linked list
    type :: Node
        integer :: data
        ! type(node), pointer :: previous => null()
        type(Node), pointer :: next => null() 
    end type Node

    contains

    ! just make it so that a singly linked list works, if you need a doubly, do so later

    function createNode(value) result (newNode)
        integer, intent(in) :: value
        type(Node), pointer :: newNode
        allocate(newNode)
        newNode%data = value
        newNode%next => null()
    end function createNode


    subroutine append(head, value)
        type(Node), pointer :: head, current, newNode
        integer, intent(in) :: value

        newNode => createNode(value) !should create a new node

        if (.not. associated(head)) then !if no available head given from the parameter, make one
            head => newNode
        else 
            current => head

            do while (associated(current%next))
                current => current%next
            end do

            current%next => newNode
        end if
    end subroutine append


    ! subroutine cleanUp (linkedList)
    !     type(node), intent(inout) :: linkedList

    !     if (associated(linkedList%data)) then
    !         deallocate(linkedList%data)



    ! end subroutine cleanUp

    subroutine printList(head)
        type(Node), pointer :: head   ! Head pointer of the list
        type(Node), pointer :: current

        current => head
        do while (associated(current))
            print *, current%data
            current => current%next
        end do
    end subroutine printList



! https://stackoverflow.com/questions/2389904/total-size-of-a-linked-list-in-c
    function sizeOfList(head) result (size)
        type(Node), pointer :: head, current
        integer :: size

        size = 0

        current => head
        do while (associated(current))
            size = size + 1
            current => current%next
        end do
    end function sizeOfList

    subroutine paddingZeros(head)
        type(Node), pointer :: head, current, newNode
        integer :: value

        value = 0 !we pad with 0s 

        newNode => createNode(value) !should create a new node

        if (.not. associated(head)) then !if no available head given from the parameter, make one
            head => newNode
        else 
            current => head

            do while (associated(current%next))
                current => current%next
            end do

            current%next => newNode
        end if
    end subroutine paddingZeros



    subroutine addition(header1, header2, result)
        !all lists are already initialized
        type(Node), pointer, intent(in) :: header1, header2
        type(Node), pointer, intent(out) :: result  

        type(Node), pointer :: current1, current2

        integer :: value1, value2, overflow, sum, digit

        overflow = 0

        current1 => header1
        current2 => header2

        do while (associated(current1) .and. associated(current2))
                
            value1 = current1%data
            current1 => current1%next
            value2 = current2%data
            current2 => current2%next

            sum = value1 + value2 + overflow
            digit = mod(sum, 10)
            overflow = sum / 10 

            call append(result, sum)
        end do

        if (overflow > 0) then
            call append(result, overflow)
        end if

    end subroutine addition


end module dynllist