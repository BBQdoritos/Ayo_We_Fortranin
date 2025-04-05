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

    subroutine addAtHead(head, value)
        type(Node), pointer :: head, newNode
        integer, intent(in) :: value

        newNode => createNode(value)

        newNode%next =>head
        head => newNode
    end subroutine addAtHead


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
            write(*,'(I0,1x)', advance='no') current%data
            current => current%next
        end do
        ! for newline
        write(*,*)

    end subroutine printList

    recursive subroutine printReverse(head)
        type(Node), pointer :: head

        if (.not. associated(head)) return

        call printReverse(head%next)
        write(*,'(I0,1x)', advance='no') head%data

    end subroutine printReverse



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
        result => null()

        current1 => header1
        current2 => header2

        do while (associated(current1) .or. associated(current2))
                
            if (associated(current1)) then
                value1 = current1%data
                current1 => current1%next
            else
                value1 = 0
            end if

            if (associated(current2)) then
                value2 = current2%data
                current2 => current2%next
            else
                value2 = 0
            end if

            sum = value1 + value2 + overflow
            digit = mod(sum, 10)
            overflow = sum / 10 

            call append(result, digit)
        end do

        if (overflow > 0) then
            call append(result, overflow)
        end if

    end subroutine addition

    ! need so solve the following:
        ! if value2 is larger than value1
        ! could have a helper function that checks which is larger (post padding), checking the most significant digit and swapping the places based on that 
    subroutine subtraction(header1, header2, result, negative)
        type(Node), pointer, intent(in) :: header1, header2
        type(Node), pointer, intent(out) :: result  

        type(Node), pointer :: current1, current2, a, b

        integer :: value1, value2, difference, borrow, comparer
        logical, intent(out) :: negative

        borrow = 0

        comparer = comparison(header1, header2)

        if (comparer >= 0) then
            a =>header1
            b =>header2
            negative = .false.
        else 
            b =>header1
            a =>header2
            negative = .true.
        end if

        current1 => a
        current2 => b


        do while (associated(current1) .and. associated(current2))
                
            value1 = current1%data - borrow
            current1 => current1%next
            value2 = current2%data
            current2 => current2%next


            if (value2 > value1) then 
                value1 = value1 + 10
                borrow = 1
            else 
                borrow = 0
            end if

            difference = value1 - value2
            
            call append(result, difference)

        end do
    end subroutine subtraction

    ! needs to have 3 options, can;t use logical
    ! this should be post padding 
    ! if one, header1> header2, -1 header2 > header1 , else equal
    function comparison(header1, header2) result (comparer)

        type(Node), pointer, intent(in) :: header1, header2
        type(Node), pointer :: current1, current2
        integer :: comparer  

        integer :: size1, size2

        integer :: mostSignificant1, mostSignificant2, i
        integer, allocatable :: arr1(:), arr2(:)

        current1 => header1
        current2 => header2

        size1 = sizeOfList(header1)
        size2 = sizeOfList(header2)

        allocate(arr1(size1))
        allocate(arr2(size2))

        do i = 1, size1
            arr1(i) = current1%data
            current1 => current1%next
        end do

        do i = 1, size2
            arr2(i) = current2%data
            current2 => current2%next
        end do

        do i = size1, 1, -1
            if (arr1(i) > arr2(i)) then
                comparer = 1
                return
            else if (arr1(i) < arr2(i)) then
                comparer = -1
                return
            end if
        end do

        comparer = 0

        ! need to go down to the tail, as we are in reversed order

    end function comparison

    subroutine multiplication(header1, header2, result)
        type(Node), pointer, intent(in) :: header1, header2
        type(Node), pointer, intent(out) :: result  

        type(Node), pointer :: current1, current2
        type(Node), pointer :: partial, tempResult

        integer :: overflow, product, digit, shift, i

        shift = 0

        current1 => header1
        ! current2 => header2

        do while (associated(current1))
            overflow = 0
            partial => null()

            do i = 1, shift
                call append(partial, 0)
            end do

            current2 => header2
            do while (associated(current2))
                product = current1%data * current2%data + overflow
                digit = mod(product, 10)
                overflow = product / 10
                call append(partial, digit)
                current2 => current2%next
            end do


            if (overflow > 0) then
                call append(partial, overflow)
            end if

            if (.not. associated(result)) then
                result => partial
            else
                call addition(result, partial, tempResult)
                result => tempResult
            end if

            shift = shift + 1
            current1 => current1%next
        end do

    end subroutine multiplication

    ! if false, not 0, else, integer is 0
    function zeroCheck (head) result (zero)
        type(Node), pointer, intent(in) :: head
        type(Node), pointer:: current

        integer :: size, zeroCounter

        logical :: zero

        zeroCounter = 0
        zero = .false.
        size = sizeOfList(head)
        current => head

        do while (associated(current))
            if (current%data == 0) then
                zeroCounter = zeroCounter + 1
            end if

            current => current%next
        end do

        if (zeroCounter == size) then   
            zero = .true.
        end if
    end function zeroCheck

    subroutine reverseLinkedList (original, reverse)
        type(Node), pointer, intent(in) :: original
        type(Node), pointer, intent(out) :: reverse

        type(Node), pointer :: current

        reverse => null()
        current => original

        do while(associated(current))
            call addAtHead(reverse, current%data)

            current => current%next
        end do 
    end subroutine reverseLinkedList


    subroutine multiplyByTen(number)

        type(Node), pointer, intent(inout) :: number

        type(Node), pointer :: current
        current => number

        if (.not. associated(number)) then
            call append(number, 0)
        else
            do while (associated(current%next))
                current => current%next
            end do

            call append(current, 0)
        end if

    end subroutine multiplyByTen


    ! god dammit I shouldve used a doubly linked list, wouldnt have to do this reversal bs
    subroutine division(divisee, divisor, quotient, remainder)
        type(Node), pointer, intent(in) :: divisee, divisor
        type(Node), pointer, intent(out) :: quotient, remainder
        type(Node), pointer :: normalDividend, normalDivisor
        type(Node), pointer :: partialDividend, currentDigit
        type(Node), pointer :: temp
        integer :: digitCount

        logical :: zeroCheck2
        zeroCheck2 = zeroCheck(divisor)

        if (zeroCheck2 .eqv. .false.) then
            print *, "cannot divide by 0"
            return
        end if

        partialDividend => null()
        call append(partialDividend, 0)
        quotient => null()

        currentDigit => normalDividend

        do while (associated(currentDigit))
            ! Shift partial dividend left by one digit (i.e. multiply by 10).
            call shiftLeft(partialDividend)
            
            ! Bring down the next digit: add current digit's data to partialDividend.
            ! One way: Create a linked list node for the digit and add it.
            call addition(partialDividend, createSingleDigit(currentDigit%data), temp)
            partialDividend => temp

            ! Initialize the count for the current quotient digit.
            digitCount = 0
            ! While partialDividend >= normalDivisor, subtract normalDivisor.
            do while (comparison(partialDividend, normalDivisor) >= 0)
                call subtraction(partialDividend, normalDivisor, temp, dummyNegative)
                partialDividend => temp
                digitCount = digitCount + 1
            end do

            ! Append the determined digit to the quotient.
            call append(quotient, digitCount)

            currentDigit => currentDigit%next
        end do

        remainder => partialDividend


    end subroutine division





end module dynllist