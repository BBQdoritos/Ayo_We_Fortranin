program unbounded

    !Compiling: gfortran dynllist.f03 unbounded.f03 -o unbounded 

    !Check list
        ! make sure that we can do addition, multiplication and all the good stuff

        

    !Completed
        !valid operations (done!)
        !need to verify that the number is valid (no chars) (done!)
        !need to make sure that any input is either positive or negative (before doing this I need to implement the linked list)
        ! implement linked list
        ! print out the linked list


!doing regular arithmetic starts from THE END, REVERSE THE LINKED LIST

    use dynllist

    implicit none

    !------------------------------------------------    
    type(Node), pointer :: headForOperand1 => null()
    type(Node), pointer :: headForOperand2 => null()
    type(Node), pointer :: resultHead => null()
    !------------------------------------------------

    character :: operation
    logical :: valid_operation, validOperant1, validOperation2, negativeOperand1, negativeOperand2
    character(len=100) :: firstOperand, secondOperand
    integer :: i, stringLengthFirst, stringLengthSecond, digit, listSize1, listSize2

    valid_operation = .false.
    negativeOperand1 = .false.
    negativeOperand2 = .false.

    !valid operation check done
    do while (.not. valid_operation)
        write(*,*) "Enter an operation: + - * / or !"
        read(*,'(A)') operation

        write(*,*) 'DEBUG: You entered [', operation, ']'

        select case (operation)
        case('+')
            write(*,*) "Addition works"
            valid_operation = .true.
        case('-')
            write(*,*) "Subtraction works"
            valid_operation = .true.
        case('*')
            write(*,*) "Multiplication works"
            valid_operation = .true.
        case('/')
            write(*,*) "Division works"
            valid_operation = .true.
        case('!')
            write(*,*) "Factorial works"
            valid_operation = .true.
        case default
            write(*,*) "Error: Invalid operation. Please try again."
        end select
    end do

    !can add to a list using ALLOCATE()
    ! I think Ill need to have the input integer as a string, parse through it, while doing so allocate the integer to a node and 
    !put each one into the LL


    !need to parse this and to make sure that every char is a valid integer somehow
    WRITE(*,*) "Enter first operand: "
    READ(*,*) firstOperand
    WRITE(*,*) firstOperand

    stringLengthFirst = len_trim(firstOperand)
    validOperant1 = .true.


    if (stringLengthFirst == 1) then 
        WRITE(*,*) "- is not a number, bro"
        validOperant1 = .false.
    end if


    !I think I will allocate from here, using a do while loop, ensuring that validOperant1 is always true and if its false, crash out
    if (firstOperand(1:1) == '-') then

        negativeOperand1 = .true. !NEED TO MAKE SURE THAT WE USE THIS FOR ADDITION/SUBTRACTION, doesnt really matter for multiplication or division (i mean it does but not really)

        do i = stringLengthFirst, 2, -1
            if (firstOperand(i:i) < '0' .or. firstOperand(i:i) > '9') then
                validOperant1 = .false.
                exit

            else 
                ! append the firstOperand to a linked list
                digit = ichar(firstOperand(i:i)) - ichar('0')
                call append(headForOperand1, digit)
            end if
        end do

    else 
        ! if there is no '-' in the beginning, need to work on this and make sure it works before implementing the one before
        !seems to be working for non-negative numbers, need a way to recognize that it is negative
        do i = stringLengthFirst, 1, -1
            if (firstOperand(i:i) < '0' .or. firstOperand(i:i) > '9') then
                validOperant1 = .false.
                exit

            else 
                ! append the firstOperand to a linked list
                digit = ichar(firstOperand(i:i)) - ichar('0')
                call append(headForOperand1, digit)
            end if
        end do
    end if

    if (validOperant1 .eqv. .false.) then 
        WRITE(*,*) "Operand does not work"
    else 
        WRITE(*,*) "That shit worked"
    end if

    call printList(headForOperand1)

    listSize1 = sizeOfList(headForOperand1)

    WRITE(*,*) "Size of list 1: ", listSize1

    WRITE(*,*) "Enter second operand: "
    READ(*,*) secondOperand
    WRITE(*,*) secondOperand

    stringLengthSecond = len_trim(secondOperand)
    validOperation2 = .true.

    if (stringLengthSecond == 1) then 
        WRITE(*,*) "- is not a number, bro"
        validOperation2 = .false.
    end if

    !I think I will allocate from here, using a do while loop, ensuring that validOperant1 is always true and if its false, crash out
    if (secondOperand(1:1) == '-') then

        negativeOperand2 = .true.

        do i = stringLengthSecond, 2, -1
            if (secondOperand(i:i) < '0' .or. secondOperand(i:i) > '9') then
                validOperation2 = .false.
                exit

            else 
                ! append the stringLengthSecond to a linked list
                digit = ichar(secondOperand(i:i)) - ichar('0')
                call append(headForOperand2, digit)
            end if
        end do
    else 
        ! if there is no '-' in the beginning, need to work on this and make sure it works before implementing the one before
        !seems to be working for non-negative numbers, need a way to recognize that it is negative
        do i = stringLengthSecond, 1, -1
            if (secondOperand(i:i) < '0' .or. secondOperand(i:i) > '9') then
                validOperation2 = .false.
                exit
            else 
                ! append the stringLengthSecond to a linked list
                digit = ichar(secondOperand(i:i)) - ichar('0')
                call append(headForOperand2, digit)
            end if
        end do
    end if


    call printList(headForOperand2)

    listSize2 = sizeOfList(headForOperand2)
    WRITE(*,*) "Size of list 2: ", listSize2


    if (validOperation2 .eqv. .false.) then 
        WRITE(*,*) "Operand does not work"
    else 
        WRITE(*,*) "That shit worked"
    end if


    ! ----------------------------------------------------------------------------------------
    ! this is the check point, if we pad or not (only for addition?)
    ! ----------------------------------------------------------------------------------------


    ! padding for operand1
    do while (listSize1 < listSize2) 
        call paddingZeros(headForOperand1)
        listSize1 = listSize1 + 1
    end do

    ! padding for operand2
    do while (listSize1 > listSize2) 
        call paddingZeros(headForOperand2)
        listSize2 = listSize2 + 1
    end do

    call addition(headForOperand1, headForOperand2, resultHead)

    ! so what we need to do is to run through both lists parallelly, ensuring that we've padded
    ! the LL as needed, and then put that result into the result header, carrying any carryover into the next
    ! cells to be added, then reverting to 0 and continuing from there

    ! also, the print needs to be in reverse


    WRITE(*,*) "The result is: "
    call printList(resultHead)



!need to make sure that any input is either positive or negative

!need to verify that the number is valid (no chars)

!allow for operations

!store the numbers using a dynamic linked list and should be provided in the form of a module in dynlist.f03
!need to be able to print out the linked list and create it




end program unbounded





! ---------------------------test for lengths and printing properly-----------------------------------------------------------------

    ! WRITE(*,*) "OPERAND 1: "
    ! call printList(headForOperand1)
    ! WRITE(*,*) "OPERAND 2: "
    ! call printList(headForOperand2)

    ! listSize1 = sizeOfList(headForOperand1)
    ! WRITE(*,*) "Size of list 1: ", listSize1

    ! listSize2 = sizeOfList(headForOperand2)
    ! WRITE(*,*) "Size of list 2: ", listSize2

! ----------------------------------------------------------------------------------------------------------------------------------



    ! type, public :: linkedList
    !     private
    !         integer(ki4) :: size = 0_ki4 !should be the length of the LL
    !         type(node), pointer :: head => null()
    !         type(node), pointer :: tail => null()
    !     contains
        
    !     procedure :: append
    !     !need to implement other functions, will include later

    ! end type linkedList