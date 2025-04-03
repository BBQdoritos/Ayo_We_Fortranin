program unbounded

    !Check list
        ! implement linked list
        ! make sure that we can do addition, multiplication and all the good stuff
        ! print out the linked list
        ! 

    !Completed
        !valid operations (done!)
        !need to verify that the number is valid (no chars) (done!)
        !need to make sure that any input is either positive or negative (before doing this I need to implement the linked list)


    use dynllist

    type, public :: linkedList
        private
            integer(ki4) :: size = 0_ki4 !should be the length of the LL
            type(node), pointer :: head => null()
            type(node), pointer :: tail => null()
        contains
        
        procedure :: append
        !need to implement other functions, will include later

    end type linkedList

    implicit none

    character :: operation
    logical :: valid_operation, validOperant1, validOperation2
    character(len=100) :: firstOperand, secondOperand
    integer :: i, stringLengthFirst, stringLengthSecond

    valid_operation = .false.

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
        do i = 2, stringLengthFirst
            if (firstOperand(i:i) < '0' .or. firstOperand(i:i) > '9') then
                validOperant1 = .false.
                exit
            end if
        end do
    end if


    if (validOperant1 .eqv. .false.) then 
        WRITE(*,*) "Operand does not work"
    else 
        WRITE(*,*) "That shit worked"
    end if

    WRITE(*,*) "Enter second operand: "
    READ(*,*) secondOperand
    WRITE(*,*) secondOperand

    stringLengthSecond = len_trim(secondOperand)
    validOperation2 = .true.

    if (stringLengthSecond == 1) then 
        WRITE(*,*) "- is not a number, bro"
        validOperation2 = .false.
    end if

    if (secondOperand(1:1) == '-') then
        do i = 2, stringLengthSecond
            if (secondOperand(i:i) < '0' .or. secondOperand(i:i) > '9') then
                validOperation2 = .false.
                exit
            end if
        end do
    end if
    if (validOperation2 .eqv. .false.) then 
        WRITE(*,*) "Operand does not work"
    else 
        WRITE(*,*) "That shit worked"
    end if



    WRITE(*,*) "The result is: "



!need to make sure that any input is either positive or negative

!need to verify that the number is valid (no chars)

!allow for operations

!store the numbers using a dynamic linked list and should be provided in the form of a module in dynlist.f03
!need to be able to print out the linked list and create it




end program unbounded