!------------------------------------------------------------------------------
! Hallym University, School of Software - Big Data
!------------------------------------------------------------------------------
!
! MODULE:  STACK_MODULE
!
! AUTHOR:
!>  20205178 Park Hyun (Sophomore)
!
! DESCRIPTION:
!>  implement of stack in FORTRAN.
!------------------------------------------------------------------------------

module STACK_MODULE
    implicit none

    interface is_empty
        module procedure :: is_empty_character_stack
        module procedure :: is_empty_integer_stack
    end interface

    interface initiate
        module procedure :: initiate_character_stack
        module procedure :: initiate_integer_stack
    end interface

    interface push
        module procedure :: push_character_stack
        module procedure :: push_integer_stack
    end interface

    interface pop
        module procedure :: pop_character_stack
        module procedure :: pop_integer_stack
    end interface

    interface remove
        module procedure :: remove_character_stack
        module procedure :: remove_integer_stack
    end interface

    ! interface to_string
    !     module procedure :: to_string_character_stack
    !     module procedure :: to_string_integer_stack
    ! end interface

    type :: character_stack
        integer :: top = 0
        character, dimension(256) :: items_array
    end type

    type :: integer_stack
        integer :: top = 0
        integer, dimension(256) :: items_array
    end type

contains
    subroutine initiate_character_stack(argstack)
        type(character_stack) :: argstack
        argstack%top = 0
    end subroutine initiate_character_stack

    subroutine initiate_integer_stack(argstack)
        type(integer_stack) :: argstack
        argstack%top = 0
    end subroutine initiate_integer_stack

    logical function is_empty_character_stack(argstack) result(retval)
        implicit none
        type(character_stack) :: argstack
        retval = argstack%top .eq. 0
    end function is_empty_character_stack

    logical function is_empty_integer_stack(argstack) result(retval)
        implicit none
        type(integer_stack) :: argstack
        retval = argstack%top .eq. 0
    end function is_empty_integer_stack

    subroutine push_character_stack(argstack, item)
        type(character_stack) :: argstack
        character :: item
        if (argstack%top .eq. 256) then
            return
        end if
        argstack%top = argstack%top + 1
        argstack%items_array(argstack%top) = item
    end subroutine push_character_stack

    subroutine push_integer_stack(argstack, item)
        type(integer_stack) :: argstack
        integer :: item
        if (argstack%top .eq. 256) then
            return
        end if
        argstack%top = argstack%top + 1
        argstack%items_array(argstack%top) = item
    end subroutine push_integer_stack

    character function pop_character_stack(argstack) result(retval)
        type(character_stack) :: argstack
        if (is_empty(argstack)) then
            return
        end if
        retval = argstack%items_array(argstack%top)
        argstack%top = argstack%top - 1
    end function

    integer function pop_integer_stack(argstack) result(retval)
        type(integer_stack) :: argstack
        if (is_empty(argstack)) then
            return
        end if
        retval = argstack%items_array(argstack%top)
        argstack%top = argstack%top - 1
    end function

    subroutine remove_character_stack(argstack, error)
        type(character_stack) :: argstack
        logical :: error
        if (is_empty(argstack)) then
            error = .TRUE.
            return
        end if
        argstack%top = argstack%top - 1
    end subroutine remove_character_stack

    subroutine remove_integer_stack(argstack, error)
        type(integer_stack) :: argstack
        logical :: error
        if (is_empty(argstack)) then
            error = .TRUE.
            return
        end if
        argstack%top = argstack%top - 1
    end subroutine remove_integer_stack
end module STACK_MODULE
