INCLUDE 'stack_module.f90'

PROGRAM MAIN
    USE STACK_MODULE

    type(character_stack) :: stk_char
    type(integer_stack) :: stk_int

    character :: temp_ch

    PRINT *, "TEST [Character Stack]"
    
    print *, "> push 'a'"
    CALL push(stk_char, 'a')

    write(*, fmt="(1x,a,i0)", advance="no") "("
    do i=1, stk_char%top
        write(*, fmt="(1x,a,i0)", advance="no") stk_char%items_array(i:i)
        write(*, fmt="(1x,a,i0)", advance="no") " "
    end do
    print *, ")"

    print *, "> push 'b'"
    CALL push(stk_char, 'b')

    write(*, fmt="(1x,a,i0)", advance="no") "("
    do i=1, stk_char%top
        write(*, fmt="(1x,a,i0)", advance="no") stk_char%items_array(i:i)
        write(*, fmt="(1x,a,i0)", advance="no") " "
    end do
    print *, ")"


    print *, "> pop"
    temp_ch = pop(stk_char)

    write(*, fmt="(1x,a,i0)", advance="no") "("
    do i=1, stk_char%top
        write(*, fmt="(1x,a,i0)", advance="no") stk_char%items_array(i:i)
        write(*, fmt="(1x,a,i0)", advance="no") " "
    end do
    write(*, fmt="(1x,a,i0)", advance="no") ")"

END PROGRAM MAIN
