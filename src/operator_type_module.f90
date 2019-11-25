module operator_type_module
    implicit none
    private

    public :: operator

    type :: operator
        integer :: x
        integer :: y
        integer :: result
    contains
        procedure,pass :: print_result
        procedure,pass :: clear
    endtype

contains

    subroutine print_result(this)
        class(operator) :: this

        write(*,*) "The result is ", this%result
    endsubroutine

    subroutine clear(this)
        class(operator) :: this
        this%x = 0
        this%y = 0
        this%result = 0

    endsubroutine

endmodule operator_type_module

