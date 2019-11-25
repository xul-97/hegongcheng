program main
    use operator_type_module
    use operator_plus
    use operator_devide
    implicit none

    class(operator), pointer :: operator_ptr 

    type(op_plus), pointer :: plus => null()
    type(op_devide), pointer :: devide => null()


    write(*,*) "===========================type allocate=========================="

    allocate(plus)
    call plus%set(22,11)
    call plus%solve()
    call plus%print_result()
    call plus%clear()

    allocate(devide)
    call devide%set(22,11)
    call devide%solve()
    call devide%print_result()
    call devide%clear()

endprogram