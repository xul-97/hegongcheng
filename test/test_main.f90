program test_main
    use operator_type_module
    use operator_plus
    use operator_devide


    type(op_plus), pointer :: plus => null()
    type(op_devide), pointer :: devide => null()

    integer :: nerror
    integer :: nerror_temp
    integer :: ntest

    nerror = 0
    nerror_temp = 0
    ntest = 0

    allocate(plus)
    write(*,*) "==================test Operator_plus%set ================="

    call plus%set(9,3)
    ntest = ntest + 1
    if((plus%x /= 9) .or. (plus%y /= 3)) then
        nerror = nerror + 1
        write(*,*) plus%x, plus%y
        write(*,*) "Error: Operator_plus%set()未能正确设置相关参数"
    else
        write(*,*) "Passed: Operator_plus%set()"
    endif
    

    write(*,*) "==================test Operator_plus%set x>0,y>0================="

    call plus%set(-9,3)
    ntest = ntest + 1
    if(plus%x == -9) then
        nerror = nerror + 1
        write(*,*) "Error: x <= 0未检测出错误"
    endif

    call plus%set(9,-3)
    ntest = ntest + 1
    if(plus%y == -3) then
        nerror = nerror + 1
        write(*,*) "Error: y <= 0未检测出错误"
    endif

    write(*,*) "==================test Operator_plus%solve ========================"
    ntest = ntest + 1
    call plus%set(9,3)
    call plus%solve()
    ntest = ntest + 1
    if(plus%result /= 12) then
        nerror = nerror + 1
        write(*,*) "Error: Operator_plus%solve()错误"
    endif
    deallocate(plus)

    allocate(devide)
    write(*,*) "==================test Operator_devide%set ================="

    call devide%set(9,3)
    ntest = ntest + 1
    if((devide%x /= 9) .or. (devide%y /= 3)) then
        nerror = nerror + 1
        write(*,*) devide%x, devide%y
        write(*,*) "Error: Operator_devide%set()未能正确设置相关参数"
    else
        write(*,*) "Passed: Operator_devide%set()"
    endif

    write(*,*) "==================test Operator_devide%set x/=0,y/=0================="

    call devide%set(0,3)
    ntest = ntest + 1
    if(devide%x == 0) then
        nerror = nerror + 1
        write(*,*) "Error: x /= 0未检测出错误"
    endif

    call devide%set(9,0)
    ntest = ntest + 1
    if(devide%y == 0) then
        nerror = nerror + 1
        write(*,*) "Error: y /= 0未检测出错误"
    endif

    write(*,*) "==================test Operator_devide%solve ========================"
    ntest = ntest + 1
    call plus%set(9,3)
    call devide%solve()
    ntest = ntest + 1
    if(devide%result /= 3) then
        nerror = nerror + 1
        write(*,*) "Error: Operator_devide%solve()错误"
    endif
    deallocate(devide)


    write(*,*)"================Summary================= ===="
    if(nerror== 0)then
         write(*,'(/a,i2,a/)')"All",ntest,"tests are passed!!! congratulations!!!"
         write(*,*)
    else
         write(*,'(/i2,a,i2,a/)') nerror,"/",ntest,"tests are failed!!!,please check the code."
         write(*,*)
    endif
endprogram test_main







