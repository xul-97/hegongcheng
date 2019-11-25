module operator_devide
    use operator_type_module

    implicit none
    private

    public:: op_devide

    type, extends(operator) :: op_devide

    contains

        procedure, pass :: set
        procedure, pass :: solve
    endtype 

contains

  subroutine set(this,x,y)
     class(op_devide) :: this
     integer :: x
     integer :: y
     integer  :: nerror

     nerror = 0

   
     if((x == 0) .or. (y == 0)) then
         write(*,*) "Error: x or y is equal 0"
         nerror = nerror + 1
     endif

     if(nerror == 0) then
         this%x = x
         this%y = y
     else
         write(*,*) "Please check the inputs of set(), some wrong input is provided"
     endif
  endsubroutine

  subroutine solve(this)
     class(op_devide) :: this

     this%result = this%x / this%y
  endsubroutine
endmodule