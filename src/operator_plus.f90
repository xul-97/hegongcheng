module operator_plus
    use operator_type_module

    implicit none
    private

    public:: op_plus

    type, extends(operator) :: op_plus

     contains

        procedure, pass :: set
        procedure, pass :: solve
    endtype

contains

  subroutine set(this,x,y)
     class(op_plus) :: this
     integer :: x
     integer :: y
     integer  :: nerror

     nerror = 0

    
     if((x .le. 0) .or. (y .le. 0)) then
         write(*,*) "Error: x or y is less than 0"
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
     class(op_plus) :: this
   
     this%result = this%x + this%y

  endsubroutine
endmodule

