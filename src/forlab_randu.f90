

submodule(forlab) forlab_randu
    !! Version: experimental
    !!
    !! Discussion:
    !! ----
    !! https://fortran-lang.discourse.group/t/fortran-function-return-value-polymorphism/1350/5
    use forlab_kinds
    implicit none
contains
    !! Default versions
    module function randu_0_default ()
        real(dp) :: randu_0_default 
        


        call random_number(randu_0_default)
        return
    end function
    module function randu_1_default (dim1)
        integer, intent(in) :: dim1
        real(dp), allocatable :: randu_1_default (:)
        
        allocate (randu_1_default (dim1))

        call random_number(randu_1_default)
        return
    end function
    module function randu_2_default (dim1, dim2)
        integer, intent(in) :: dim1, dim2
        real(dp), allocatable :: randu_2_default (:,:)
        
        allocate (randu_2_default (dim1, dim2))

        call random_number(randu_2_default)
        return
    end function
    module function randu_3_default (dim1, dim2, dim3)
        integer, intent(in) :: dim1, dim2, dim3
        real(dp), allocatable :: randu_3_default (:,:,:)
        
        allocate (randu_3_default (dim1, dim2, dim3))

        call random_number(randu_3_default)
        return
    end function
    !! Multi-precision versions
    module function randu_0_sp (flag)
        real(sp) :: randu_0_sp 
        real(sp), intent(in) :: flag



        call random_number(randu_0_sp)
        return
    end function
    module function randu_0_dp (flag)
        real(dp) :: randu_0_dp 
        real(dp), intent(in) :: flag



        call random_number(randu_0_dp)
        return
    end function
    module function randu_0_qp (flag)
        real(qp) :: randu_0_qp 
        real(qp), intent(in) :: flag



        call random_number(randu_0_qp)
        return
    end function
    module function randu_1_sp (dim1, flag)
        !! Unlike dynamic scripting languages, static languages generally
        !! have multiple precision variables, so we need to explicitly provide precision hints.
        integer, intent(in) :: dim1
        real(sp), allocatable :: randu_1_sp (:)
        real(sp), intent(in) :: flag

        allocate (randu_1_sp (dim1))

        call random_number(randu_1_sp)
        return
    end function
    module function randu_1_dp (dim1, flag)
        !! Unlike dynamic scripting languages, static languages generally
        !! have multiple precision variables, so we need to explicitly provide precision hints.
        integer, intent(in) :: dim1
        real(dp), allocatable :: randu_1_dp (:)
        real(dp), intent(in) :: flag

        allocate (randu_1_dp (dim1))

        call random_number(randu_1_dp)
        return
    end function
    module function randu_1_qp (dim1, flag)
        !! Unlike dynamic scripting languages, static languages generally
        !! have multiple precision variables, so we need to explicitly provide precision hints.
        integer, intent(in) :: dim1
        real(qp), allocatable :: randu_1_qp (:)
        real(qp), intent(in) :: flag

        allocate (randu_1_qp (dim1))

        call random_number(randu_1_qp)
        return
    end function
    module function randu_2_sp (dim1, dim2, flag)
        integer, intent(in) :: dim1, dim2
        real(sp), allocatable :: randu_2_sp (:,:)
        real(sp), intent(in) :: flag

        allocate (randu_2_sp (dim1, dim2))

        call random_number(randu_2_sp)
        return
    end function
    module function randu_2_dp (dim1, dim2, flag)
        integer, intent(in) :: dim1, dim2
        real(dp), allocatable :: randu_2_dp (:,:)
        real(dp), intent(in) :: flag

        allocate (randu_2_dp (dim1, dim2))

        call random_number(randu_2_dp)
        return
    end function
    module function randu_2_qp (dim1, dim2, flag)
        integer, intent(in) :: dim1, dim2
        real(qp), allocatable :: randu_2_qp (:,:)
        real(qp), intent(in) :: flag

        allocate (randu_2_qp (dim1, dim2))

        call random_number(randu_2_qp)
        return
    end function
    module function randu_3_sp (dim1, dim2, dim3, flag)
        integer, intent(in) :: dim1, dim2, dim3
        real(sp), allocatable :: randu_3_sp (:,:,:)
        real(sp), intent(in) :: flag

        allocate (randu_3_sp (dim1, dim2, dim3))

        call random_number(randu_3_sp)
        return
    end function
    module function randu_3_dp (dim1, dim2, dim3, flag)
        integer, intent(in) :: dim1, dim2, dim3
        real(dp), allocatable :: randu_3_dp (:,:,:)
        real(dp), intent(in) :: flag

        allocate (randu_3_dp (dim1, dim2, dim3))

        call random_number(randu_3_dp)
        return
    end function
    module function randu_3_qp (dim1, dim2, dim3, flag)
        integer, intent(in) :: dim1, dim2, dim3
        real(qp), allocatable :: randu_3_qp (:,:,:)
        real(qp), intent(in) :: flag

        allocate (randu_3_qp (dim1, dim2, dim3))

        call random_number(randu_3_qp)
        return
    end function
end submodule
