submodule(forlab) forlab_x
    use forlab_kinds
    implicit none
    
contains
    module procedure rmut_sp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

    module procedure rmut_dp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

    module procedure rmut_qp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

    module procedure cmut_sp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

    module procedure cmut_dp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

    module procedure cmut_qp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

    module procedure rcmut_sp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

    module procedure rcmut_dp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

    module procedure rcmut_qp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

    module procedure crmut_sp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

    module procedure crmut_dp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

    module procedure crmut_qp
        if (size(m1, 2) == size(m2, 1)) then
            ret = matmul(m1, m2)
        else
            print *, 'size(matrix_1, 2) /= size(matrix_2, 1)'
        end if
    end procedure

end submodule