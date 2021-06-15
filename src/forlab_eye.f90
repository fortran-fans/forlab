submodule(forlab) forlab_eye
    !! Version: experimental
    !!
    !! eye creates the identity matrix.
    !!
    !!## Syntax
    !!    I = eye(dim1)
    !!    I = eye(dim1, dim2)
    !!
    !!## Description
    !! `I = eye(dim1)` returns an dim1-by-dim1 matrix with ones on the main
    !! diagonal and zeros elsewhere.  
    !! `I = eye(dim1, dim2)` returns a dim1-by-dim2 matrix with ones on the
    !! main diagonal and zeros elsewhere.
    !!
    !!## Examples
    !!      I = eye(3)  
    !!          1.  0.  0.  
    !!          0.  1.  0.  
    !!          0.  0.  1.
    !!
    !!      I = eye(3, 4)  
    !!          1.  0.  0.  0.  
    !!          0.  1.  0.  0.  
    !!          0.  0.  1.  0.
    !!
    !!      I = eye(4, 3)  
    !!          1.  0.  0.
    !!          0.  1.  0.
    !!          0.  0.  1.
    !!          0.  0.  0. 
    use forlab_kinds
    implicit none
contains
    module procedure eye_sp
        integer :: i
        
        call zeros(X)
        do i = 1, min(size(X,1), size(X,2))
            X(i, i) = 1.0_sp
        end do
        return
    end procedure
    module procedure eye_dp
        integer :: i
        
        call zeros(X)
        do i = 1, min(size(X,1), size(X,2))
            X(i, i) = 1.0_dp
        end do
        return
    end procedure
    module procedure eye_qp
        integer :: i
        
        call zeros(X)
        do i = 1, min(size(X,1), size(X,2))
            X(i, i) = 1.0_qp
        end do
        return
    end procedure
end submodule
