
submodule(forlab_linalg) forlab_linalg_eye

    implicit none

contains

    module procedure eye_sp
    integer :: i

    X = 0
    do i = 1, min(size(X, 1), size(X, 2))
        X(i, i) = 1.0_sp
    end do
    return

    end procedure eye_sp
    module procedure eye_dp
    integer :: i

    X = 0
    do i = 1, min(size(X, 1), size(X, 2))
        X(i, i) = 1.0_dp
    end do
    return

    end procedure eye_dp
    module procedure eye_qp
    integer :: i

    X = 0
    do i = 1, min(size(X, 1), size(X, 2))
        X(i, i) = 1.0_qp
    end do
    return

    end procedure eye_qp

end submodule forlab_linalg_eye
