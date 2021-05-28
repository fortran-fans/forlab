
submodule(forlab) forlab_eye
    use forlab_kinds

contains
        module function eye (dim1, dim2)
            real(dp), dimension(:, :), allocatable :: eye
            integer, intent(in) :: dim1
            integer, intent(in), optional :: dim2
            integer :: i

            if (.not. present(dim2)) then
                eye = zeros(dim1, dim1)
                do i = 1, dim1
                    eye (i, i) = 1.0d0
                end do
            else
                eye = zeros(dim1, dim2)
                do i = 1, min(dim1, dim2)
                    eye (i, i) = 1.0d0
                end do
            end if
            return
        end function
end submodule
