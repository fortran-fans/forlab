!> Compare with `demo_linalg_zerosones`
!> It is not recommended to use the `zeros/ones` function, 
!>  it is recommended to use `allocate(array(dim1, dim2, ..), source=0.0/1.0)`.
program demo_allocation
    use forlab_io, only: disp

    real, allocatable :: zero(:, :), one(:, :)
    real, allocatable :: array(:, :)


    allocate(zero(1, 2), source=0.0)
    allocate(one (1, 2), source=1.0)

    call disp(zero, "zeros: ")
    call disp(one , "ones : ")

    allocate(array(2, 2), source=0.0)
    call disp(array, "array with zeros: ")

    deallocate(array)
    allocate(array(2, 2), source=1.0)
    call disp(array, "array with ones : ")

end program demo_allocation