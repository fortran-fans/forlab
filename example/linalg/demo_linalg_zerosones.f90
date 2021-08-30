!> It is not recommended to use the `zeros/ones` function, 
!>  it is recommended to use `allocate(array(dim1, dim2, ..), source=0.0/1.0)`.
program demo_linalg_zerosones
    use forlab_linalg, only: zeros, ones
    use forlab_io, only: disp

    real, allocatable :: zero(:, :), one(:, :)
    real, allocatable :: array(:, :)

    zero = zeros(1, 2)
    one  = ones (2, 1)

    call disp(zero, "zeros: ")
    call disp(one , "ones : ")

    call disp(ones(2, 2)/2, "!attention: `ones(2, 2)/2` is like `1/2 == 0`")

    array = zeros(2, 2)
    call disp(array, "array with zeros: ")

    array = ones (2, 2)
    call disp(array, "array with ones :")

end program demo_linalg_zerosones