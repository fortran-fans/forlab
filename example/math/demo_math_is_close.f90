program demo_math_is_close
    use forlab_math, only: is_close
    use stdlib_error, only: check
    real :: x(2) = [1, 2]
    print *, is_close(x,[real :: 1, 2.1])     !! [T, F]
    print *, all(is_close(x,[real :: 1, 2.1]))!! F
    print *, is_close(2.0, 2.1, atol=0.1)     !! T
    call check(all(is_close(x, [2.0, 2.0])), msg="all(is_close(x, [2.0, 2.0])) failed.", warn=.true.)
        !! all(is_close(x, [2.0, 2.0])) failed.
end program demo_math_is_close