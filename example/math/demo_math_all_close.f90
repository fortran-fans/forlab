program demo_math_all_close
    use forlab_math,  only: all_close
    use stdlib_error, only: check
    real    :: x(2) = [1, 2], random(4, 4)
    complex :: z(4, 4)
    
    call check(all_close(x, [2.0, 2.0], rel_tol=1.0e-6, abs_tol=1.0e-3), &
               msg="all_close(x, [2.0, 2.0]) failed.", warn=.true.)
               !! all_close(x, [2.0, 2.0]) failed.
    call random_number(random(4, 4))
    z = 1.0
    print *, all_close(z+1.0e-11*random, z)     !! T
    
end program demo_math_all_close