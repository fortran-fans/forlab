!> fpm run --example math_signum
program demo_math_signum
    use forlab_math, only: signum

    print *, signum(1 - 2)
    print *, signum([0.0, 2.1])
    print *, signum((1.0, -2.0))

    !>    -1
    !>   0.00000000       1.00000000    
    !>          (0.447213590,-0.894427180)

end program demo_math_signum
