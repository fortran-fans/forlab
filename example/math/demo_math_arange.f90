program demo_math_arange
    use forlab_math, only: arange
    use forlab_io, only: disp

    call disp(arange(3))                 !! [1,2,3]
    call disp(arange(-1))                !! [1,0,-1]
    call disp(arange(0,2))               !! [0,1,2]
    call disp(arange(1,-1))              !! [1,0,-1]
    call disp(arange(0, 2, 2))           !! [0,2]

    call disp(arange(3.0))               !! [1.0,2.0,3.0]
    call disp(arange(0.0,5.0))           !! [0.0,1.0,2.0,3.0,4.0,5.0]
    call disp(arange(0.0,6.0,2.5))       !! [0.0,2.5,5.0]

    call disp((1.0,1.0)*arange(3))       !! [(1.0,1.0),(2.0,2.0),[3.0,3.0]]

    call disp(arange(0.0,2.0,-2.0))      !! [0.0,2.0].     Not recommended: `step` argument is negative!
    call disp(arange(0.0,2.0,0.0))       !! [0.0,1.0,2.0]. Not recommended: `step` argument is zero!

end program demo_math_arange