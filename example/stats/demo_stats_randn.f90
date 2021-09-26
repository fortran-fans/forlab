program demo_stats_randn

    use forlab_stats, only: randn

    print *, "running `demo_stats_randn`.."

    print *, randn(mean=0.0, std=2.0)
    print *, randn(mean=0.0, std=2.0, ndim=3)
    print *, reshape(randn(0.0, 2.0, 2*2), [2,2])

    !> Chi-square distribution of 3 degrees of freedom
    print *, sum(randn(mean=0.0, std=1.0, ndim=3)**2)
    print *, sum(reshape(randn(mean=0.0, std=1.0, ndim=5*3), [5, 3])**2, dim=2)

    !> Possible output:

    !! -0.387298465    
    !! -1.37615824     -0.529266298       5.43095016
    !! -1.35311902       1.81701779      0.772518456     -0.269844353

    !! 9.45483303
    !! 0.962645471      0.698421597      0.687875450       4.75956964       1.71025097

end program demo_stats_randn