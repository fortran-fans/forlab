program demo_stats_randu

    use forlab_stats, only: randu

    print *, "running `demo_stats_randu`.."

    print *, randu(start=1, end=2)
    print *, randu(start=1.0, end=2.0, ndim=3)
    print *, reshape(randu(1.0, 2.0, 2*2), [2,2])

    !> Possible output:

    !! 2
    !! 1.65676987       1.11625218       1.03502560
    !! 1.74973476       1.82997108       1.77998054       1.14384007

end program demo_stats_randu