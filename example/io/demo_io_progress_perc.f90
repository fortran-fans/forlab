program demo_io_progress_perc

    use forlab_io, only: progress_perc
    use forlab_stats, only: randu
    
    do i = 0, 100, 10
        call progress_perc(i, 100, ">>")
        call sleep(randu(1, 3))
    end do

end program demo_io_progress_perc