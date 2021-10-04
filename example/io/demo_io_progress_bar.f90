program demo_io_progress_bar

    use forlab_io, only: progress_bar
    use forlab_stats, only: randu
    
    do i = 0, 100, 10
        call progress_bar(i, 100)
        call sleep(randu(1, 2))
    end do
    
    write(*,"(3A)",advance="no") char(13), "Calculation Done!", repeat(" ", 55)

end program demo_io_progress_bar
