!! fpm test test
program main
    block
        use forlab, only: disp,randu,rng
        integer :: i
        integer, allocatable :: iX(:)
        call rng
        call randu(i, -1, 1)
            !! 错误示范
        call disp(i, 'randu(i):')
        allocate(iX(10))
        call randu(iX(:))
            !! 默认生成[0, 1]区间随机数
        call disp(iX, 'randu(iX):')
        call randu(iX(:), 10, 100)
            !! 生成[10, 100]区间随机数
        call disp(iX, 'randu(iX):')
    end block


end program
