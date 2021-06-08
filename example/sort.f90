!! fpm run --example example_sort --profile release
program main
    use forlab, only: RPRE, disp, sort
    real(RPRE) :: x(5) =[2,1,4,5,2]

    x = sort(x)
    call disp(x)
    
end program