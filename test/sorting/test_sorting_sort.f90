program test_sorting_sort
    use forlab_sorting,only:argsort,sort
    use forlab_io, only:disp
    real(8)::x(4)
    integer::a(4)=[1,3,2,4]
    call random_number(x)
    call disp("argsort/sort tri U L test")
    call disp(x,"x")
    call disp(argsort(x),"argsort(x)")
    call disp(argsort(x,2),"argsort(x)")
    call disp(sort(x,2),"sort(x)")
    call disp(sort(x,1),"sort(x)")
    call disp(a,"x")
    call disp(argsort(a),"argsort(a)")
    call disp(argsort(a,2),"argsort(a)")
    call disp(sort(a,2),"sort(a)")
    call disp(sort(a,1),"sort(a)")
end program test_sorting_sort