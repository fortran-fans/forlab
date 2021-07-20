program test_linalg_tri
    use forlab_linalg,only:tril,triu,ones
    use forlab_io, only: disp
    real::X(4,4)
    call disp("tri U L test")
    call ones(X)
    call disp(X,"A=")
    call disp(tril(X),"tril")
    call disp(triu(X),"triu")
    call disp(tril(X,-1),"tril")
    call disp(triu(X,1),"triu")
end program test_linalg_tri