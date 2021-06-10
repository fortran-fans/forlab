!! `fpm test` or `fpm test test`
program main
    block
        use forlab, only: empty, disp, operator(.x.)
        real, dimension(:, :), allocatable :: x_sp, y_sp
        real(8), dimension(:, :), allocatable :: x_dp, y_dp
        real(16), dimension(:, :), allocatable :: x_qp, y_qp

        x_sp = empty(2, 2); x_dp = empty(2, 2); x_qp = empty(2, 2)
        y_sp = empty(2, 2); y_dp = empty(2, 2); y_qp = empty(2, 2)

        x_sp = 1.; x_dp = 1.; x_qp = 1.
        y_sp = 2.; y_dp = 2.; y_qp = 2.

        call disp('----------------------------')
        call disp(x_sp.x.y_sp, 'x .x. y SP✨:')
        call disp(x_dp.x.y_dp, 'x .x. y DP✨:')
        call disp(x_qp.x.y_qp, 'x .x. y QP✨:')
    end block

    block
        use forlab, only: operator(.i.), disp
        ! test inv and solve equation Ax=b, A^(-1) * b
        real :: a(2, 2) = reshape([1, 2, 3, 1], [2, 2])
        real :: b(2) = [1, 1]
        call disp("Test Inv and solve equation Ax=b")
        call disp('A=')
        call disp(a)
        call disp("b=")
        call disp(b)
        call disp("x=")
        call disp(matmul(.i.a, b))
    end block

    block
        use forlab, only: acosd, tand, disp
        ! test degrees circular function
        call disp('Test degrees circular function')
        call disp("acosd(1.d0)=")
        call disp(acosd(1.d0))
        call disp("tand([45.0,60.0,0.0])")
        call disp(tand([45.0, 60.0, 0.0]))
    end block

    block
        use forlab, only: tic, toc, disp, rng, randn
        real :: time_sp
        real(8) :: time_dp
        real(16) :: time_qp
        real(16), allocatable :: x(:)

        call disp('----------------------------')
        call tic()
        x = randn(1000)
        call disp(size(x), 'x size:')

        call toc()
        call toc(time_sp)
        call disp(time_sp, 'tic/toc sp-version is passed:')

        call toc()
        call toc(time_dp)
        call disp(time_dp, 'tic/toc dp-version is passed:')

        call toc()
        call toc(time_qp)
        call disp(time_qp, 'tic/toc qp-version is passed:')
    end block

    block
        use forlab, only: angle, disp, pi
        call disp('-----------------------')
        call disp(angle((1, 1)), 'rad phase:')
        call disp(angle((1, 1))*180/pi, 'deg phase:')
    end block

    block
        use forlab, only: linspace, dlinspace, disp, pi
        call disp('-----------------------')
        call disp(linspace(1, 3, 5), 'linspace 1:')
        call disp(linspace(1.1d0, pi, 1), 'linspace 2:')
    end block

    block
        use forlab, only: disp, savebin, dloadbin, rng, randn, savetxt, dloadtxt
        real*8, allocatable :: x(:)
        call disp('---------------------')
        call rng()
        x = randn(3)
        call disp(x)
        call savebin('DP.bin', x)
        call savetxt('DP.txt', x)
        x = dloadbin('DP.bin')
        call disp(x)
        x = dloadtxt('DP.txt')
        call disp(x)
    end block

    block
        use forlab,only:tril,triu,ones,disp
        call disp("tri U L test")
        call disp(ones(4,4),"A=")
        call disp(tril(ones(4,4)),"tril")
        call disp(triu(ones(4,4)),"triu")
        call disp(tril(ones(4,4),-1),"tril")
        call disp(triu(ones(4,4),1),"triu")
    end block

    block
        use forlab,only:argsort,sort,disp
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
    end block

    block
        use forlab,only:eig,chol,disp
        real::a(2,2)=reshape([real::0,1,1,0],[2,2])
        real,allocatable::v(:,:),e(:),L(:,:)
        call disp("eig/chol test")
        call eig(a,v,e,100)
        call disp(a,"a=")
        call disp(e,"e=")
        call disp(v,"v=")
        a=reshape([real::1,-2,-2,6],[2,2])
        call disp(a,"a=")
        L=chol(a)
        call disp(L,"L=")
    end block

    block
        use forlab, only: outer, disp
        real :: ra(3) = [1, 2, 3]
        real :: rb(4) = [1, 2, 3, 4]
        real, allocatable :: rX(:,:)
        integer :: ia(3) = [1, 2, 3]
        integer :: ib(4) = [1, 2, 3, 4]
        integer, allocatable :: iX(:,:)

        call disp("outer test")
        call disp("outer for real")
        rX = outer(ra, rb)
        call disp(ra, "a=")
        call disp(rb, "b=")
        call disp(rX, "a .outer. b=")
        call disp("outer for integer")
        iX = outer(ia, ib)
        call disp(ia, "a=")
        call disp(ib, "b=")
        call disp(iX, "a .outer. b=")
    end block

    block
        use forlab, only: disp,svd
        integer::i
        real(8) ::a(3,3)=reshape([real::(i,i=1,9)],shape(a),order=[1,2])
        real(8),allocatable::w(:),u(:,:),v(:,:)
        call disp("test Svd")
        call svd(a,w)
        call disp(a,"a=")
        call disp(w,"w=")
        call svd(a,w,u,v)
        call disp(a,"a=")
        call disp(w,"w=")
        call disp(u,"u=")
        call disp(v,"v=")
    end block

    block
        use forlab, only: disp,solve
        integer::i
        real(8) ::a(2,2)=reshape([1,2,2,1],shape(a))
        real(8) ::b(2)=[3,4]
        real(8) ::c(3,2)=reshape([1,1,2,4,2,3],shape(c))
        real(8) ::d(3)=[-2,6,1]
        call disp("Test solve")
        call disp(a,"a=")
        call disp(b,"b=")
        call disp(solve(a,b),"x=")
        call disp(c,"a=")
        call disp(d,"b=")
        call disp(solve(c,d),"x=")
    end block

    block
        use forlab, only: disp,randi
        call disp(randi(1),'x = randi(imax):')
        call disp(randi(-1),'x = randi(imax):')
        call disp(randi(10),'x = randi(imax):')
        call disp(randi([10, 100]),'x = randi([10, 100]):')
        call disp(randi([10, 100], 2, 3),'A = randi([10, 100], 2, 3):')
    end block

end program
