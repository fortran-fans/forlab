!! `fpm test` or `fpm test test`
program main
    block
        use forlab, only: disp, operator(.x.)
        real, dimension(:, :), allocatable :: x_sp, y_sp
        real(8), dimension(:, :), allocatable :: x_dp, y_dp
        real(16), dimension(:, :), allocatable :: x_qp, y_qp

        allocate(x_sp(2,2), &
                 x_dp(2,2), &
                 x_qp(2,2), &
                 y_sp(2,2), &
                 y_dp(2,2), &
                 y_qp(2,2))

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
        real(8), allocatable :: x(:)

        call disp('----------------------------')
        call tic()
        allocate(x(1000))
        call randn(x)
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
        use forlab, only: linspace, disp, pi, seq
        real :: X(5)
        real*8, allocatable :: Y(:)
        call disp('-----------------------')
        call linspace(X, 1., 3.)
        call disp(X, 'linspace 1:')
        allocate(Y(3))
        call linspace(Y, 1.d0, 3.d0)
        call disp(Y, 'linspace 2:')
        call seq(Y, 1.1d0, pi, 0.5d0)
        call disp(Y, 'seq:')
    end block

    block
        use forlab, only: disp, savebin, loadbin, rng, randn, savetxt, loadtxt
        real*8, allocatable :: x(:)
        real*8, allocatable :: y(:)
        call disp('---------------------')
        call rng()
        allocate(X(5))
        call randn(X)
        call disp(x,'call randn(X):')
        call savebin('DP.bin', x)
        call savetxt('DP.txt', x)
        call loadtxt('DP.txt', y)
        call disp(y,'read from DP.txt')
        call loadbin('DP.bin', y)
        call disp(y,'read from DP.bin')
    end block

    block
        use forlab,only:tril,triu,ones,disp
        real::X(4,4)
        call disp("tri U L test")
        call ones(X)
        call disp(X,"A=")
        call disp(tril(X),"tril")
        call disp(triu(X),"triu")
        call disp(tril(X,-1),"tril")
        call disp(triu(X,1),"triu")
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
        use forlab, only: disp,randu
        integer :: i
        integer, allocatable :: iX(:)
        call randu(i, -1, 1)
        call disp(i, 'randu(i):')
        allocate(iX(4))
        call randu(iX(:))
        call disp(iX, 'randu(iX):')
    end block

    block
        use forlab, only: disp,logspace
        real :: X(3)
        call logspace(X, 1., 3.)
        call disp(X, 'call logspace(X, 1., 3.)')
    end block

    block
        use forlab, only: disp,var,randn,rng,mean,std
        real, allocatable :: x(:)
        call rng()
        allocate(X(5))
        call randn(X)
        call disp(x,'randn(n)')
        call disp(mean(x),'mean(randn(n)):')
        call disp(var(x),'var(randn(n)):')
        call disp(std(x), 'std(randn(n)):')
        if(allocated(X)) deallocate(X)
        allocate(X(4))  !!\FIXME:
        call randn(X,10.,1.0)
        call disp(X,'call randn(X,10.,1.0)')
    end block

    block
        use forlab, only: disp,qr
        real(8) ::a(4,3)=reshape([1.0,2.0,1.0,-1.0,1.0,1.0,-1.0,2.0,-1.0,0.0,0.0,1.0],shape(a))
        real(8),allocatable ::q(:,:),r(:,:)
        real(8),allocatable ::q1(:,:),r1(:,:)
        call disp("Test QR")
        call disp(a,"a=")
        call qr(a,q,r)
        call disp(q,"q=")
        call disp(r,"r=")
        call qr(a,q1,r1,2)
        call disp(q1,"q1=")
        call disp(r1,"r1=")
    end block

    block
        use forlab, only: disp,matpow
        real(8) ::a(3,3)=reshape([1,0,0,0,1,0,1,0,1],shape(a))
        real(8),allocatable ::c(:,:)
        call disp("Test matpow")
        call disp(a,"a=")
        c=matpow(a,0)
        call disp(c,"c=")
    end block

    block
        use forlab, only: disp,vertcat,progress_bar
        real*8, dimension(:,:), allocatable :: A1, A2, A
        A1 = reshape([ 1., 2., 3., 4. ], [ 2, 2 ], order = [ 2, 1 ])
        A2 = reshape([ 5., 6., 7., 8. ], [ 2, 2 ], order = [ 2, 1 ])
        A = vertcat(A1, A2)
        call disp(A,"A=")
        A2 = reshape([ 5., 6., 7., 8. ], [ 2, 1 ], order = [ 1, 2 ])
        A = vertcat(A1, A2)
        call disp(A,"A=")
        ! call progress_bar(57,100)
        ! call sleep(2)
        ! call progress_bar(89,100)
        ! call sleep(3)
        ! call progress_bar(100,100)
        ! call disp()
    end block

end program
