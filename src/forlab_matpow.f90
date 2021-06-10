


submodule(forlab) forlab_matpow
    use forlab_kinds
    !! calculat matrix power
    !!
    !!## Syntax
    !!    c = matpow(a,100)
    !!
    !!## Description
    !! `c = matpow(a,num)` returns the `num`th power of matrix `a`

contains
    module procedure matpow_sp
        real(sp),allocatable :: a1(:,:)
        real(sp),parameter::zero=0.0_sp,one=1.0_sp
        integer::i,n,m
        if(.not. issquare(a))then
            error stop "Error:A must be a square matrix"
        end if
        m=num
        if(m<0)then
            error stop "Error: num must be a positive number"
        end if
        n=size(a,1)
        allocate(a1(n,n),source=a)
        allocate(c(n,n))
        c=zero
        forall(i=1:n)
            c(i,i)=one
        end forall
        do
            if(mod(m,2)==1)then
                c=matmul(c,a1)
            end if
            m=shiftr(m, 1)
            if(m==0)exit
            a1=matmul(a1,a1)
        end do
        deallocate(a1)
    end procedure matpow_sp

    module procedure matpow_dp
        real(dp),allocatable :: a1(:,:)
        real(dp),parameter::zero=0.0_dp,one=1.0_dp
        integer::i,n,m
        if(.not. issquare(a))then
            error stop "Error:A must be a square matrix"
        end if
        m=num
        if(m<0)then
            error stop "Error: num must be a positive number"
        end if
        n=size(a,1)
        allocate(a1(n,n),source=a)
        allocate(c(n,n))
        c=zero
        forall(i=1:n)
            c(i,i)=one
        end forall
        do
            if(mod(m,2)==1)then
                c=matmul(c,a1)
            end if
            m=shiftr(m, 1)
            if(m==0)exit
            a1=matmul(a1,a1)
        end do
        deallocate(a1)
    end procedure matpow_dp

    module procedure matpow_qp
        real(qp),allocatable :: a1(:,:)
        real(qp),parameter::zero=0.0_qp,one=1.0_qp
        integer::i,n,m
        if(.not. issquare(a))then
            error stop "Error:A must be a square matrix"
        end if
        m=num
        if(m<0)then
            error stop "Error: num must be a positive number"
        end if
        n=size(a,1)
        allocate(a1(n,n),source=a)
        allocate(c(n,n))
        c=zero
        forall(i=1:n)
            c(i,i)=one
        end forall
        do
            if(mod(m,2)==1)then
                c=matmul(c,a1)
            end if
            m=shiftr(m, 1)
            if(m==0)exit
            a1=matmul(a1,a1)
        end do
        deallocate(a1)
    end procedure matpow_qp


end submodule

