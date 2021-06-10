

submodule(forlab) forlab_qr
    use forlab_kinds
    !! the QR decomposition of square matrix
    !!
    !!## Syntax
    !!    call qr(a,q,r)
    !!    call qr(a,q,r,2)
    !!
    !!## Description
    !!
    !! `call qr(a,q,r)` returns the QR decomposition using Householder Transformation
    !!
    !! `call qr(a,q,r,2)` returns the QR decomposition using schmidt orthogonalization
    !!
    !! return dimension structure is 
    !!      l=1(default)  [m,n]-> [m,m]X[m,n]
    !!      l=2           [m,n]-> [m,n]X[n,n]

contains
    module procedure qr_sp
        real(sp),allocatable::v(:)
        integer::i,j,k,m,n,nn,templ
        real(sp)::alpha,t,u
        real(sp),parameter::eps=epsilon(1.0_sp),zero=0.0_sp,one=1.0_sp
        m=size(a,1)
        n=size(a,2)
        if (m<n) then
            error stop "Error:Matrix a dimension m < n "
        end if
        if(present(l))then
            templ=l
        else
            templ=1
        end if
        select case(templ)
        case(1)
            r=a
            allocate(q(m,m))
            q=zero
            r=a
            forall(i=1:m)
                q(i,i)=one
            end forall
            nn=n
            if (m==n) nn=m-1
            do  k=1,nn
                u=zero
                do i=k,m
                    if (abs(r(i,k))>u) u=abs(r(i,k))
                end do
                alpha=dot_product(r(k:m,k), r(k:m,k))/(u*u)
                if (r(k,k)>0) u=-u
                alpha=u*sqrt(alpha)
                if (abs(alpha)<eps) then
                    error stop "Error:matrix r linearly dependent"
                end if
                u=sqrt(2*alpha*(alpha-r(k,k)))
                if (abs(u)>eps) then
                    r(k,k)=(r(k,k)-alpha)/u
                    r(k+1:m,k)=r(k+1:m,k)/u
                    do j=1,m
                        t=dot_product(r(k:m,k),q(k:m,j))
                        q(k:m,j)=q(k:m,j)-2*r(k:m,k)*t
                    end do
                    do  j=k+1,n
                        t=dot_product(r(k:m,k),r(k:m,j))
                        r(k:m,j)=r(k:m,j)-2*r(k:m,k)*t
                    end do
                    r(k,k)=alpha
                    r(k+1:m,k)=zero
                end if
            end do
            do  i=1,m-1
                do  j=i+1,m
                    t=q(i,j)
                    q(i,j)=q(j,i)
                    q(j,i)=t
                end do
            end do
        case(2)
            allocate(r(n,n))
            r=zero
            q=a
            v=q(:,1)
            alpha=norm2(v)
            q(:,1)=v/alpha
            r(1,1)=alpha
            do i=2,n
                v=q(:,i)
                do j=1,i-1
                    alpha=dot_product(q(:,i), q(:,j))
                    v=v-alpha*q(:,j)
                    r(j,i)=alpha
                end do
                alpha=norm2(v)
                if(abs(alpha)<eps)then
                    error stop "Error:Matrix q linearly dependent"
                end if
                q(:,i)=v/alpha
                r(i,i)=alpha
            end do
        case default
            Error stop "Error: QR decomposition Type must be 1 or 2"
        end select
    end procedure qr_sp
    module procedure qr_dp
        real(dp),allocatable::v(:)
        integer::i,j,k,m,n,nn,templ
        real(dp)::alpha,t,u
        real(dp),parameter::eps=epsilon(1.0_dp),zero=0.0_dp,one=1.0_dp
        m=size(a,1)
        n=size(a,2)
        if (m<n) then
            error stop "Error:Matrix a dimension m < n "
        end if
        if(present(l))then
            templ=l
        else
            templ=1
        end if
        select case(templ)
        case(1)
            r=a
            allocate(q(m,m))
            q=zero
            r=a
            forall(i=1:m)
                q(i,i)=one
            end forall
            nn=n
            if (m==n) nn=m-1
            do  k=1,nn
                u=zero
                do i=k,m
                    if (abs(r(i,k))>u) u=abs(r(i,k))
                end do
                alpha=dot_product(r(k:m,k), r(k:m,k))/(u*u)
                if (r(k,k)>0) u=-u
                alpha=u*sqrt(alpha)
                if (abs(alpha)<eps) then
                    error stop "Error:matrix r linearly dependent"
                end if
                u=sqrt(2*alpha*(alpha-r(k,k)))
                if (abs(u)>eps) then
                    r(k,k)=(r(k,k)-alpha)/u
                    r(k+1:m,k)=r(k+1:m,k)/u
                    do j=1,m
                        t=dot_product(r(k:m,k),q(k:m,j))
                        q(k:m,j)=q(k:m,j)-2*r(k:m,k)*t
                    end do
                    do  j=k+1,n
                        t=dot_product(r(k:m,k),r(k:m,j))
                        r(k:m,j)=r(k:m,j)-2*r(k:m,k)*t
                    end do
                    r(k,k)=alpha
                    r(k+1:m,k)=zero
                end if
            end do
            do  i=1,m-1
                do  j=i+1,m
                    t=q(i,j)
                    q(i,j)=q(j,i)
                    q(j,i)=t
                end do
            end do
        case(2)
            allocate(r(n,n))
            r=zero
            q=a
            v=q(:,1)
            alpha=norm2(v)
            q(:,1)=v/alpha
            r(1,1)=alpha
            do i=2,n
                v=q(:,i)
                do j=1,i-1
                    alpha=dot_product(q(:,i), q(:,j))
                    v=v-alpha*q(:,j)
                    r(j,i)=alpha
                end do
                alpha=norm2(v)
                if(abs(alpha)<eps)then
                    error stop "Error:Matrix q linearly dependent"
                end if
                q(:,i)=v/alpha
                r(i,i)=alpha
            end do
        case default
            Error stop "Error: QR decomposition Type must be 1 or 2"
        end select
    end procedure qr_dp
    module procedure qr_qp
        real(qp),allocatable::v(:)
        integer::i,j,k,m,n,nn,templ
        real(qp)::alpha,t,u
        real(qp),parameter::eps=epsilon(1.0_qp),zero=0.0_qp,one=1.0_qp
        m=size(a,1)
        n=size(a,2)
        if (m<n) then
            error stop "Error:Matrix a dimension m < n "
        end if
        if(present(l))then
            templ=l
        else
            templ=1
        end if
        select case(templ)
        case(1)
            r=a
            allocate(q(m,m))
            q=zero
            r=a
            forall(i=1:m)
                q(i,i)=one
            end forall
            nn=n
            if (m==n) nn=m-1
            do  k=1,nn
                u=zero
                do i=k,m
                    if (abs(r(i,k))>u) u=abs(r(i,k))
                end do
                alpha=dot_product(r(k:m,k), r(k:m,k))/(u*u)
                if (r(k,k)>0) u=-u
                alpha=u*sqrt(alpha)
                if (abs(alpha)<eps) then
                    error stop "Error:matrix r linearly dependent"
                end if
                u=sqrt(2*alpha*(alpha-r(k,k)))
                if (abs(u)>eps) then
                    r(k,k)=(r(k,k)-alpha)/u
                    r(k+1:m,k)=r(k+1:m,k)/u
                    do j=1,m
                        t=dot_product(r(k:m,k),q(k:m,j))
                        q(k:m,j)=q(k:m,j)-2*r(k:m,k)*t
                    end do
                    do  j=k+1,n
                        t=dot_product(r(k:m,k),r(k:m,j))
                        r(k:m,j)=r(k:m,j)-2*r(k:m,k)*t
                    end do
                    r(k,k)=alpha
                    r(k+1:m,k)=zero
                end if
            end do
            do  i=1,m-1
                do  j=i+1,m
                    t=q(i,j)
                    q(i,j)=q(j,i)
                    q(j,i)=t
                end do
            end do
        case(2)
            allocate(r(n,n))
            r=zero
            q=a
            v=q(:,1)
            alpha=norm2(v)
            q(:,1)=v/alpha
            r(1,1)=alpha
            do i=2,n
                v=q(:,i)
                do j=1,i-1
                    alpha=dot_product(q(:,i), q(:,j))
                    v=v-alpha*q(:,j)
                    r(j,i)=alpha
                end do
                alpha=norm2(v)
                if(abs(alpha)<eps)then
                    error stop "Error:Matrix q linearly dependent"
                end if
                q(:,i)=v/alpha
                r(i,i)=alpha
            end do
        case default
            Error stop "Error: QR decomposition Type must be 1 or 2"
        end select
    end procedure qr_qp
end submodule
