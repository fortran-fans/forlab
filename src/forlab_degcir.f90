submodule(forlab) forlab_degcir
    !! Version: experimental
    !! degree circular functions
    !!
    !!## Discussion:
    !! https://fortran-lang.discourse.group/t/fortran-function-return-value-polymorphism/1350/5
    use forlab_kinds
    implicit none
contains
    
    module procedure acosd_sp
        acosd_sp=acos(x)*180/pi_sp
    end procedure

    module procedure acosd_dp
        acosd_dp=acos(x)*180/pi_dp
    end procedure

    module procedure acosd_qp
        acosd_qp=acos(x)*180/pi_qp
    end procedure

    module procedure asind_sp
        asind_sp=asin(x)*180/pi_sp
    end procedure

    module procedure asind_dp
        asind_dp=asin(x)*180/pi_dp
    end procedure

    module procedure asind_qp
        asind_qp=asin(x)*180/pi_qp
    end procedure

    module procedure atand_sp
        atand_sp=atan(x)*180/pi_sp
    end procedure

    module procedure atand_dp
        atand_dp=atan(x)*180/pi_dp
    end procedure

    module procedure atand_qp
        atand_qp=atan(x)*180/pi_qp
    end procedure

    ! sind
    !-----------------------------------------------------------------------
    ! sind computes the sine of argument in degrees.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! y = sind(x)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! y = sind(x) returns the sine of the elements in x, which are expressed
    ! in degrees.
    !
    ! Examples
    !-----------------------------------------------------------------------
    ! y = sind(90.)
    !     1.
    !
    ! x = [ 0., 90., 180., 270. ]
    ! y = sind(x)
    !     0.  1.  0.  -1.

    ! cosd
    !-----------------------------------------------------------------------
    ! cosd computes the cosine of argument in degrees.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! y = cosd(x)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! y = cosd(x) returns the cosine of the elements in x, which are
    ! expressed in degrees.
    !
    ! Examples
    !-----------------------------------------------------------------------
    ! y = cosd(0.)
    !     1.
    !
    ! x = [ 0., 90., 180., 270. ]
    ! y = cosd(x)
    !     1.  0. -1.  0.

    ! tand
    !-----------------------------------------------------------------------
    ! tand computes the tangent of argument in degrees.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! y = tand(x)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! y = tand(x) returns the tangent of the elements in x, which are
    ! expressed in degrees.
    !
    ! Examples
    !-----------------------------------------------------------------------
    ! y = tand(0.)
    !     0.
    !
    ! x = [ 0., 90., 180., 270. ]
    ! y = tand(x)
    !     0.  Inf   0.  -Inf
    module procedure cosd_sp
        cosd_sp=cos(x*pi_sp/180)
    end procedure

    module procedure cosd_dp
        cosd_dp=cos(x*pi_dp/180)
    end procedure

    module procedure cosd_qp
        cosd_qp=cos(x*pi_qp/180)
    end procedure

    module procedure sind_sp
        sind_sp=sin(x*pi_sp/180)
    end procedure

    module procedure sind_dp
        sind_dp=sin(x*pi_dp/180)
    end procedure

    module procedure sind_qp
        sind_qp=sin(x*pi_qp/180)
    end procedure

    module procedure tand_sp
        tand_sp=tan(x*pi_sp/180)
    end procedure

    module procedure tand_dp
        tand_dp=tan(x*pi_dp/180)
    end procedure

    module procedure tand_qp
        tand_qp=tan(x*pi_qp/180)
    end procedure

end submodule
