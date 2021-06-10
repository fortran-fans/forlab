submodule(forlab) forlab_degcir
    !! Version: experimental
    !! degree circular functions
    !!## acosd
    !! acosd computes the inverse cosine in degrees.
    !!([Interface](../interface/acosd.html))
    !!
    !!### Syntax
    !!    y = acosd(x)
    !!
    !!### Description
    !! `y = acosd(x)` returns the inverse cosine of the elements in x in
    !! degrees. For real elements of x in the domain [-1,1], acosd returns
    !! values in the range [0,180]. For values of x outside this range,
    !! acosd returns NaN (Not a Number).
    !!
    !!### Examples
    !!    y = acosd(1.)
    !!        1.
    !!
    !!    y = acosd(2.)
    !!        NaN
    !!
    !!    x = [ -1., 0., 1. ]
    !!    y = acosd(x)
    !!        180.  90.   0.
    !!## asind
    !!
    !! asind computes the inverse sine in degrees.
    !!
    !!### Syntax
    !!    y = asind(x)
    !!
    !!### Description
    !! `y = asind(x)` returns the inverse sine of the elements in x in degrees.
    !! For real elements of x in the domain [-1,1], asind returns values in
    !! the range [-90,90]. For values of x outside this range, asind returns
    !! NaN (Not a Number).
    !!
    !!### Examples
    !!    y = asind(1.)
    !!        90.
    !!
    !!    y = asind(2.)
    !!        NaN
    !!
    !!    x = [ -1., 0., 1. ]
    !!    y = asind(x)
    !!        -90.  0.  90.
    !!## atand
    !!
    !! atand computes the inverse tangent in degrees.
    !!
    !!### Syntax
    !!     y = atand(x)
    !!
    !!### Description
    !! `y = atand(x)` returns the inverse tangent of the elements in x in
    !! degrees. For real elements of x in the domain [-Inf,Inf], atand
    !! returns values in the range [-90,90].
    !!
    !!### Examples
    !!     y = atand(0.)
    !!         0.
    !!
    !!     y = atand(50.)
    !!         88.8542328
    !!
    !!     x = [ -50., 0., 50. ]
    !!     y = atand(x)
    !!         -88.8542328   0.  88.8542328
    !!## sind
    !! sind computes the sine of argument in degrees.
    !!
    !!### Syntax
    !!
    !!      y = sind(x)
    !!
    !!### Description
    !!
    !!  `y = sind(x)` returns the sine of the elements in x, which are expressed
    !! in degrees.
    !!
    !!### Examples
    !!
    !!      y = sind(90.)
    !!          1.
    !!
    !!      x = [ 0., 90., 180., 270. ]
    !!      y = sind(x)
    !!            0.  1.  0.  -1.
    !!## cosd
    !! cosd computes the cosine of argument in degrees.
    !!
    !!### Syntax
    !!
    !!      y = cosd(x)
    !!
    !!### Description
    !!
    !!  `y = cosd(x)` returns the cosine of the elements in x, which are
    !! expressed in degrees.
    !!
    !!### Examples
    !!
    !!      y = cosd(0.)
    !!          1.
    !!
    !!      x = [ 0., 90., 180., 270. ]
    !!      y = cosd(x)
    !!          1.  0. -1.  0.
    !!## tand
    !!
    !! tand computes the tangent of argument in degrees.
    !!
    !!### Syntax
    !!
    !!      y = tand(x)
    !!
    !!### Description
    !!
    !!  `y = tand(x)` returns the tangent of the elements in x, which are
    !! expressed in degrees.
    !!
    !!### Examples
    !!
    !!      y = tand(0.)
    !!           0.
    !!
    !!      x = [ 0., 90., 180., 270. ]
    !!      y = tand(x)
    !!           0.  Inf   0.  -Inf
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
