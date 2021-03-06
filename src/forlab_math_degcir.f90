
submodule(forlab_math) forlab_math_degcir

    implicit none
    real(sp), parameter ::pi_sp = acos(-1.0_sp)
    real(dp), parameter ::pi_dp = acos(-1.0_dp)

contains

    module procedure acosd_sp
    acosd_sp = acos(x)*180/pi_sp
    end procedure

    module procedure acosd_dp
    acosd_dp = acos(x)*180/pi_dp
    end procedure

    module procedure asind_sp
    asind_sp = asin(x)*180/pi_sp
    end procedure

    module procedure asind_dp
    asind_dp = asin(x)*180/pi_dp
    end procedure

    module procedure atand_sp
    atand_sp = atan(x)*180/pi_sp
    end procedure

    module procedure atand_dp
    atand_dp = atan(x)*180/pi_dp
    end procedure

    module procedure cosd_sp
    cosd_sp = cos(x*pi_sp/180)
    end procedure

    module procedure cosd_dp
    cosd_dp = cos(x*pi_dp/180)
    end procedure

    module procedure sind_sp
    sind_sp = sin(x*pi_sp/180)
    end procedure

    module procedure sind_dp
    sind_dp = sin(x*pi_dp/180)
    end procedure

    module procedure tand_sp
    tand_sp = tan(x*pi_sp/180)
    end procedure

    module procedure tand_dp
    tand_dp = tan(x*pi_dp/180)
    end procedure

end submodule forlab_math_degcir
