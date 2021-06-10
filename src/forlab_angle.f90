!! This file contains the multi-precision `arange` function implementation.

submodule(forlab) forlab_angle
    !! Version: experimental
    !! angle compute the phase angle.
    !!([Interface](../interface/angle.html))
    !!
    !!### Syntax
    !!    p = angle(z)
    !!    P = angle(Z)
    !!
    !!### Description
    !! `p = angle(z)` returns the phase angle in radians of the complex
    !! number `z`.
    !!
    !! `P = angle(Z)` returns the phase angles in radians of each complex
    !! numbers in vector `Z`.
    use forlab_kinds
    implicit none

contains
    
    module procedure angle_sp
        angle_sp = imag(log(z))
    end procedure

    module procedure angle_dp
        angle_dp = imag(log(z))
    end procedure

    module procedure angle_qp
        angle_qp = imag(log(z))
    end procedure


end submodule
