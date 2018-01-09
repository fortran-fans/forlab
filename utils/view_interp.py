# -*- coding: utf-8 -*-

"""
Author: Keurfon Luu <keurfon.luu@mines-paristech.fr>
License: MIT
"""

import numpy as np
import matplotlib.pyplot as plt


if __name__ == "__main__":
    # True grid
    true = np.loadtxt("../examples/interp/true.txt", unpack = True)
    
    # Initialize figure
    fig = plt.figure(figsize = (12, 8), facecolor = "white")
    fig.patch.set_alpha(0.)
    ax1 = fig.add_subplot(2, 3, 1)
    ax2 = fig.add_subplot(2, 3, 2)
    ax3 = fig.add_subplot(2, 3, 3)
    ax4 = fig.add_subplot(2, 3, 4)
    ax5 = fig.add_subplot(2, 3, 5)
    ax6 = fig.add_subplot(2, 3, 6)
    ax1.patch.set_alpha(1.)
    ax2.patch.set_alpha(1.)
    ax3.patch.set_alpha(1.)
    ax4.patch.set_alpha(1.)
    ax5.patch.set_alpha(1.)
    ax6.patch.set_alpha(1.)
    
    # Bilinear interpolation
    lin2 = np.loadtxt("../examples/interp/linear2.txt", unpack = True)
    ax1.contourf(lin2, 200)
    ax4.contourf(np.abs(true - lin2), 200)
    ax1.set_title("Bilinear", fontsize = 14)
    ax1.set_ylabel("Interpolated", fontsize = 14)
    ax4.set_ylabel("Residuals", fontsize = 14)
    ax1.set_xticklabels("")
    ax1.set_yticklabels("")
    ax4.set_xticklabels("")
    ax4.set_yticklabels("")

    # Bicubic spline interpolation
    spl2 = np.loadtxt("../examples/interp/spline2.txt", unpack = True)
    ax2.contourf(spl2, 200)
    ax5.contourf(np.abs(true - spl2), 200)
    ax2.set_title("Bicubic spline", fontsize = 14)
    ax2.set_xticklabels("")
    ax2.set_yticklabels("")
    ax5.set_xticklabels("")
    ax5.set_yticklabels("")
    
    # 2D B-spline approximation
    bspl2 = np.loadtxt("../examples/interp/bspline2.txt", unpack = True)
    ax3.contourf(bspl2, 200)
    ax6.contourf(np.abs(true - bspl2), 200)
    ax3.set_title("2D B-spline", fontsize = 14)
    ax3.set_xticklabels("")
    ax3.set_yticklabels("")
    ax6.set_xticklabels("")
    ax6.set_yticklabels("")
    
    fig.tight_layout()