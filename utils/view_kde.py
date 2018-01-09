# -*- coding: utf-8 -*-

"""
Author: Keurfon Luu <keurfon.luu@mines-paristech.fr>
License: MIT
"""

import numpy as np
import matplotlib.pyplot as plt


if __name__ == "__main__":
    # Parameters
    fkind = "float32"
    
    # Initialize figure
    fig = plt.figure(figsize = (10, 5), facecolor = "white")
    fig.patch.set_alpha(0.)
    ax1 = fig.add_subplot(1, 2, 1)
    ax2 = fig.add_subplot(1, 2, 2)
    ax1.patch.set_alpha(1.)
    ax2.patch.set_alpha(1.)
    
    # 1D Kernel Density Estimation
    data1d = np.fromfile("../examples/rand/data1d.bin", dtype = fkind)
    data1d_kde = np.fromfile("../examples/rand/data1d_kde.bin", dtype = fkind)
    xi = np.fromfile("../examples/rand/data1d_kde_xaxis.bin", dtype = fkind)
    
    n = 1500
    nx = len(xi)

    ax1.hist(data1d, 30, normed = True, color = "black", alpha = 0.8)
    ax1.plot(xi, data1d_kde, color = "red", linewidth = 2)
    ax1.set_title("1D KDE")
    ax1.set_xlabel("X")
    ax1.set_ylabel("PDF")
    ax1.set_xlim(np.min(xi), np.max(xi))
    ax1.grid(True, linestyle = ":")
    
    # 2D Kernel Density Estimation
    data2d = np.fromfile("../examples/rand/data2d.bin", dtype = fkind)
    data2d_kde = np.fromfile("../examples/rand/data2d_kde.bin", dtype = fkind)
    xi = np.fromfile("../examples/rand/data2d_kde_xaxis.bin", dtype = fkind)
    yi = np.fromfile("../examples/rand/data2d_kde_yaxis.bin", dtype = fkind)
    
    n = 750
    nx, ny = len(xi), len(yi)
    data2d = np.reshape(data2d, (n, 2), order = "F")
    data2d_kde = np.reshape(data2d_kde, (nx, ny), order = "F")
    
    ax2.contour(xi, yi, data2d_kde.T, 30)
    ax2.plot(data2d[:,0], data2d[:,1], color = "black", linestyle = "none", marker = "o", markersize = 6, alpha = 0.33)
    ax2.set_title("2D KDE")
    ax2.set_xlabel("X")
    ax2.set_ylabel("Y")
    ax2.set_xlim(np.min(xi), np.max(xi))
    ax2.set_ylim(np.min(yi), np.max(yi))
    ax2.grid(True, linestyle = ":")
    
    fig.tight_layout()