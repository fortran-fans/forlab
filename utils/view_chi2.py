# -*- coding: utf-8 -*-

"""
Author: Keurfon Luu <keurfon.luu@mines-paristech.fr>
License: MIT
"""

import numpy as np
import matplotlib.pyplot as plt


if __name__ == "__main__":
    # Parameters
    fkind = "float64"
    
    # Initialize figure
    fig = plt.figure(figsize = (10, 5), facecolor = "white")
    fig.patch.set_alpha(0.)
    ax1 = fig.add_subplot(1, 2, 1)
    ax2 = fig.add_subplot(1, 2, 2)
    ax1.patch.set_alpha(1.)
    ax2.patch.set_alpha(1.)
    
    # Normal random number standard deviation distribution
    randn_to_chi2 = np.fromfile("../examples/rand/randn_to_chi2.bin", dtype = fkind)
    ax1.hist(randn_to_chi2, 30, normed = True, color = "black", alpha = 0.8)
    ax1.set_title("Normal random number standard deviation")
    ax1.set_xlabel("std(X)")
    ax1.set_ylabel("PDF")
    ax1.grid(True, linestyle = ":")
    
    # Chi-square distribution
    randchi2 = np.fromfile("../examples/rand/randchi2.bin", dtype = fkind)
    ax2.hist(randchi2, 30, normed = True, color = "black", alpha = 0.8)
    ax2.set_title("Chi-square distribution")
    ax2.set_xlabel("Chi-square")
    ax2.set_ylabel("PDF")
    ax2.grid(True, linestyle = ":")
    
    fig.tight_layout()