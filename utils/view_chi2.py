# -*- coding: utf-8 -*-
#==============================================================================
# view_chi2.py
#------------------------------------------------------------------------------
# view_chi2 checks that the standard deviation of a normal distribution follows
# a chi-square distribution.
#
# Created by
#       Keurfon Luu <keurfon.luu@mines-paristech.fr>
#       MINES ParisTech - Centre de Géosciences
#       PSL - Research University
#
# Last updated
#       2017-01-23 11:02
#==============================================================================

import numpy as np
import matplotlib.pyplot as plt

# Figure
#========
fig1 = plt.figure(figsize = (16, 7), facecolor = "white")
fig1.patch.set_alpha(0.)
ax1 = fig1.add_subplot(1, 2, 1)
ax2 = fig1.add_subplot(1, 2, 2)
ax1.patch.set_alpha(1.)
ax2.patch.set_alpha(1.)

# Normal random number standard deviation distribution
#======================================================
randn_to_chi2 = np.fromfile("../examples/rand/randn_to_chi2.bin", dtype = "float32")

ax1.hist(randn_to_chi2, 30, normed = True, color = "black", alpha = 0.8)
ax1.set_title("Normal random number standard deviation")
ax1.set_xlabel("std(X)")
ax1.set_ylabel("PDF")
ax1.grid(True)

# Chi-square distribution
#==========================
randchi2 = np.fromfile("../examples/rand/randchi2.bin", dtype = "float32")

ax2.hist(randchi2, 30, normed = True, color = "black", alpha = 0.8)
ax2.set_title("Chi-square distribution")
ax2.set_xlabel("Chi-square")
ax2.set_ylabel("PDF")
ax2.grid(True)