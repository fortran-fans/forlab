# -*- coding: utf-8 -*-

"""
Author: Keurfon Luu <keurfon.luu@mines-paristech.fr>
License: MIT
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
import matplotlib.cm as cm


if __name__ == "__main__":
    # Parameters
    cmap = "viridis"
    
    # Initialize figure
    fig = plt.figure(figsize = (9, 6), facecolor = "white")
    fig.patch.set_alpha(0.)
    gs = GridSpec(2, 3)
    ax1 = fig.add_subplot(gs[:,:2])
    ax2 = fig.add_subplot(gs[0,2])
    ax3 = fig.add_subplot(gs[1,2])
    
    # Clustering result
    A = np.loadtxt("../examples/cluster2d/data.txt")
    mu = np.loadtxt("../examples/cluster2d/means.txt")
    idx = np.loadtxt("../examples/cluster2d/index.txt", dtype = int)
    n = A.shape[0]
    k = mu.shape[0]
    n_clusters = np.arange(k) / ( k - 1 )
    col = eval("cm.%s(n_clusters)" % cmap)
    for i, a in enumerate(A):
        ax1.scatter(a[0], a[1], s = 30, facecolor = col[idx[i]-1])
    for m in mu:
        ax1.scatter(m[0], m[1], s = 400, marker = "*", facecolor = "white", edgecolor = "black")
    ax1.set_xlabel("X")
    ax1.set_ylabel("Y")
    ax1.grid(True, linestyle = ":")
        
    # Average silhouette
    sil = np.loadtxt("../examples/cluster2d/silhouette.txt")
    kmax = len(sil)
    ak = np.arange(kmax) + 1
    ax2.plot(ak[1:], sil[1:], color = "black", linewidth = 2, marker = "o", markerfacecolor = "black")
    ax2.set_ylim(0, 1)
    ax2.set_xlabel("# clusters")
    ax2.set_ylabel("Average silhouette")
    ax2.grid(True, linestyle = ":")
    
    # Davies-Bouldin index
    db = np.loadtxt("../examples/cluster2d/db.txt")
    kmax = len(db)
    ak = np.arange(kmax) + 1
    ax3.plot(ak[1:], db[1:], color = "black", linewidth = 2, marker = "o", markerfacecolor = "black")
    ax3.set_xlabel("# cluster")
    ax3.set_ylabel("Davies-Bouldin index")
    ax3.grid(True, linestyle = ":")
    
    fig.tight_layout()