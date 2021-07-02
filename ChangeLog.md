> Please refer to [gnu changelog](https://www.gnu.org/prep/standards/html_node/Change-Logs.html) for the format of `ChangeLog.md`.

2021-06-05  zoziha  zuo.zhihua@qq.com

        Provide multi-precision functions, set submodule files.

        * src/fypp/common.fypp:
        * src/fypp/forlb_angle.fypp:
        * src/fypp/forlab_arange.fypp:
        * src/fypp/forlab_det.fypp:
        * src/fypp/forlab_disp.fypp:
        * src/fypp/forlab_datenum.fypp: 
        * src/fypp/forlab_empty.fypp:
        * src/fypp/forlab_eye.fypp:
        * src/fypp/forlab_file.fypp:
        * src/fypp/forlab_inv.fypp: 
        * src/fypp/forlab_issquare.fypp:
        * src/fypp/forlab_kinds.fypp:
        * src/fypp/forlab_lu.fypp:
        * src/fypp/forlab_linspace.fypp:
        * src/fypp/forlab_load.fypp:
        * src/fypp/forlab_mean.fypp: (#10 PR)
        * src/fypp/forlab_num2str.fypp: (#10 PR)
        * src/fypp/forlab_ones.fypp:
        * src/fypp/forlab_operator_x.fypp: matmul
        * src/fypp/forlab_randi.fypp: integer(4)
        * src/fypp/forlab_randn.fypp:
        * src/fypp/forlab_randu.fypp:
        * src/fypp/forlab_rng.fypp:
        * src/fypp/forlab_save.fypp:
        * src/fypp/forlab_tioc.fypp: attention precision changes (#10 PR)
        * src/fypp/forlab_var.fypp:
        * src/fypp/forlab_zeros.fypp:
        * src/fypp/forlab.fypp: Modify the interface of the above functions
        * src/fypp/Makefile: make it more smart. (#9 issue, #10 PR)
        * src/fypp/README.md:
        * src/fypp_test/forlab.fypp:
        * src/fypp_test/forlab_load.fypp: 

2021-06-08  Euler-37

        Provide multi-precision functions, set submodule files.

        * src/fypp/forlab_argsort.fypp:
        * src/fypp/forlab_chol.fypp:
        * src/fypp/forlab_degcir.fypp:
        * src/fypp/forlab_eig.fypp:
        * src/fypp/forlab_issymmertric.fypp:
        * src/fypp/forlab_qr.fypp:
        * src/fypp/forlab_matpow.fypp:
        * src/fypp/forlab_norm.fypp:
        * src/fypp/forlab_sort.fypp:
        * src/fypp/forlab_solve.fypp:
        * src/fypp/forlab_svd.fypp:
        * src/fypp/forlab_svdsolve.fypp:
        * src/fypp/forlab_trace.fypp:
        * src/fypp/forlab_tri.fypp:

2021-06-09  St-Maxwell

        Provide multi-precision functions, set submodule files.

        * src/fypp/forlab_outer.fypp:

2021-07-02  zoziha  zuo.zhihua@qq.com

        Uniform style with stdlib, refactor code.

        * src/forlab_fpm/*.f90 *.c
        * src/common.fypp
        * src/forlab_.f95
        * src/forlab_io.fypp
        * src/forlab_io_bin.fypp
        * src/forlab_io_disp_.fypp
        * src/forlab_io_txt.fypp
        * src/forlab_linalg.fypp
        * src/forlab_linalg_cat.fypp
        * src/forlab_linalg_chol.fypp
        * src/forlab_linalg_det.fypp
        * src/forlab_linalg_diag.fypp
        * src/forlab_linalg_eig.fypp
        * src/forlab_linalg_eye.fypp
        * src/forlab_linalg_inv.fypp
        * src/forlab_linalg_linspace.fypp
        * src/forlab_linalg_lu.fypp
        * src/forlab_linalg_matpow.fypp
        * src/forlab_linalg_ones.fypp
        * src/forlab_linalg_outer.fypp
        * src/forlab_linalg_qr.fypp
        * src/forlab_linalg_seq.fypp
        * src/forlab_linalg_solve.fypp
        * src/forlab_linalg_svd.fypp
        * src/forlab_linalg_svdsolve.fypp
        * src/forlab_linalg_tri.fypp
        * src/forlab_linalg_zeros.fypp
        * src/forlab_math.fypp
        * src/forlab_math_degcir.fypp
        * src/forlab_sorting.fypp
        * src/forlab_sorting_argsort.fypp
        * src/forlab_sorting_sort.fypp
        * src/forlab_stats.fypp
        * src/forlab_stats_chi.fypp
        * src/forlab_stats_mean.fypp
        * src/forlab_stats_norm.fypp
        * src/forlab_stats_rand.fypp
        * src/forlab_stats_rng.fypp
        * src/forlab_stats_var.fypp
        * src/forlab_strings_.fypp
        * src/forlab_strings_format_string_.fypp
        * src/forlab_strings_progress.fypp
        * src/forlab_time.fypp
        * src/forlab_time_datenum.fypp
        * src/forlab_time_tioc.fypp
        * src/Makefile