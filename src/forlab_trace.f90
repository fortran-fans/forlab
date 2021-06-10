


submodule(forlab) forlab_trace
    use forlab_kinds

contains
    module procedure trace_sp
        trace = sum(diag(A))
    end procedure trace_sp
    module procedure trace_dp
        trace = sum(diag(A))
    end procedure trace_dp
    module procedure trace_qp
        trace = sum(diag(A))
    end procedure trace_qp

end submodule

