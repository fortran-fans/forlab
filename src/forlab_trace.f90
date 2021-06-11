


submodule(forlab) forlab_trace
    !! trace computes the sum of diagonal elements.
    !!
    !!## Syntax
    !!
    !!      x = trace(A)
    !!
    !!## Description
    !!
    !! `x = trace(A)` returns the sum of the elements on the main diagonal of
    !! the matrix A.
    !!
    !!## Examples
    !!
    !!      A = eye(3)
    !!      x = trace(A)
    !!          3.
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

