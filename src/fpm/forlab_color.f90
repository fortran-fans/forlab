module forlab_color
    use iso_c_binding, only: c_long

    interface
        subroutine setcolor(x) bind(c, name="color")
            import c_long
            integer(c_long), value, intent(in) :: x
        end subroutine
    end interface

contains
end module forlab_color
