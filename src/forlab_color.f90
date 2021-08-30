module forlab_color
    
    ! Some parameters for our ANSI escape codes
    character(*), parameter :: esc     = achar(27) ! Escape character.
    character(*), parameter :: default = esc // '[0m' ! Terminates an ANSI code.
    ! Foreground(font) Colours
    character(*), parameter :: red     = esc // '[31m'
    character(*), parameter :: green   = esc // '[32m'
    character(*), parameter :: yellow  = esc // '[33m'
    character(*), parameter :: blue    = esc // '[34m'
    character(*), parameter :: magenta = esc // '[35m'
    character(*), parameter :: cyan    = esc // '[36m'
    character(*), parameter :: grey    = esc // '[90m' !Bright-Black 
    ! One background colour
    character(*), parameter :: background_green = esc // '[42m'
    ! Some other formatting
    character(*), parameter :: bold       = esc // '[1m'
    character(*), parameter :: bold_blink = esc // '[1;5m'

end module forlab_color