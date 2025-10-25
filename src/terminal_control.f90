module terminal_control
    implicit none
    private

    public :: term_clear, term_move_cursor, term_hide_cursor, term_show_cursor
    public :: term_reset, term_get_size, term_save_cursor, term_restore_cursor
    public :: color_red, color_green, color_yellow, color_blue, color_magenta, color_cyan
    public :: color_white, color_black, color_reset, color_bold, color_dim, color_underline
    public :: bg_red, bg_green, bg_yellow, bg_blue, bg_magenta, bg_cyan, bg_white, bg_black

    ! ANSI color codes
    character(len=*), parameter :: ESC = achar(27)
    character(len=*), parameter :: CSI = ESC // '['

    ! Foreground colors
    character(len=*), parameter :: color_black   = CSI // '30m'
    character(len=*), parameter :: color_red     = CSI // '31m'
    character(len=*), parameter :: color_green   = CSI // '32m'
    character(len=*), parameter :: color_yellow  = CSI // '33m'
    character(len=*), parameter :: color_blue    = CSI // '34m'
    character(len=*), parameter :: color_magenta = CSI // '35m'
    character(len=*), parameter :: color_cyan    = CSI // '36m'
    character(len=*), parameter :: color_white   = CSI // '37m'

    ! Background colors
    character(len=*), parameter :: bg_black   = CSI // '40m'
    character(len=*), parameter :: bg_red     = CSI // '41m'
    character(len=*), parameter :: bg_green   = CSI // '42m'
    character(len=*), parameter :: bg_yellow  = CSI // '43m'
    character(len=*), parameter :: bg_blue    = CSI // '44m'
    character(len=*), parameter :: bg_magenta = CSI // '45m'
    character(len=*), parameter :: bg_cyan    = CSI // '46m'
    character(len=*), parameter :: bg_white   = CSI // '47m'

    ! Text styles
    character(len=*), parameter :: color_reset     = CSI // '0m'
    character(len=*), parameter :: color_bold      = CSI // '1m'
    character(len=*), parameter :: color_dim       = CSI // '2m'
    character(len=*), parameter :: color_underline = CSI // '4m'

contains

    ! Clear the screen
    subroutine term_clear()
        write(*, '(A)', advance='no') CSI // '2J' // CSI // 'H'
        call flush(6)
    end subroutine term_clear

    ! Move cursor to specific position (1-indexed)
    subroutine term_move_cursor(row, col)
        integer, intent(in) :: row, col
        character(len=32) :: cmd

        write(cmd, '(A, I0, A, I0, A)') CSI, row, ';', col, 'H'
        write(*, '(A)', advance='no') trim(cmd)
        call flush(6)
    end subroutine term_move_cursor

    ! Hide cursor
    subroutine term_hide_cursor()
        write(*, '(A)', advance='no') CSI // '?25l'
        call flush(6)
    end subroutine term_hide_cursor

    ! Show cursor
    subroutine term_show_cursor()
        write(*, '(A)', advance='no') CSI // '?25h'
        call flush(6)
    end subroutine term_show_cursor

    ! Reset all terminal attributes
    subroutine term_reset()
        write(*, '(A)', advance='no') color_reset
        call flush(6)
    end subroutine term_reset

    ! Save cursor position
    subroutine term_save_cursor()
        write(*, '(A)', advance='no') ESC // '7'
        call flush(6)
    end subroutine term_save_cursor

    ! Restore cursor position
    subroutine term_restore_cursor()
        write(*, '(A)', advance='no') ESC // '8'
        call flush(6)
    end subroutine term_restore_cursor

    ! Get terminal size (using tput command)
    subroutine term_get_size(rows, cols, success)
        integer, intent(out) :: rows, cols
        logical, intent(out) :: success
        character(len=256) :: cmd_output
        integer :: unit, ios

        success = .false.
        rows = 24
        cols = 80

        ! Try to get rows
        open(newunit=unit, file='/tmp/fit_rows.txt', status='replace', action='write')
        close(unit)
        call execute_command_line('tput lines > /tmp/fit_rows.txt 2>/dev/null', exitstat=ios)
        if (ios == 0) then
            open(newunit=unit, file='/tmp/fit_rows.txt', status='old', action='read')
            read(unit, *, iostat=ios) rows
            close(unit)
            if (ios == 0) success = .true.
        end if

        ! Try to get cols
        open(newunit=unit, file='/tmp/fit_cols.txt', status='replace', action='write')
        close(unit)
        call execute_command_line('tput cols > /tmp/fit_cols.txt 2>/dev/null', exitstat=ios)
        if (ios == 0) then
            open(newunit=unit, file='/tmp/fit_cols.txt', status='old', action='read')
            read(unit, *, iostat=ios) cols
            close(unit)
            if (ios == 0) success = .true.
        end if

        ! Clean up temp files
        call execute_command_line('rm -f /tmp/fit_rows.txt /tmp/fit_cols.txt 2>/dev/null')

    end subroutine term_get_size

    ! Helper to flush output
    subroutine flush(unit)
        integer, intent(in) :: unit
        ! Fortran 2003 flush statement
        flush(unit)
    end subroutine flush

end module terminal_control
