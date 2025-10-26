module keyboard_input
    use iso_c_binding
    implicit none
    private

    public :: key_t, get_key, enable_raw_mode, disable_raw_mode

    ! Key codes
    integer, parameter, public :: KEY_NONE      = 0
    integer, parameter, public :: KEY_UP        = 1
    integer, parameter, public :: KEY_DOWN      = 2
    integer, parameter, public :: KEY_LEFT      = 3
    integer, parameter, public :: KEY_RIGHT     = 4
    integer, parameter, public :: KEY_ENTER     = 10
    integer, parameter, public :: KEY_ESC       = 27
    integer, parameter, public :: KEY_Q         = 113
    integer, parameter, public :: KEY_I         = 105  ! Select incoming
    integer, parameter, public :: KEY_L         = 108  ! Select local
    integer, parameter, public :: KEY_B         = 98   ! Select both
    integer, parameter, public :: KEY_N         = 110  ! Next conflict
    integer, parameter, public :: KEY_P         = 112  ! Previous conflict
    integer, parameter, public :: KEY_S         = 115  ! Save and quit

    type :: key_t
        integer :: code
        character :: char
    end type key_t

contains

    ! Enable raw mode (no buffering, no echo)
    subroutine enable_raw_mode()
        ! Raw mode - we'll handle multi-byte sequences in get_key()
        call execute_command_line('stty -echo -icanon 2>/dev/null', wait=.true.)
    end subroutine enable_raw_mode

    ! Disable raw mode (restore normal terminal)
    subroutine disable_raw_mode()
        call execute_command_line('stty echo icanon 2>/dev/null', wait=.true.)
    end subroutine disable_raw_mode

    ! Get a single key press
    function get_key() result(key)
        type(key_t) :: key
        character :: c1, c2, c3
        integer :: unit, ios, file_size
        logical :: file_exists

        key%code = KEY_NONE
        key%char = ' '

        ! Use Perl to read with proper timeout handling
        ! Reads 1 byte, if ESC reads 2 more with 50ms timeout
        call execute_command_line( &
            'perl -e ''use Time::HiRes qw(usleep); ' // &
            'open(TTY, "</dev/tty"); ' // &
            'sysread(TTY, $c, 1); print $c; ' // &
            'if(ord($c)==27){usleep(10000); sysread(TTY, $m, 2); print $m;} ' // &
            'close(TTY);'' ' // &
            '> /tmp/fit_key.tmp 2>/dev/null', &
            wait=.true.)

        inquire(file='/tmp/fit_key.tmp', exist=file_exists, size=file_size)
        if (.not. file_exists .or. file_size == 0) return

        open(newunit=unit, file='/tmp/fit_key.tmp', status='old', action='read', &
             form='unformatted', access='stream', iostat=ios)
        if (ios /= 0) return

        ! Read first byte
        read(unit, iostat=ios) c1
        if (ios /= 0) then
            close(unit)
            return
        end if

        key%char = c1
        key%code = ichar(c1)

        ! Check if we got an arrow key sequence (3 bytes total)
        if (key%code == KEY_ESC .and. file_size >= 3) then
            read(unit, iostat=ios) c2
            if (ios == 0) then
                read(unit, iostat=ios) c3
                if (ios == 0 .and. c2 == '[') then
                    select case (c3)
                    case ('A')
                        key%code = KEY_UP
                    case ('B')
                        key%code = KEY_DOWN
                    case ('C')
                        key%code = KEY_RIGHT
                    case ('D')
                        key%code = KEY_LEFT
                    end select
                end if
            end if
        end if

        close(unit)

        ! Clean up temp file
        call execute_command_line('rm -f /tmp/fit_key.tmp 2>/dev/null', wait=.true.)

    end function get_key

end module keyboard_input
