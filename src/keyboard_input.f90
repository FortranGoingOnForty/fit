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
        call execute_command_line('stty -echo -icanon min 1 time 0 2>/dev/null', wait=.true.)
    end subroutine enable_raw_mode

    ! Disable raw mode (restore normal terminal)
    subroutine disable_raw_mode()
        call execute_command_line('stty echo icanon 2>/dev/null', wait=.true.)
    end subroutine disable_raw_mode

    ! Get a single key press using dd command
    function get_key() result(key)
        type(key_t) :: key
        character(len=10) :: buffer
        integer :: unit, ios, i
        character :: c

        key%code = KEY_NONE
        key%char = ' '
        buffer = ''

        ! Use dd to read a single character with timeout
        call execute_command_line('dd bs=1 count=1 2>/dev/null > /tmp/fit_key.tmp', wait=.true.)

        open(newunit=unit, file='/tmp/fit_key.tmp', status='old', action='read', &
             form='unformatted', access='stream', iostat=ios)
        if (ios /= 0) return

        ! Read up to 3 bytes for escape sequences
        do i = 1, 3
            read(unit, iostat=ios) c
            if (ios /= 0) exit
            buffer(i:i) = c
        end do
        close(unit)

        ! Parse the buffer
        if (len_trim(buffer) == 0) return

        key%char = buffer(1:1)
        key%code = ichar(buffer(1:1))

        ! Check for escape sequences (arrow keys)
        if (key%code == KEY_ESC .and. len_trim(buffer) >= 3) then
            if (buffer(2:2) == '[') then
                select case (buffer(3:3))
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

    end function get_key

end module keyboard_input
