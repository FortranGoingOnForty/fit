program fit
    use conflict_parser
    use terminal_control
    use keyboard_input
    use tui_layout
    use resolution_engine
    use pane_state
    implicit none

    character(len=512) :: filename
    type(conflict_t), allocatable :: conflicts(:)
    integer :: n_conflicts, current_conflict, ios
    logical :: success, running
    type(key_t) :: key
    integer :: term_rows, term_cols
    character(len=256) :: status_msg
    type(pane_t) :: pane

    ! Parse command line arguments
    if (command_argument_count() < 1) then
        print '(A)', 'Usage: fit <file-with-conflicts>'
        stop 1
    end if

    call get_command_argument(1, filename)

    ! Check if file exists
    open(unit=99, file=trim(filename), status='old', iostat=ios)
    if (ios /= 0) then
        print '(A)', 'Error: File not found: ' // trim(filename)
        stop 1
    end if
    close(99)

    ! Parse conflicts
    call parse_conflict_file(trim(filename), conflicts, n_conflicts, success)

    if (.not. success) then
        print '(A)', 'Error: Could not parse file'
        stop 1
    end if

    if (n_conflicts == 0) then
        print '(A)', 'No merge conflicts found in ' // trim(filename)
        stop 0
    end if

    ! Initialize terminal
    call term_get_size(term_rows, term_cols, success)
    if (.not. success) then
        term_rows = 24
        term_cols = 80
    end if

    ! Enter raw mode and hide cursor
    call enable_raw_mode()
    call term_hide_cursor()

    ! Draw initial layout
    call draw_layout(term_rows, term_cols)
    call draw_help()

    ! Initialize pane state
    call init_pane_state(pane)

    ! Main event loop
    current_conflict = 1
    running = .true.

    do while (running)
        ! Draw current conflict with scrolling
        call draw_conflict_scrollable(conflicts(current_conflict), current_conflict, n_conflicts, pane)

        ! Update status message
        write(status_msg, '(A, I0, A, I0, A)') &
            '[i/l/b] choose | [←→] switch pane | [↑↓] scroll | [n/p] conflict | [s]ave [q]uit  (', &
            current_conflict, '/', n_conflicts, ')'
        call draw_status_bar(status_msg)

        ! Get user input
        key = get_key()

        ! Handle key presses
        select case (key%code)
        case (KEY_Q, KEY_ESC)
            ! Quit without saving
            running = .false.

        case (KEY_I)
            ! Select incoming
            conflicts(current_conflict)%choice = 1

        case (KEY_L)
            ! Select local
            conflicts(current_conflict)%choice = 2

        case (KEY_B)
            ! Select both
            conflicts(current_conflict)%choice = 3

        case (KEY_UP)
            ! Scroll up in active pane
            call scroll_up(pane)

        case (KEY_DOWN)
            ! Scroll down in active pane
            call scroll_down(pane, max(10, int((term_rows - 2) * 0.4)) - 4)

        case (KEY_LEFT)
            ! Select previous pane
            call select_prev_pane(pane)

        case (KEY_RIGHT)
            ! Select next pane
            call select_next_pane(pane)

        case (KEY_N)
            ! Next conflict
            if (current_conflict < n_conflicts) then
                current_conflict = current_conflict + 1
                call reset_scroll(pane)
            end if

        case (KEY_P)
            ! Previous conflict
            if (current_conflict > 1) then
                current_conflict = current_conflict - 1
                call reset_scroll(pane)
            end if

        case (KEY_S)
            ! Save and quit
            if (all_conflicts_resolved(conflicts, n_conflicts)) then
                call write_resolved_file(trim(filename), conflicts, n_conflicts, success)
                if (success) then
                    running = .false.
                else
                    call draw_status_bar('Error: Could not write file!')
                    call sleep_ms(2000)
                end if
            else
                call draw_status_bar('Error: Not all conflicts resolved!')
                call sleep_ms(2000)
            end if

        end select
    end do

    ! Cleanup
    call term_show_cursor()
    call disable_raw_mode()
    call term_clear()

    if (all_conflicts_resolved(conflicts, n_conflicts)) then
        print '(A)', 'All conflicts resolved! File saved: ' // trim(filename)
    else
        print '(A)', 'Exited without saving.'
    end if

contains

    ! Sleep for milliseconds (using system call)
    subroutine sleep_ms(ms)
        integer, intent(in) :: ms
        real :: seconds

        seconds = real(ms) / 1000.0
        call execute_command_line('sleep ' // trim(adjustl(real_to_str(seconds))))
    end subroutine sleep_ms

    ! Convert real to string
    function real_to_str(r) result(str)
        real, intent(in) :: r
        character(len=32) :: str

        write(str, '(F0.3)') r
    end function real_to_str

end program fit
