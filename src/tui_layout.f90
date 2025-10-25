module tui_layout
    use terminal_control
    use conflict_parser
    implicit none
    private

    public :: draw_layout, draw_conflict, draw_status_bar, draw_help

    integer :: term_rows = 24
    integer :: term_cols = 80

contains

    ! Initialize and draw the basic layout
    subroutine draw_layout(rows, cols)
        integer, intent(in) :: rows, cols
        integer :: i, mid_col, mid_row

        term_rows = rows
        term_cols = cols
        mid_col = cols / 2
        mid_row = rows / 2

        call term_clear()

        ! Draw top border
        call term_move_cursor(1, 1)
        write(*, '(A)', advance='no') color_cyan // repeat('─', cols) // color_reset

        ! Draw middle horizontal divider
        call term_move_cursor(mid_row, 1)
        write(*, '(A)', advance='no') color_cyan // repeat('─', cols) // color_reset

        ! Draw vertical divider in top half
        do i = 2, mid_row - 1
            call term_move_cursor(i, mid_col)
            write(*, '(A)', advance='no') color_cyan // '│' // color_reset
        end do

        ! Draw labels
        call term_move_cursor(2, 3)
        write(*, '(A)', advance='no') color_bold // color_yellow // 'INCOMING' // color_reset

        call term_move_cursor(2, mid_col + 3)
        write(*, '(A)', advance='no') color_bold // color_yellow // 'LOCAL' // color_reset

        call term_move_cursor(mid_row + 1, 3)
        write(*, '(A)', advance='no') color_bold // color_yellow // 'PREVIEW' // color_reset

        call flush(6)
    end subroutine draw_layout

    ! Draw a conflict in the three panes
    subroutine draw_conflict(conflict, current, total)
        type(conflict_t), intent(in) :: conflict
        integer, intent(in) :: current, total
        integer :: i, mid_col, mid_row, max_lines
        integer :: incoming_lines, local_lines
        character(len=1024) :: line

        mid_col = term_cols / 2
        mid_row = term_rows / 2
        max_lines = mid_row - 4  ! Leave room for borders and labels

        ! Clear previous content
        call clear_pane(3, mid_row - 1, 1, mid_col - 1)
        call clear_pane(3, mid_row - 1, mid_col + 1, term_cols)
        call clear_pane(mid_row + 2, term_rows - 2, 1, term_cols)

        ! Draw incoming changes (left pane)
        if (allocated(conflict%incoming_lines)) then
            incoming_lines = min(size(conflict%incoming_lines), max_lines)
            do i = 1, incoming_lines
                call term_move_cursor(3 + i, 2)
                line = conflict%incoming_lines(i)
                write(*, '(A)', advance='no') color_green // '+' // trim(line(1:min(len_trim(line), mid_col - 4))) // color_reset
            end do
        end if

        ! Draw local changes (right pane)
        if (allocated(conflict%local_lines)) then
            local_lines = min(size(conflict%local_lines), max_lines)
            do i = 1, local_lines
                call term_move_cursor(3 + i, mid_col + 2)
                line = conflict%local_lines(i)
                write(*, '(A)', advance='no') color_red // '-' // trim(line(1:min(len_trim(line), mid_col - 4))) // color_reset
            end do
        end if

        ! Draw preview based on choice
        call draw_preview(conflict, mid_row, max_lines)

        ! Draw conflict counter
        call term_move_cursor(mid_row, term_cols - 15)
        write(*, '(A, I0, A, I0, A)', advance='no') color_cyan // '[ ', current, ' / ', total, ' ]' // color_reset

        call flush(6)
    end subroutine draw_conflict

    ! Draw the preview pane based on current choice
    subroutine draw_preview(conflict, start_row, max_lines)
        type(conflict_t), intent(in) :: conflict
        integer, intent(in) :: start_row, max_lines
        integer :: i, n_lines
        character(len=1024) :: line

        select case (conflict%choice)
        case (1)  ! Incoming
            if (allocated(conflict%incoming_lines)) then
                n_lines = min(size(conflict%incoming_lines), max_lines)
                do i = 1, n_lines
                    call term_move_cursor(start_row + 2 + i, 2)
                    line = conflict%incoming_lines(i)
                    write(*, '(A)', advance='no') trim(line(1:min(len_trim(line), term_cols - 4)))
                end do
            end if

        case (2)  ! Local
            if (allocated(conflict%local_lines)) then
                n_lines = min(size(conflict%local_lines), max_lines)
                do i = 1, n_lines
                    call term_move_cursor(start_row + 2 + i, 2)
                    line = conflict%local_lines(i)
                    write(*, '(A)', advance='no') trim(line(1:min(len_trim(line), term_cols - 4)))
                end do
            end if

        case (3)  ! Both
            n_lines = 0
            if (allocated(conflict%incoming_lines)) then
                do i = 1, min(size(conflict%incoming_lines), max_lines)
                    n_lines = n_lines + 1
                    call term_move_cursor(start_row + 2 + n_lines, 2)
                    line = conflict%incoming_lines(i)
                    write(*, '(A)', advance='no') trim(line(1:min(len_trim(line), term_cols - 4)))
                end do
            end if
            if (allocated(conflict%local_lines) .and. n_lines < max_lines) then
                do i = 1, min(size(conflict%local_lines), max_lines - n_lines)
                    n_lines = n_lines + 1
                    call term_move_cursor(start_row + 2 + n_lines, 2)
                    line = conflict%local_lines(i)
                    write(*, '(A)', advance='no') trim(line(1:min(len_trim(line), term_cols - 4)))
                end do
            end if

        case default
            ! No choice yet - show placeholder
            call term_move_cursor(start_row + 3, 2)
            write(*, '(A)', advance='no') color_dim // '(Select incoming, local, or both)' // color_reset
        end select

        call flush(6)
    end subroutine draw_preview

    ! Clear a rectangular region
    subroutine clear_pane(row_start, row_end, col_start, col_end)
        integer, intent(in) :: row_start, row_end, col_start, col_end
        integer :: i, width

        width = col_end - col_start + 1
        do i = row_start, row_end
            call term_move_cursor(i, col_start)
            write(*, '(A)', advance='no') repeat(' ', width)
        end do
    end subroutine clear_pane

    ! Draw status bar at bottom
    subroutine draw_status_bar(message)
        character(len=*), intent(in) :: message

        call term_move_cursor(term_rows - 1, 1)
        write(*, '(A)', advance='no') color_cyan // repeat('─', term_cols) // color_reset

        call term_move_cursor(term_rows, 1)
        write(*, '(A)', advance='no') color_bold // trim(message(1:min(len_trim(message), term_cols))) // color_reset

        call flush(6)
    end subroutine draw_status_bar

    ! Draw help text
    subroutine draw_help()
        character(len=256) :: help_text

        help_text = '[i]ncoming [l]ocal [b]oth | [n]ext [p]rev | [s]ave [q]uit'
        call draw_status_bar(help_text)
    end subroutine draw_help

end module tui_layout
