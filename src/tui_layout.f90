module tui_layout
    use terminal_control
    use conflict_parser
    use pane_state
    implicit none
    private

    public :: draw_layout, draw_conflict_scrollable, draw_status_bar, draw_help

    integer :: term_rows = 24
    integer :: term_cols = 80

contains

    ! Initialize and draw the basic layout
    subroutine draw_layout(rows, cols, active_pane)
        integer, intent(in) :: rows, cols
        integer, intent(in), optional :: active_pane
        integer :: i, mid_col, mid_row, pane_id
        character(len=:), allocatable :: border

        term_rows = rows
        term_cols = cols
        mid_col = cols / 2
        ! Use 40% for top panes, 60% for preview (gives more space to preview)
        ! Subtract 2 for status bar at bottom
        mid_row = max(10, int((rows - 2) * 0.4))

        pane_id = 1
        if (present(active_pane)) pane_id = active_pane

        ! Create border string with proper width
        allocate(character(len=cols) :: border)
        border = repeat('─', cols)

        call term_clear()

        ! Draw top border (full width)
        call term_move_cursor(1, 1)
        write(*, '(A)', advance='no') color_cyan // border // color_reset

        ! Draw middle horizontal divider (full width)
        call term_move_cursor(mid_row, 1)
        write(*, '(A)', advance='no') color_cyan // border // color_reset

        ! Draw vertical divider in top half
        do i = 2, mid_row - 1
            call term_move_cursor(i, mid_col)
            write(*, '(A)', advance='no') color_cyan // '│' // color_reset
        end do

        ! Draw labels with active highlighting
        call term_move_cursor(2, 3)
        if (pane_id == PANE_INCOMING) then
            write(*, '(A)', advance='no') color_bold // bg_green // color_black // ' INCOMING ' // color_reset
        else
            write(*, '(A)', advance='no') color_bold // color_yellow // 'INCOMING' // color_reset
        end if

        call term_move_cursor(2, mid_col + 3)
        if (pane_id == PANE_LOCAL) then
            write(*, '(A)', advance='no') color_bold // bg_red // color_white // ' LOCAL ' // color_reset
        else
            write(*, '(A)', advance='no') color_bold // color_yellow // 'LOCAL' // color_reset
        end if

        call term_move_cursor(mid_row + 1, 3)
        if (pane_id == PANE_PREVIEW) then
            write(*, '(A)', advance='no') color_bold // bg_blue // color_white // ' PREVIEW ' // color_reset
        else
            write(*, '(A)', advance='no') color_bold // color_yellow // 'PREVIEW' // color_reset
        end if

        deallocate(border)
        call flush(6)
    end subroutine draw_layout

    ! Draw a conflict in the three panes
    subroutine draw_conflict(conflict, current, total)
        type(conflict_t), intent(in) :: conflict
        integer, intent(in) :: current, total
        integer :: i, mid_col, mid_row, max_lines, preview_max_lines
        integer :: incoming_lines, local_lines
        character(len=1024) :: line

        mid_col = term_cols / 2
        ! Use same calculation as draw_layout for consistency
        mid_row = max(10, int((term_rows - 2) * 0.4))
        max_lines = mid_row - 4  ! Leave room for borders and labels
        ! Preview gets more space (60% of screen)
        preview_max_lines = term_rows - mid_row - 4

        ! Clear previous content
        call clear_pane(3, mid_row - 1, 1, mid_col - 1)
        call clear_pane(3, mid_row - 1, mid_col + 1, term_cols)
        call clear_pane(mid_row + 2, term_rows - 2, 1, term_cols)

        ! Draw left pane (INCOMING): context + incoming + context
        call draw_pane_with_context(conflict, 2, mid_col - 4, max_lines, .true.)

        ! Draw right pane (LOCAL): context + local + context
        call draw_pane_with_context(conflict, mid_col + 2, term_cols - mid_col - 2, max_lines, .false.)

        ! Draw preview based on choice (use larger preview_max_lines)
        call draw_preview(conflict, mid_row, preview_max_lines)

        ! Draw conflict counter
        call term_move_cursor(mid_row, term_cols - 15)
        write(*, '(A, I0, A, I0, A)', advance='no') color_cyan // '[ ', current, ' / ', total, ' ]' // color_reset

        call flush(6)
    end subroutine draw_conflict

    ! Draw a conflict with scrollable panes
    subroutine draw_conflict_scrollable(conflicts, n_conflicts, current, pane)
        type(conflict_t), intent(inout) :: conflicts(:)
        integer, intent(in) :: n_conflicts, current
        type(pane_t), intent(inout) :: pane
        integer :: mid_col, mid_row, max_lines, preview_max_lines
        character(len=:), allocatable, dimension(:) :: incoming_view, local_view, preview_view
        integer :: n_incoming, n_local, n_preview
        integer :: conflict_start, conflict_end

        mid_col = term_cols / 2
        mid_row = max(10, int((term_rows - 2) * 0.4))
        max_lines = mid_row - 4
        preview_max_lines = term_rows - mid_row - 4

        ! Redraw layout with active pane highlighted
        call draw_layout(term_rows, term_cols, pane%active_pane)

        ! Get side views - show incoming/local for current conflict, all others resolved
        call get_side_view(conflicts, n_conflicts, current, 1, incoming_view, n_incoming, conflict_start, conflict_end)
        call get_side_view(conflicts, n_conflicts, current, 2, local_view, n_local, conflict_start, conflict_end)

        ! Get full preview with ALL conflicts resolved
        call get_full_preview(conflicts, n_conflicts, current, preview_view, n_preview, conflict_start, conflict_end)

        ! Update pane max lines
        pane%max_lines_incoming = n_incoming
        pane%max_lines_local = n_local
        pane%max_lines_preview = n_preview

        ! Clear previous content
        call clear_pane(3, mid_row - 1, 1, mid_col - 1)
        call clear_pane(3, mid_row - 1, mid_col + 1, term_cols)
        call clear_pane(mid_row + 2, term_rows - 2, 1, term_cols)

        ! Draw scrollable panes
        call draw_scrollable_pane(incoming_view, n_incoming, 2, mid_col - 4, 4, max_lines, &
                                  pane%scroll_incoming, conflict_start, conflict_end, &
                                  pane%active_pane == PANE_INCOMING, .true.)

        call draw_scrollable_pane(local_view, n_local, mid_col + 2, term_cols - mid_col - 2, 4, max_lines, &
                                  pane%scroll_local, conflict_start, conflict_end, &
                                  pane%active_pane == PANE_LOCAL, .false.)

        call draw_scrollable_pane(preview_view, n_preview, 2, term_cols - 4, mid_row + 2, preview_max_lines, &
                                  pane%scroll_preview, conflict_start, conflict_end, &
                                  pane%active_pane == PANE_PREVIEW, .false.)

        ! Draw conflict counter
        call term_move_cursor(mid_row, term_cols - 15)
        write(*, '(A, I0, A, I0, A)', advance='no') color_cyan // '[ ', current, ' / ', n_conflicts, ' ]' // color_reset

        call flush(6)
    end subroutine draw_conflict_scrollable

    ! Draw the preview pane based on current choice (with context)
    subroutine draw_preview(conflict, start_row, max_lines)
        type(conflict_t), intent(in) :: conflict
        integer, intent(in) :: start_row, max_lines
        integer :: i, row, n_context_before, n_context_after, n_resolution
        character(len=1024) :: line

        row = start_row + 2

        ! Draw context before (dimmed)
        if (allocated(conflict%context_before)) then
            n_context_before = size(conflict%context_before)
            do i = 1, min(n_context_before, max_lines)
                if (row > start_row + max_lines) exit
                call term_move_cursor(row, 2)
                line = conflict%context_before(i)
                write(*, '(A)', advance='no') color_dim // ' ' // &
                      trim(line(1:min(len_trim(line), term_cols - 4))) // color_reset
                row = row + 1
            end do
        end if

        ! Draw resolved content based on choice
        select case (conflict%choice)
        case (1)  ! Incoming
            if (allocated(conflict%incoming_lines)) then
                do i = 1, size(conflict%incoming_lines)
                    if (row > start_row + max_lines) exit
                    call term_move_cursor(row, 2)
                    line = conflict%incoming_lines(i)
                    write(*, '(A)', advance='no') ' ' // trim(line(1:min(len_trim(line), term_cols - 4)))
                    row = row + 1
                end do
            end if

        case (2)  ! Local
            if (allocated(conflict%local_lines)) then
                do i = 1, size(conflict%local_lines)
                    if (row > start_row + max_lines) exit
                    call term_move_cursor(row, 2)
                    line = conflict%local_lines(i)
                    write(*, '(A)', advance='no') ' ' // trim(line(1:min(len_trim(line), term_cols - 4)))
                    row = row + 1
                end do
            end if

        case (3)  ! Both
            if (allocated(conflict%incoming_lines)) then
                do i = 1, size(conflict%incoming_lines)
                    if (row > start_row + max_lines) exit
                    call term_move_cursor(row, 2)
                    line = conflict%incoming_lines(i)
                    write(*, '(A)', advance='no') ' ' // trim(line(1:min(len_trim(line), term_cols - 4)))
                    row = row + 1
                end do
            end if
            if (allocated(conflict%local_lines)) then
                do i = 1, size(conflict%local_lines)
                    if (row > start_row + max_lines) exit
                    call term_move_cursor(row, 2)
                    line = conflict%local_lines(i)
                    write(*, '(A)', advance='no') ' ' // trim(line(1:min(len_trim(line), term_cols - 4)))
                    row = row + 1
                end do
            end if

        case default
            ! No choice yet - show placeholder
            call term_move_cursor(row, 2)
            write(*, '(A)', advance='no') color_dim // '(Select incoming, local, or both)' // color_reset
            row = row + 1
        end select

        ! Draw context after (dimmed)
        if (allocated(conflict%context_after)) then
            n_context_after = size(conflict%context_after)
            do i = 1, min(n_context_after, max_lines - (row - start_row - 2))
                if (row > start_row + max_lines) exit
                call term_move_cursor(row, 2)
                line = conflict%context_after(i)
                write(*, '(A)', advance='no') color_dim // ' ' // &
                      trim(line(1:min(len_trim(line), term_cols - 4))) // color_reset
                row = row + 1
            end do
        end if

        call flush(6)
    end subroutine draw_preview

    ! Draw a pane with context (before + conflict + after)
    subroutine draw_pane_with_context(conflict, col_start, max_width, max_lines, is_incoming)
        type(conflict_t), intent(in) :: conflict
        integer, intent(in) :: col_start, max_width, max_lines
        logical, intent(in) :: is_incoming
        integer :: row, i, n_context_before, n_conflict, n_context_after
        character(len=1024) :: line
        character(len=:), allocatable, dimension(:) :: conflict_lines

        row = 4  ! Start after label

        ! Determine which conflict lines to show
        if (is_incoming) then
            conflict_lines = conflict%incoming_lines
        else
            conflict_lines = conflict%local_lines
        end if

        ! Draw context before (dimmed)
        if (allocated(conflict%context_before)) then
            n_context_before = size(conflict%context_before)
            do i = 1, min(n_context_before, max_lines)
                if (row > max_lines + 3) exit
                call term_move_cursor(row, col_start)
                line = conflict%context_before(i)
                write(*, '(A)', advance='no') color_dim // ' ' // &
                      trim(line(1:min(len_trim(line), max_width))) // color_reset
                row = row + 1
            end do
        end if

        ! Draw conflict lines (colored)
        if (allocated(conflict_lines)) then
            n_conflict = size(conflict_lines)
            do i = 1, min(n_conflict, max_lines - row + 3)
                if (row > max_lines + 3) exit
                call term_move_cursor(row, col_start)
                line = conflict_lines(i)
                if (is_incoming) then
                    write(*, '(A)', advance='no') color_green // '+' // &
                          trim(line(1:min(len_trim(line), max_width))) // color_reset
                else
                    write(*, '(A)', advance='no') color_red // '-' // &
                          trim(line(1:min(len_trim(line), max_width))) // color_reset
                end if
                row = row + 1
            end do
        end if

        ! Draw context after (dimmed)
        if (allocated(conflict%context_after)) then
            n_context_after = size(conflict%context_after)
            do i = 1, min(n_context_after, max_lines - row + 3)
                if (row > max_lines + 3) exit
                call term_move_cursor(row, col_start)
                line = conflict%context_after(i)
                write(*, '(A)', advance='no') color_dim // ' ' // &
                      trim(line(1:min(len_trim(line), max_width))) // color_reset
                row = row + 1
            end do
        end if

        call flush(6)
    end subroutine draw_pane_with_context

    ! Draw a scrollable pane with full file content
    subroutine draw_scrollable_pane(file_view, n_lines, col_start, max_width, row_start, max_rows, &
                                     scroll_offset, conflict_start, conflict_end, is_active, is_incoming)
        character(len=*), dimension(:), intent(in) :: file_view
        integer, intent(in) :: n_lines, col_start, max_width, row_start, max_rows
        integer, intent(in) :: scroll_offset, conflict_start, conflict_end
        logical, intent(in) :: is_active, is_incoming
        integer :: i, row, file_line
        character(len=1024) :: line
        character(len=10) :: line_num_str
        logical :: in_conflict

        row = row_start
        do i = 1, max_rows
            file_line = scroll_offset + i
            if (file_line > n_lines) exit

            ! Determine if this line is in the conflict region
            in_conflict = (file_line >= conflict_start .and. file_line <= conflict_end)

            call term_move_cursor(row, col_start)
            line = file_view(file_line)

            ! Draw with appropriate coloring
            if (in_conflict) then
                ! Conflict region - color coded
                if (is_incoming) then
                    write(*, '(A)', advance='no') color_green // '+' // &
                          trim(line(1:min(len_trim(line), max_width))) // color_reset
                else
                    write(*, '(A)', advance='no') color_red // '-' // &
                          trim(line(1:min(len_trim(line), max_width))) // color_reset
                end if
            else
                ! Normal file content - dimmed
                write(*, '(A)', advance='no') color_dim // ' ' // &
                      trim(line(1:min(len_trim(line), max_width))) // color_reset
            end if

            ! Add active pane indicator
            if (is_active .and. i == 1) then
                call term_move_cursor(row, col_start + max_width + 2)
                write(*, '(A)', advance='no') color_yellow // '◀' // color_reset
            end if

            row = row + 1
        end do

        ! Draw scroll indicator
        if (n_lines > max_rows) then
            call draw_scroll_indicator(col_start + max_width + 1, row_start, max_rows, scroll_offset, n_lines)
        end if

        call flush(6)
    end subroutine draw_scrollable_pane

    ! Draw a scroll position indicator
    subroutine draw_scroll_indicator(col, row_start, pane_height, scroll_offset, total_lines)
        integer, intent(in) :: col, row_start, pane_height, scroll_offset, total_lines
        integer :: indicator_pos, i
        real :: scroll_ratio

        ! Calculate indicator position
        scroll_ratio = real(scroll_offset) / real(max(1, total_lines - pane_height))
        indicator_pos = row_start + int(scroll_ratio * (pane_height - 1))

        ! Draw scroll bar
        do i = row_start, row_start + pane_height - 1
            call term_move_cursor(i, col)
            if (i == indicator_pos) then
                write(*, '(A)', advance='no') color_cyan // '█' // color_reset
            else
                write(*, '(A)', advance='no') color_dim // '│' // color_reset
            end if
        end do
    end subroutine draw_scroll_indicator

    ! Clear a rectangular region
    subroutine clear_pane(row_start, row_end, col_start, col_end)
        integer, intent(in) :: row_start, row_end, col_start, col_end
        integer :: i, width
        character(len=:), allocatable :: spaces

        width = col_end - col_start + 1
        allocate(character(len=width) :: spaces)
        spaces = repeat(' ', width)

        do i = row_start, row_end
            call term_move_cursor(i, col_start)
            write(*, '(A)', advance='no') spaces
        end do

        deallocate(spaces)
    end subroutine clear_pane

    ! Draw status bar at bottom
    subroutine draw_status_bar(message)
        character(len=*), intent(in) :: message
        character(len=:), allocatable :: border, padded_msg
        integer :: msg_len

        ! Create border string with proper width
        allocate(character(len=term_cols) :: border)
        border = repeat('─', term_cols)

        ! Create padded message to fill entire status line
        allocate(character(len=term_cols) :: padded_msg)
        ! Initialize with spaces
        padded_msg = repeat(' ', term_cols)
        ! Copy message (truncated if too long)
        msg_len = min(len_trim(message), term_cols)
        if (msg_len > 0) then
            padded_msg(1:msg_len) = message(1:msg_len)
        end if

        call term_move_cursor(term_rows - 1, 1)
        write(*, '(A)', advance='no') color_cyan // border // color_reset

        call term_move_cursor(term_rows, 1)
        write(*, '(A)', advance='no') color_bold // padded_msg // color_reset

        deallocate(border, padded_msg)
        call flush(6)
    end subroutine draw_status_bar

    ! Draw help text
    subroutine draw_help()
        character(len=256) :: help_text

        help_text = '[i]ncoming [l]ocal [b]oth | [n]ext [p]rev | [s]ave [q]uit'
        call draw_status_bar(help_text)
    end subroutine draw_help

end module tui_layout
