module conflict_parser
    implicit none
    private

    public :: conflict_t, parse_conflict_file, conflict_count, get_file_view, get_full_preview, get_side_view

    ! Type to hold a single conflict
    type :: conflict_t
        integer :: start_line              ! Line number where conflict starts
        integer :: middle_line             ! Line number of ======= separator
        integer :: end_line                ! Line number where conflict ends
        character(len=:), allocatable :: incoming_branch  ! Branch name from <<<<<<< HEAD
        character(len=:), allocatable :: local_branch     ! Branch name from >>>>>>>
        character(len=:), allocatable, dimension(:) :: context_before  ! Lines before conflict (context)
        character(len=:), allocatable, dimension(:) :: incoming_lines  ! Lines from incoming
        character(len=:), allocatable, dimension(:) :: local_lines     ! Lines from local
        character(len=:), allocatable, dimension(:) :: context_after   ! Lines after conflict (context)
        character(len=:), allocatable, dimension(:) :: full_file       ! All lines from the file
        integer :: total_file_lines        ! Total number of lines in file
        integer :: choice  ! 0=none, 1=incoming, 2=local, 3=both
    end type conflict_t

    ! Number of context lines to show before and after conflict
    integer, parameter :: CONTEXT_LINES = 5

contains

    ! Count the number of conflicts in a file
    function conflict_count(filename) result(count)
        character(len=*), intent(in) :: filename
        integer :: count
        integer :: unit, ios
        character(len=1024) :: line

        count = 0
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) return

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            if (index(trim(line), '<<<<<<<') == 1) then
                count = count + 1
            end if
        end do

        close(unit)
    end function conflict_count

    ! Parse a file with conflicts and return array of conflict_t
    subroutine parse_conflict_file(filename, conflicts, n_conflicts, success)
        character(len=*), intent(in) :: filename
        type(conflict_t), allocatable, intent(out) :: conflicts(:)
        integer, intent(out) :: n_conflicts
        logical, intent(out) :: success

        integer :: unit, ios, line_num, i
        character(len=1024) :: line
        character(len=1024), allocatable :: file_lines(:)
        integer :: n_lines, capacity
        logical :: in_conflict
        integer :: conflict_start, conflict_middle
        character(len=256) :: branch_name

        success = .false.
        n_conflicts = 0

        ! First pass: read all lines into memory
        capacity = 100
        allocate(file_lines(capacity))
        n_lines = 0

        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) return

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            n_lines = n_lines + 1
            if (n_lines > capacity) then
                ! Grow array
                call grow_array(file_lines, capacity)
            end if
            file_lines(n_lines) = line
        end do

        close(unit)

        ! Count conflicts
        n_conflicts = 0
        do i = 1, n_lines
            if (index(trim(file_lines(i)), '<<<<<<<') == 1) then
                n_conflicts = n_conflicts + 1
            end if
        end do

        if (n_conflicts == 0) then
            success = .true.
            return
        end if

        ! Allocate conflicts array
        allocate(conflicts(n_conflicts))

        ! Second pass: parse conflicts
        n_conflicts = 0
        in_conflict = .false.

        do line_num = 1, n_lines
            line = file_lines(line_num)

            if (index(trim(line), '<<<<<<<') == 1) then
                ! Start of conflict
                in_conflict = .true.
                n_conflicts = n_conflicts + 1
                conflict_start = line_num
                conflicts(n_conflicts)%start_line = line_num
                conflicts(n_conflicts)%choice = 0

                ! Extract branch name (after <<<<<<<)
                branch_name = adjustl(line(8:))
                conflicts(n_conflicts)%incoming_branch = trim(branch_name)

                ! Extract context before conflict (up to CONTEXT_LINES)
                call extract_lines(file_lines, max(1, conflict_start - CONTEXT_LINES), &
                                   conflict_start - 1, conflicts(n_conflicts)%context_before)

            else if (index(trim(line), '=======') == 1 .and. in_conflict) then
                ! Middle of conflict
                conflict_middle = line_num
                conflicts(n_conflicts)%middle_line = line_num

                ! Extract incoming lines
                call extract_lines(file_lines, conflict_start + 1, conflict_middle - 1, &
                                   conflicts(n_conflicts)%incoming_lines)

            else if (index(trim(line), '>>>>>>>') == 1 .and. in_conflict) then
                ! End of conflict
                conflicts(n_conflicts)%end_line = line_num

                ! Extract branch name
                branch_name = adjustl(line(8:))
                conflicts(n_conflicts)%local_branch = trim(branch_name)

                ! Extract local lines
                call extract_lines(file_lines, conflict_middle + 1, line_num - 1, &
                                   conflicts(n_conflicts)%local_lines)

                ! Extract context after conflict (up to CONTEXT_LINES)
                call extract_lines(file_lines, line_num + 1, &
                                   min(n_lines, line_num + CONTEXT_LINES), &
                                   conflicts(n_conflicts)%context_after)

                in_conflict = .false.
            end if
        end do

        ! Store full file content in each conflict for scrolling
        do i = 1, n_conflicts
            conflicts(i)%total_file_lines = n_lines
            allocate(character(len=1024) :: conflicts(i)%full_file(n_lines))
            do line_num = 1, n_lines
                conflicts(i)%full_file(line_num) = file_lines(line_num)
            end do
        end do

        success = .true.

    end subroutine parse_conflict_file

    ! Helper: extract lines from array
    subroutine extract_lines(source, start_idx, end_idx, dest)
        character(len=*), intent(in) :: source(:)
        integer, intent(in) :: start_idx, end_idx
        character(len=:), allocatable, intent(out) :: dest(:)
        integer :: n, i

        n = end_idx - start_idx + 1
        if (n <= 0) then
            allocate(character(len=0) :: dest(0))
            return
        end if

        allocate(character(len=1024) :: dest(n))
        do i = 1, n
            dest(i) = source(start_idx + i - 1)
        end do
    end subroutine extract_lines

    ! Helper: grow an array
    subroutine grow_array(array, capacity)
        character(len=*), allocatable, intent(inout) :: array(:)
        integer, intent(inout) :: capacity
        character(len=:), allocatable :: temp(:)
        integer :: old_capacity, i

        old_capacity = capacity
        capacity = capacity * 2

        allocate(character(len=len(array)) :: temp(capacity))
        do i = 1, old_capacity
            temp(i) = array(i)
        end do

        call move_alloc(temp, array)
    end subroutine grow_array

    ! Get a view of the file with conflicts resolved
    ! view_type: 1=incoming, 2=local, 3=preview (uses conflict%choice)
    subroutine get_file_view(conflict, view_type, file_view, n_lines, conflict_start, conflict_end)
        type(conflict_t), intent(in) :: conflict
        integer, intent(in) :: view_type
        character(len=:), allocatable, dimension(:), intent(out) :: file_view
        integer, intent(out) :: n_lines
        integer, intent(out) :: conflict_start, conflict_end
        integer :: i, j, view_idx, n_conflict_lines
        character(len=:), allocatable, dimension(:) :: resolution_lines

        ! Start with full file capacity
        allocate(character(len=1024) :: file_view(conflict%total_file_lines * 2))
        view_idx = 0

        ! Determine which resolution to use
        select case (view_type)
        case (1)  ! Incoming
            resolution_lines = conflict%incoming_lines
        case (2)  ! Local
            resolution_lines = conflict%local_lines
        case (3)  ! Preview - use chosen resolution
            select case (conflict%choice)
            case (1)
                resolution_lines = conflict%incoming_lines
            case (2)
                resolution_lines = conflict%local_lines
            case (3)  ! Both
                if (allocated(conflict%incoming_lines) .and. allocated(conflict%local_lines)) then
                    n_conflict_lines = size(conflict%incoming_lines) + size(conflict%local_lines)
                    allocate(character(len=1024) :: resolution_lines(n_conflict_lines))
                    j = 1
                    do i = 1, size(conflict%incoming_lines)
                        resolution_lines(j) = conflict%incoming_lines(i)
                        j = j + 1
                    end do
                    do i = 1, size(conflict%local_lines)
                        resolution_lines(j) = conflict%local_lines(i)
                        j = j + 1
                    end do
                end if
            end select
        end select

        ! Build the view: lines before + resolution + lines after
        conflict_start = 0
        conflict_end = 0

        ! Copy lines before conflict
        do i = 1, conflict%start_line - 1
            view_idx = view_idx + 1
            file_view(view_idx) = conflict%full_file(i)
        end do

        ! Insert resolution (mark where conflict region starts)
        conflict_start = view_idx + 1
        if (allocated(resolution_lines)) then
            do i = 1, size(resolution_lines)
                view_idx = view_idx + 1
                file_view(view_idx) = resolution_lines(i)
            end do
        end if
        conflict_end = view_idx

        ! Copy lines after conflict
        do i = conflict%end_line + 1, conflict%total_file_lines
            view_idx = view_idx + 1
            file_view(view_idx) = conflict%full_file(i)
        end do

        n_lines = view_idx

    end subroutine get_file_view

    ! Get a full preview with ALL conflicts resolved
    subroutine get_full_preview(conflicts, n_conflicts, current_idx, file_view, n_lines, conflict_start, conflict_end)
        type(conflict_t), intent(in) :: conflicts(:)
        integer, intent(in) :: n_conflicts, current_idx
        character(len=:), allocatable, dimension(:), intent(out) :: file_view
        integer, intent(out) :: n_lines
        integer, intent(out) :: conflict_start, conflict_end
        integer :: i, j, view_idx, line_num, n_res_lines
        character(len=:), allocatable, dimension(:) :: resolution_lines
        logical :: in_conflict
        integer :: conflict_idx

        if (n_conflicts == 0 .or. .not. allocated(conflicts(1)%full_file)) return

        ! Allocate output
        allocate(character(len=1024) :: file_view(conflicts(1)%total_file_lines * 2))
        view_idx = 0
        conflict_start = 0
        conflict_end = 0

        ! Walk through the file line by line
        line_num = 1
        do while (line_num <= conflicts(1)%total_file_lines)
            ! Check if this line starts a conflict
            in_conflict = .false.
            do i = 1, n_conflicts
                if (line_num == conflicts(i)%start_line) then
                    in_conflict = .true.
                    conflict_idx = i

                    ! Mark if this is the current conflict
                    if (i == current_idx) then
                        conflict_start = view_idx + 1
                    end if

                    ! Get resolution for this conflict
                    select case (conflicts(i)%choice)
                    case (1)  ! Incoming
                        resolution_lines = conflicts(i)%incoming_lines
                    case (2)  ! Local
                        resolution_lines = conflicts(i)%local_lines
                    case (3)  ! Both
                        if (allocated(conflicts(i)%incoming_lines) .and. allocated(conflicts(i)%local_lines)) then
                            n_res_lines = size(conflicts(i)%incoming_lines) + size(conflicts(i)%local_lines)
                            allocate(character(len=1024) :: resolution_lines(n_res_lines))
                            do j = 1, size(conflicts(i)%incoming_lines)
                                resolution_lines(j) = conflicts(i)%incoming_lines(j)
                            end do
                            do j = 1, size(conflicts(i)%local_lines)
                                resolution_lines(size(conflicts(i)%incoming_lines) + j) = conflicts(i)%local_lines(j)
                            end do
                        end if
                    case default  ! No choice yet - show incoming
                        resolution_lines = conflicts(i)%incoming_lines
                    end select

                    ! Add resolution lines
                    if (allocated(resolution_lines)) then
                        do j = 1, size(resolution_lines)
                            view_idx = view_idx + 1
                            file_view(view_idx) = resolution_lines(j)
                        end do
                        deallocate(resolution_lines)
                    end if

                    ! Mark end if this is the current conflict
                    if (i == current_idx) then
                        conflict_end = view_idx
                    end if

                    ! Skip to end of conflict
                    line_num = conflicts(i)%end_line + 1
                    exit
                end if
            end do

            ! If not in conflict, copy the line
            if (.not. in_conflict) then
                view_idx = view_idx + 1
                file_view(view_idx) = conflicts(1)%full_file(line_num)
                line_num = line_num + 1
            end if
        end do

        n_lines = view_idx

    end subroutine get_full_preview

    ! Get a view showing a specific side (incoming/local) for current conflict, all others resolved
    subroutine get_side_view(conflicts, n_conflicts, current_idx, side, file_view, n_lines, conflict_start, conflict_end)
        type(conflict_t), intent(in) :: conflicts(:)
        integer, intent(in) :: n_conflicts, current_idx
        integer, intent(in) :: side  ! 1=incoming, 2=local
        character(len=:), allocatable, dimension(:), intent(out) :: file_view
        integer, intent(out) :: n_lines
        integer, intent(out) :: conflict_start, conflict_end
        integer :: i, j, view_idx, line_num, n_res_lines
        character(len=:), allocatable, dimension(:) :: resolution_lines
        logical :: in_conflict

        if (n_conflicts == 0 .or. .not. allocated(conflicts(1)%full_file)) return

        ! Allocate output
        allocate(character(len=1024) :: file_view(conflicts(1)%total_file_lines * 2))
        view_idx = 0
        conflict_start = 0
        conflict_end = 0

        ! Walk through the file line by line
        line_num = 1
        do while (line_num <= conflicts(1)%total_file_lines)
            ! Check if this line starts a conflict
            in_conflict = .false.
            do i = 1, n_conflicts
                if (line_num == conflicts(i)%start_line) then
                    in_conflict = .true.

                    ! Mark if this is the current conflict
                    if (i == current_idx) then
                        conflict_start = view_idx + 1
                    end if

                    ! Get resolution for this conflict
                    if (i == current_idx) then
                        ! For current conflict, use specified side
                        if (side == 1) then
                            resolution_lines = conflicts(i)%incoming_lines
                        else
                            resolution_lines = conflicts(i)%local_lines
                        end if
                    else
                        ! For other conflicts, use their choice (or incoming if no choice)
                        select case (conflicts(i)%choice)
                        case (1)
                            resolution_lines = conflicts(i)%incoming_lines
                        case (2)
                            resolution_lines = conflicts(i)%local_lines
                        case (3)  ! Both
                            if (allocated(conflicts(i)%incoming_lines) .and. allocated(conflicts(i)%local_lines)) then
                                n_res_lines = size(conflicts(i)%incoming_lines) + size(conflicts(i)%local_lines)
                                allocate(character(len=1024) :: resolution_lines(n_res_lines))
                                do j = 1, size(conflicts(i)%incoming_lines)
                                    resolution_lines(j) = conflicts(i)%incoming_lines(j)
                                end do
                                do j = 1, size(conflicts(i)%local_lines)
                                    resolution_lines(size(conflicts(i)%incoming_lines) + j) = conflicts(i)%local_lines(j)
                                end do
                            end if
                        case default  ! No choice yet - show incoming
                            resolution_lines = conflicts(i)%incoming_lines
                        end select
                    end if

                    ! Add resolution lines
                    if (allocated(resolution_lines)) then
                        do j = 1, size(resolution_lines)
                            view_idx = view_idx + 1
                            file_view(view_idx) = resolution_lines(j)
                        end do
                        deallocate(resolution_lines)
                    end if

                    ! Mark end if this is the current conflict
                    if (i == current_idx) then
                        conflict_end = view_idx
                    end if

                    ! Skip to end of conflict
                    line_num = conflicts(i)%end_line + 1
                    exit
                end if
            end do

            ! If not in conflict, copy the line
            if (.not. in_conflict) then
                view_idx = view_idx + 1
                file_view(view_idx) = conflicts(1)%full_file(line_num)
                line_num = line_num + 1
            end if
        end do

        n_lines = view_idx

    end subroutine get_side_view

end module conflict_parser
