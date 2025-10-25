module conflict_parser
    implicit none
    private

    public :: conflict_t, parse_conflict_file, conflict_count

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

end module conflict_parser
