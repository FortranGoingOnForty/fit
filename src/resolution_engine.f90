module resolution_engine
    use conflict_parser
    implicit none
    private

    public :: write_resolved_file, all_conflicts_resolved

contains

    ! Check if all conflicts have been resolved
    function all_conflicts_resolved(conflicts, n) result(resolved)
        type(conflict_t), intent(in) :: conflicts(:)
        integer, intent(in) :: n
        logical :: resolved
        integer :: i

        resolved = .true.
        do i = 1, n
            if (conflicts(i)%choice == 0) then
                resolved = .false.
                return
            end if
        end do
    end function all_conflicts_resolved

    ! Write the resolved file back to disk
    subroutine write_resolved_file(filename, conflicts, n_conflicts, success)
        character(len=*), intent(in) :: filename
        type(conflict_t), intent(in) :: conflicts(:)
        integer, intent(in) :: n_conflicts
        logical, intent(out) :: success

        integer :: in_unit, out_unit, ios, line_num, i, j
        character(len=1024) :: line
        integer :: current_conflict
        logical :: in_conflict_region
        character(len=256) :: temp_filename

        success = .false.
        current_conflict = 0
        in_conflict_region = .false.

        ! Create temporary file
        temp_filename = trim(filename) // '.fit_tmp'

        ! Open input and output files
        open(newunit=in_unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) return

        open(newunit=out_unit, file=temp_filename, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            close(in_unit)
            return
        end if

        ! Process file line by line
        line_num = 0
        do
            read(in_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit

            line_num = line_num + 1

            ! Check if we're at the start of a conflict
            if (index(trim(line), '<<<<<<<') == 1) then
                in_conflict_region = .true.
                current_conflict = current_conflict + 1

                ! Write the resolved content based on choice
                if (current_conflict <= n_conflicts) then
                    call write_resolution(out_unit, conflicts(current_conflict))
                end if
                cycle
            end if

            ! Skip lines inside conflict region
            if (in_conflict_region) then
                if (index(trim(line), '>>>>>>>') == 1) then
                    in_conflict_region = .false.
                end if
                cycle
            end if

            ! Write non-conflict lines
            write(out_unit, '(A)') trim(line)
        end do

        close(in_unit)
        close(out_unit)

        ! Replace original file with resolved version
        call execute_command_line('mv "' // trim(temp_filename) // '" "' // trim(filename) // '"', exitstat=ios)
        success = (ios == 0)

    end subroutine write_resolved_file

    ! Write the resolution for a single conflict
    subroutine write_resolution(unit, conflict)
        integer, intent(in) :: unit
        type(conflict_t), intent(in) :: conflict
        integer :: i

        select case (conflict%choice)
        case (1)  ! Incoming
            if (allocated(conflict%incoming_lines)) then
                do i = 1, size(conflict%incoming_lines)
                    write(unit, '(A)') trim(conflict%incoming_lines(i))
                end do
            end if

        case (2)  ! Local
            if (allocated(conflict%local_lines)) then
                do i = 1, size(conflict%local_lines)
                    write(unit, '(A)') trim(conflict%local_lines(i))
                end do
            end if

        case (3)  ! Both
            if (allocated(conflict%incoming_lines)) then
                do i = 1, size(conflict%incoming_lines)
                    write(unit, '(A)') trim(conflict%incoming_lines(i))
                end do
            end if
            if (allocated(conflict%local_lines)) then
                do i = 1, size(conflict%local_lines)
                    write(unit, '(A)') trim(conflict%local_lines(i))
                end do
            end if

        case default
            ! Should not happen if all_conflicts_resolved was called first
            write(unit, '(A)') '!!! UNRESOLVED CONFLICT !!!'
        end select

    end subroutine write_resolution

end module resolution_engine
