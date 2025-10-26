module pane_state
    implicit none
    private

    public :: pane_t, init_pane_state, reset_scroll, scroll_up, scroll_down
    public :: select_next_pane, select_prev_pane, get_active_pane
    public :: PANE_INCOMING, PANE_LOCAL, PANE_PREVIEW

    ! Pane identifiers
    integer, parameter :: PANE_INCOMING = 1
    integer, parameter :: PANE_LOCAL = 2
    integer, parameter :: PANE_PREVIEW = 3

    ! Pane state type
    type :: pane_t
        integer :: active_pane             ! Currently active pane (1=incoming, 2=local, 3=preview)
        integer :: scroll_incoming         ! Scroll offset for incoming pane
        integer :: scroll_local            ! Scroll offset for local pane
        integer :: scroll_preview          ! Scroll offset for preview pane
        integer :: max_lines_incoming      ! Total lines available in incoming
        integer :: max_lines_local         ! Total lines available in local
        integer :: max_lines_preview       ! Total lines available in preview
    end type pane_t

contains

    ! Initialize pane state
    subroutine init_pane_state(pane)
        type(pane_t), intent(out) :: pane

        pane%active_pane = PANE_INCOMING
        pane%scroll_incoming = 0
        pane%scroll_local = 0
        pane%scroll_preview = 0
        pane%max_lines_incoming = 0
        pane%max_lines_local = 0
        pane%max_lines_preview = 0
    end subroutine init_pane_state

    ! Reset scroll positions
    subroutine reset_scroll(pane)
        type(pane_t), intent(inout) :: pane

        pane%scroll_incoming = 0
        pane%scroll_local = 0
        pane%scroll_preview = 0
    end subroutine reset_scroll

    ! Scroll up in active pane
    subroutine scroll_up(pane)
        type(pane_t), intent(inout) :: pane

        select case (pane%active_pane)
        case (PANE_INCOMING)
            pane%scroll_incoming = max(0, pane%scroll_incoming - 1)
        case (PANE_LOCAL)
            pane%scroll_local = max(0, pane%scroll_local - 1)
        case (PANE_PREVIEW)
            pane%scroll_preview = max(0, pane%scroll_preview - 1)
        end select
    end subroutine scroll_up

    ! Scroll down in active pane
    subroutine scroll_down(pane, visible_lines)
        type(pane_t), intent(inout) :: pane
        integer, intent(in) :: visible_lines
        integer :: max_scroll

        select case (pane%active_pane)
        case (PANE_INCOMING)
            max_scroll = max(0, pane%max_lines_incoming - visible_lines)
            pane%scroll_incoming = min(max_scroll, pane%scroll_incoming + 1)
        case (PANE_LOCAL)
            max_scroll = max(0, pane%max_lines_local - visible_lines)
            pane%scroll_local = min(max_scroll, pane%scroll_local + 1)
        case (PANE_PREVIEW)
            max_scroll = max(0, pane%max_lines_preview - visible_lines)
            pane%scroll_preview = min(max_scroll, pane%scroll_preview + 1)
        end select
    end subroutine scroll_down

    ! Select next pane (wraps around)
    subroutine select_next_pane(pane)
        type(pane_t), intent(inout) :: pane

        pane%active_pane = pane%active_pane + 1
        if (pane%active_pane > PANE_PREVIEW) then
            pane%active_pane = PANE_INCOMING
        end if
    end subroutine select_next_pane

    ! Select previous pane (wraps around)
    subroutine select_prev_pane(pane)
        type(pane_t), intent(inout) :: pane

        pane%active_pane = pane%active_pane - 1
        if (pane%active_pane < PANE_INCOMING) then
            pane%active_pane = PANE_PREVIEW
        end if
    end subroutine select_prev_pane

    ! Get currently active pane
    function get_active_pane(pane) result(active)
        type(pane_t), intent(in) :: pane
        integer :: active

        active = pane%active_pane
    end function get_active_pane

end module pane_state
