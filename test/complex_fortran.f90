module matrix_operations
    implicit none
    private
    public :: matrix_multiply, matrix_transpose

contains

<<<<<<< HEAD
    ! Matrix multiplication using optimized algorithm
    subroutine matrix_multiply(A, B, C, n)
        real, intent(in) :: A(n,n), B(n,n)
        real, intent(out) :: C(n,n)
        integer, intent(in) :: n
        integer :: i, j, k

        ! Loop blocking for cache optimization
        do i = 1, n
            do k = 1, n
                do j = 1, n
                    C(i,j) = C(i,j) + A(i,k) * B(k,j)
                end do
            end do
        end do
    end subroutine matrix_multiply
=======
    ! Matrix multiplication using standard algorithm
    subroutine matrix_multiply(A, B, C, n)
        real, intent(in) :: A(n,n), B(n,n)
        real, intent(out) :: C(n,n)
        integer, intent(in) :: n
        integer :: i, j, k

        ! Standard triple loop
        do i = 1, n
            do j = 1, n
                C(i,j) = 0.0
                do k = 1, n
                    C(i,j) = C(i,j) + A(i,k) * B(k,j)
                end do
            end do
        end do
    end subroutine matrix_multiply
>>>>>>> feature-standard-multiply

    subroutine matrix_transpose(A, AT, m, n)
        real, intent(in) :: A(m,n)
        real, intent(out) :: AT(n,m)
        integer, intent(in) :: m, n
        integer :: i, j

<<<<<<< HEAD
        ! Transpose with explicit loops
        do j = 1, n
            do i = 1, m
                AT(j,i) = A(i,j)
            end do
        end do
=======
        ! Transpose using array syntax
        AT = transpose(A)
>>>>>>> feature-intrinsic-transpose

    end subroutine matrix_transpose

end module matrix_operations
