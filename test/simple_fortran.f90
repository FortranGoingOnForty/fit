program calculate_area
    implicit none
    real :: radius, area
    real, parameter :: pi = 3.14159265359

    print *, "Enter radius:"
    read *, radius

<<<<<<< HEAD
    ! Calculate area using precise formula
    area = pi * radius * radius
    print *, "Circle area (precise):", area
=======
    ! Calculate area using approximation
    area = 3.14 * radius ** 2
    print *, "Circle area (approx):", area
>>>>>>> feature-approx

    print *, "Thank you for using the calculator!"

end program calculate_area
