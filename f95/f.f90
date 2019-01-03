function area(r, theta)
        real, intent(in) :: r, theta
        real :: area
        area = 0.5 * r**2 * theta
end function area

program filetest
        real :: pi = 3.14195, tau, unit_area
        tau = pi * 2
        unit_area = area(1.0, tau)

        open ( & ! '&' line-continuation
                unit=myfile, & ! file handler
                file="pi.txt", &
                action="write", &
                position="append")

        write(myfile,*) "pi ", "tau ", "unit_area"
        write(myfile,*) pi, tau, unit_area
        write(*,*) pi, tau, unit_area
end program filetest
