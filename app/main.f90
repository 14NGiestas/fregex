program simple_example
    use fregex
    implicit none
    type(regex_t) :: re

    character(:), allocatable :: string
    character(:), allocatable :: pattern
    integer :: info

    ! Operations
    pattern = "(.+)([\+\-\*\/]{1})(.+)"
    string = "1/sin(x)"
    call re % compile(pattern)
    call re % match(string)
    if (re % matches) then
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, 1, re % group(1) ! 1
        print*, 2, re % group(2) ! /
        print*, 3, re % group(3) ! sin(x)
    end if

    string = "-1/sqrt(2)"
    call re % match(string)
    if (re % matches) then
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, 1, re % group(1) ! -1
        print*, 2, re % group(2) ! /
        print*, 3, re % group(3) ! sqrt(2)
    end if

    ! Math functions
    pattern = "([A-z]+)\((.+)\)"
    string = "sqrt(1/2)"
    call re % compile(pattern)
    call re % match(string)
    if (re % matches) then
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, 1, re % group(1) ! sqrt
        print*, 2, re % group(2) ! 1/2
    end if

    string = "Sqrt(2)"
    call re % match(string)

    string = "SQRT(3)"
    call re % match(string)

    ! Complex numbers
    pattern = "([+-]?[0-9]*[.]?[0-9]+)([+-]{1}[0-9]*[.]?[0-9]+)[ijIJ]{1}"
    string = "2.5+7.5i"
    call re % compile(pattern)
    call re % match(string)
    if (re % matches) then
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, 1, re % group(1) ! 2.5
        print*, 2, re % group(2) !+7.5
    end if
    string = "5+5j"
    call re % match(string)
    if (re % matches) then
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, 1, re % group(1) ! 5
        print*, 2, re % group(2) !+5
    end if
    string = "4.33-5.66j"
    call re % match(string)
    if (re % matches) then
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, 1, re % group(1) ! 4.33
        print*, 2, re % group(2) !-5.66
    end if

    ! It is a complex number (it shouldn't match with this specific pattern)
    call re % match("10j")
    print*, re % matches
end program
