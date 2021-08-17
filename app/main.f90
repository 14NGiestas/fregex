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
        print*, "Entire Match: ", re % group(1) ! 1/sin(x)
        print*, "First Group: ",  re % group(2)
        print*, "Second Group: ", re % group(3)
        print*, "Third Group: ",  re % group(4)
    end if

    string = "-1/sqrt(2)"
    call re % match(string)
    if (re % matches) then
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, re % group(1) ! 1/sqrt(2)
        print*, re % group(2) ! 1
        print*, re % group(3) ! /
        print*, re % group(4) ! sqrt(2)
    end if

    ! Math functions
    pattern = "([A-z]+)\((.+)\)"
    string = "sqrt(1/2)"
    call re % compile(pattern)
    call re % match(string)
    if (re % matches) then
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, re % group(1) ! sqrt(2)
        print*, re % group(2) ! sqrt
        print*, re % group(3) ! 2
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
        print*, re % group(1)
        print*, re % group(2)
        print*, re % group(3)
    end if
    string = "5+5j"
    call re % match(string)
    if (re % matches) then
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, re % group(1) ! Sqrt(2)
        print*, re % group(2) ! Sqrt
        print*, re % group(3) ! 2
    end if
    string = "4.33-5.66j"
    call re % match(string)
    if (re % matches) then
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, re % group(1) ! Sqrt(2)
        print*, re % group(2) ! Sqrt
        print*, re % group(3) ! 2
    end if

    ! It is a complex number (it shouldn't match with this specific pattern)
    call re % match("10j", info=info)
    print*, re % matches, info
end program
