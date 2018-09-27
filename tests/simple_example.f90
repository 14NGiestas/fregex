program simple_example
    use fregex
    implicit none
    type(Regex) :: re
    type(Match) :: m

    character(:), allocatable :: pattern

    allocate(character(100) :: pattern)
    ! Operations
    pattern = "(.+)([\+\-\*\/]{1})(.+)"
    call re % compile(trim(pattern))
    m = re % match("1/sin(x)")
    if (m % matches) then
        print*, "string: 1/sin(x) pattern: ",pattern
        print*, "Entire Match: ", m % group(0) ! 1/sin(x)
        print*, "First Group: ",  m % group(1)
        print*, "Second Group: ", m % group(2)
        print*, "Third Group: ",  m % group(3)
    end if
    m = re % match("-1/sqrt(2)")
    print*, m % group(0) ! 1/sqrt(2)
    print*, m % group(1) ! 1
    print*, m % group(2) ! /
    print*, m % group(3) ! sqrt(2)

    ! Math functions
    pattern = "([A-z]+)\((.+)\)"
    call re % compile(pattern)
    m = re % match("sqrt(1/2)")
    print*, m % group(0) ! sqrt(2)
    print*, m % group(1) ! sqrt
    print*, m % group(2) ! 2
    m = re % match("Sqrt(2)")
    m = re % match("SQRT(3)")

    ! Complex numbers
    pattern = "([+-]?[0-9]*[.]?[0-9]+)([+-]{1}[0-9]*[.]?[0-9]+)[ijIJ]{1}"
    call re % compile(pattern)
    m = re % match("2.5+7.5i")
    print*, m % group(0)
    print*, m % group(1)
    print*, m % group(2)
    m = re % match("5+5j")
    print*, m % group(0) ! Sqrt(2)
    print*, m % group(1) ! Sqrt
    print*, m % group(2) ! 2
    m = re % match("4.33-5.66j")
    print*, m % group(0) ! Sqrt(2)
    print*, m % group(1) ! Sqrt
    print*, m % group(2) ! 2
    m = re % match("10j")
    if (m % matches) then
        print*, "Matched 10j"
        print*, m % group(0) ! Sqrt(2)
        print*, m % group(1) ! Sqrt
    end if 
end program
