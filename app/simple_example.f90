program simple_example
    use fregex
    implicit none
    type(regex_t) :: re
    type(match_t) :: match

    character(:), allocatable :: string
    character(:), allocatable :: pattern

    ! Operations
    pattern = "(.+)([\+\-\*\/]{1})(.+)"
    string = "1/sin(x)"
    call re % compile(pattern)
    call re % match(string)
    if (size(re % matches) > 0) then
        match = re % matches(1) ! Gets the first match
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, "First Group: ",  match % groups(1) % content ! 1
        print*, "Second Group: ", match % groups(2) % content ! /
        print*, "Third Group: ",  match % groups(3) % content ! sin(x)
    end if
end program
