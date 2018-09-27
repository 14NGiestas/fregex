# FRegex
## Yet another PCRE wrapper to fortran
This project aims to provide a good suport for Regex just like other 
famous existing languages: Python, C, C++...

# Requirements
- libpcre
- Fortran Compiler:
    - gfortran or ifort

# Examples

```fortran
type(Regex) :: re
type(Match) :: m
character(:), allocatable :: pattern

pattern = "([+-]{1}?[0-9]+[.]?[0-9]+)([+-]{1}[0-9]+[.]?[0-9]+)i"
m = re % match("-2.5+5.5i")
if (m % matches) then
    print*, "Entire Match: ", m % group(0)
    print*, "First Group: ", m % group(1)
    print*, "Second Group: ", m % group(2)
    print*, "Third Group: ", m % group(3)
end if
```
