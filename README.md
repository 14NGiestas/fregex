# Fregex - Yet another PCRE wrapper to fortran

This project aims to provide a good suport for Perl Compatible Regex just like other 
famous existing languages: Python, C, C++...

# Example
```f90
program simple_example
    use fregex
    implicit none
    type(regex_t) :: re

    character(:), allocatable :: string
    character(:), allocatable :: pattern

    ! Operations
    pattern = "(.+)([\+\-\*\/]{1})(.+)"
    string = "1/sin(x)"
    call re % compile(pattern)
    call re % match(string)
    if (re % matches) then
        print*, "string: ", string
        print*, "pattern: ", pattern
        print*, "First Group: ",  re % group(1) ! 1
        print*, "Second Group: ", re % group(2) ! /
        print*, "Third Group: ",  re % group(3) ! sin(x)
    end if
end program
```

# Requirements
- libpcre

## Getting Started
### FPM

This project supports the [Fortran Package Manager](https://github.com/fortran-lang/fpm). Follow the directions on that page to install FPM if you haven't already.

First get the code, by cloning the repo:

```sh
git clone https://github.com/14NGiestas/fregex.git
cd fregex/
fpm run
```

### Using as a dependency in FPM

Add a entry in the "dependencies" section of your project's fpm.toml

```toml
# fpm.toml
[ dependencies ]
fregex = { git = "https://github.com/14NGiestas/fregex.git" }
```

