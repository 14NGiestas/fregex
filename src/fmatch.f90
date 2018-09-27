module fmatch
    type Token
        character(:), allocatable :: string
    end type

    type Match
#ifdef gfortran
        type(Token), allocatable :: groups(:)
#elif ifort
        character(:), allocatable :: groups(:)
#endif
        logical :: matches = .false.
    contains
        procedure :: group => match_group
        procedure :: free  => match_free
    end type

    interface size
        module procedure match_size
    end interface 

contains

    function Match_(string, vector, num_groups) result(self)
        implicit none
        type(Match) :: self
        character(*) :: string
        integer :: vector(0:)
        integer :: i, start, end_
        integer :: max_size, num_groups
#ifdef gfortran
        allocate(self % groups(0:num_groups-1))
#elif ifort
        max_size = vector(1)-vector(0)
        allocate(character(max_size) :: self % groups(0:num_groups-1))
#endif
        do i = 0, num_groups-1
            start = vector(2*i) + 1
            end_  = vector(2*i + 1)
#ifdef gfortran
            self % groups(i) = token(string(start:end_))
#elif ifort 
            self % groups(i) = string(start:end_)
#endif
            self % matches = .true.
        end do
    end function

    function match_group(self, id) result(buff)
        implicit none
        class(Match) :: self
        character(:),allocatable :: buff
        integer :: id
#if gfortran
        buff = self % groups(id) % string
#elif ifort
        buff = self % groups(id)
#endif 
    end function

    function match_size(self)
        class(Match) :: self
        match_size = size(self % groups)
    end function

    subroutine match_free(self)
        class(Match) :: self
        deallocate(self % groups)
    end subroutine
end module
