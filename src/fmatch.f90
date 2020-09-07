module fmatch
    type Match
        character(:), allocatable :: groups(:)
        logical :: matches = .false.
    contains
        procedure :: group
        procedure :: free
    end type

    interface size
        module procedure size_
    end interface
    
    interface Match
        module procedure constructor
    end interface

contains

    function constructor(string, vector, num_groups) result(self)
        implicit none
        type(Match) :: self
        character(*) :: string
        integer :: vector(0:)
        integer :: i, start, end_
        integer :: max_size, num_groups
        allocate(self % groups(0:num_groups-1))
        
        do i = 0, num_groups-1
            start = vector(2*i) + 1
            end_  = vector(2*i + 1)
            self % groups(i) = string(start:end_)
            self % matches = .true.
        end do
    end function

    function group(self, id) result(buff)
        implicit none
        class(Match) :: self
        character(:),allocatable :: buff
        integer :: id
        buff = self % groups(id)
    end function

    function size_(self)
        class(Match) :: self
        match_size = size(self % groups)
    end function

    subroutine free(self)
        class(Match) :: self
        deallocate(self % groups)
    end subroutine
end module
