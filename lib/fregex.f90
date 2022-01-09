module fregex
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use pcre_interfaces
    use pcre_constants
    implicit none
    private

    integer, parameter :: DEFAULT_MAX_GROUPS = 10
    integer :: MAX_SIZE = 3*(DEFAULT_MAX_GROUPS+1)

    type, public :: group_t
        integer :: start
        integer :: ending
        character(:), allocatable :: content
    end type

    type, public :: match_t
        integer :: start
        integer :: ending
        type(group_t), allocatable :: groups(:)
    end type

    type, public :: regex_t
        type(c_ptr), private :: re    = C_NULL_PTR
        type(c_ptr), private :: study = C_NULL_PTR
        type(match_t), allocatable :: matches(:)
    contains
        procedure :: full_info
        procedure :: options
        procedure :: memory_usage
        procedure :: capture_count
        procedure :: compile
        procedure :: match
        procedure :: free
    end type

contains

    subroutine compile(self, pattern, flags, info)
        class(regex_t) :: self
        character(*),   intent(in) :: pattern
        integer(c_int), intent(in),  optional :: flags(:)
        integer,        intent(out), optional :: info
        character(len=len(pattern)+1, kind=c_char) :: c_pattern
        type(C_ptr)    :: c_error_msg
        integer(c_int) :: error_offset
        character(c_char), pointer :: c_string(:)
        character(:), allocatable :: error_msg
        integer :: n, capture_count, reduced_flags

        if (present(flags)) then
            ! Reduces a array of flags into a number using bitwise OR
            reduced_flags = iany(flags)
        else
            reduced_flags = PCRE_CONFIG_UTF8
        end if
        if (present(info)) info = 0

        ! C - strings must be NULL terminated
        c_pattern = pattern // C_NULL_CHAR

        self % re = c_pcre_compile(c_pattern,     & ! The compiled pattern
                                   reduced_flags, & ! Zero or more option bits
                                   c_error_msg,   & ! Where to put an error message
                                   error_offset,  & ! Offset in pattern where error was found
                                   C_NULL_PTR)      ! Use default character tables


        if (.not. c_associated(self % re)) then
            if (present(info)) then
                ! Send the index (with the minus sign) to where the problem occured
                info = -error_offset
                return
            else
                ! Retrieve string from pointer c_error_msg
                call c_f_pointer(c_error_msg, c_string, [huge(0)])
                n = 1
                do while(c_string(n) /= C_NULL_CHAR)
                    error_msg = error_msg // c_string(n)
                    n = n + 1
                end do
                ! Show and terminate the program
                write(error_unit,'(A)') pattern
                write(error_unit,'(A)') repeat(' ',error_offset)//'1'
                write(error_unit,'("Compile error at (1): ",A)') error_msg
                error stop
            end if
        end if

        ! Update the MAX_SIZE so ovector has enough room for
        ! the needed capturing groups
        capture_count = self % capture_count()
        MAX_SIZE = 3*(capture_count+1)
    end subroutine

    subroutine match(self, string, flags, extra, info)
        class(regex_t) :: self
        character(*) :: string
        integer,     intent(in), optional :: flags(:)
        type(c_ptr), intent(in), optional :: extra
        integer,     intent(out), optional :: info
        character(len=len(string)+1, kind=c_char) :: c_string
        integer :: ovector(0:MAX_SIZE-1)
        integer :: ret_code
        integer :: i, start, ending, start_offset, num_groups
        type(match_t) :: new_match

        integer     :: reduced_flags
        type(c_ptr) :: extra_

        if (present(flags)) then
            ! Reduces a array of flags into a number using bitwise OR
            reduced_flags = iany(flags)
        else
            reduced_flags = PCRE_CONFIG_UTF8
        end if

        if (present(extra)) then
            extra_ = extra
        else
            extra_ = C_NULL_PTR
        end if

        if (present(info)) info = 0

        if (allocated(self % matches)) deallocate(self % matches)
        ovector = 0
        start_offset = 0
        c_string = string // C_NULL_CHAR ! C - Strings must be null terminated
        do while(start_offset < len(string))

            ! Execute the PCRE
            ret_code = c_pcre_exec( &
                    self % re,     & ! Points to the compiled pattern
                    extra_,        & ! Points to an associated pcre[16|32]_extra structure or is NULL
                    c_string,      & ! Points to the subject string
                    len(string),   & ! Length of the subject string
                    start_offset,  & ! Offset in the subject at which to start matching
                    reduced_flags, & ! Option bits
                    ovector,       & ! Points to a vector of ints for result offsets
                    size(ovector))   ! Number of elements in the vector (a multiple of 3)

            ! Handle error cases
            if (ret_code < 0) then
                ! If the start_offset /= 0, a error code should not be propagated
                ! because there was a match before
                if (present(info) .and. start_offset == 0) then
                    info = ret_code
                else
                    ! A no match should not stop the code
                    if (ret_code < -1) error stop ret_code
                end if
                return
            end if

            num_groups = ret_code-1

            ! Generate a new_match entry
            allocate(new_match % groups(num_groups))
            new_match % start  = ovector(0)+1
            new_match % ending = ovector(1)
            start_offset = new_match % ending
            do i=1,num_groups
                start  = ovector(2*i) + 1
                ending = ovector(2*i + 1)
                new_match % groups(i) % start = start
                new_match % groups(i) % ending = ending
                new_match % groups(i) % content = string(start:ending)
            end do

            ! FIXME It may not be very efficient for too many matches
            ! Allocate or expand each new match
            if (.not. allocated(self % matches)) then
                self % matches = [ new_match ]
            else
                self % matches = [ self % matches, new_match ]
            end if
            deallocate(new_match % groups)
        end do
    end subroutine

    ! FIXME it only deals with integer answers
    function full_info(self, what, info)
        class(regex_t) :: self
        integer(c_int), intent(in) :: what
        integer, intent(out), optional :: info
        integer(c_int), target :: full_info
        integer :: info_
        type(c_ptr) :: c_address
        c_address = c_loc(full_info)
        full_info = 0
        info_ = c_pcre_fullinfo(self % re, &
                                self % study, &
                                what, &
                                c_address)
        !TODO handle possible error codes
        if (present(info)) info = info_
    end function

    integer function options(self, info)
        !! Returns the compiled pattern flags (reduced)
        class(regex_t) :: self
        integer, intent(out), optional :: info
        options = self % full_info(PCRE_INFO_OPTIONS, info)
    end function

    integer function capture_count(self, info)
        !! Returns the number of capturing groups in the compiled pattern
        class(regex_t) :: self
        integer, intent(out), optional :: info
        capture_count = self % full_info(PCRE_INFO_CAPTURECOUNT, info)
    end function

    integer function memory_usage(self, info)
        !! Returns the memory usage of the compiled pattern in bytes
        class(regex_t) :: self
        integer, intent(out), optional :: info
        memory_usage = self % full_info(PCRE_INFO_SIZE, info)
    end function

    subroutine free(self)
        class(regex_t) :: self
        if (c_associated(self % re)) then
            call c_pcre_free(self % re)
            self % re = C_NULL_PTR
        end if
        if (c_associated(self % study)) then
            call c_pcre_free(self % study)
            self % study = C_NULL_PTR
        end if
    end subroutine
end module
