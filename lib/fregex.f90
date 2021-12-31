module fregex
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use pcre_constants
    implicit none
    private

    type group_t
        character(:), allocatable :: content
    end type

    type, public :: regex_t
        type(C_ptr) :: pcre_ptr
        type(group_t), allocatable :: groups(:)
        logical :: matches = .false.
    contains
        procedure :: full_info
        procedure :: group
        procedure :: compile
        procedure :: match
        procedure :: free
    end type

    interface
        ! pcre *pcre_compile(const char *pattern, int options,
        !                    const char **errptr, int *erroffset,
        !                    const unsigned char *tableptr);
        function c_pcre_compile(pattern, options, errptr, erroffset, &
            tableptr) result(pcre) bind(C, name="pcre_compile")
            import
            type(c_ptr) :: pcre
            character(len=1, kind=c_char), intent(in) :: pattern(*)
            integer(c_int), intent(in), value :: options
            type(c_ptr), intent(inout) :: errptr
            integer(c_int), intent(out) :: erroffset
            type(c_ptr), value, intent(in) :: tableptr
        end function

        ! int pcre_exec(const pcre*, const pcre_extra*, const char*,
        !               int, int, int, int*, int);
        function c_pcre_exec(code, extra, subject, length, startoffset, &
            &                options, ovector, ovecsize) &
            &                result(error) bind(C, name="pcre_exec")
            import
            type(c_ptr), value, intent(in) :: code
            type(c_ptr), value, intent(in) :: extra
            character(len=1,kind=c_char), intent(in) :: subject(*)
            integer(c_int), value, intent(in) :: length
            integer(c_int), value, intent(in) :: startoffset
            integer(c_int), value, intent(in) :: options
            integer(c_int), value, intent(in) :: ovecsize
            integer(c_int), dimension(ovecsize), intent(out) :: ovector
            integer(c_int) :: error
        end function

            ! int pcre_fullinfo(const pcre*, const pcre_extra*, int, void*);
        function c_pcre_fullinfo(code, extra, what, where) &
            result(error) bind(C, name="pcre_fullinfo")
            import
            type(c_ptr), value, intent(in) :: code
            type(c_ptr), value, intent(in) :: extra
            integer(c_int), value, intent(in) :: what
            type(c_ptr), intent(out) :: where
            integer(c_int) :: error
        end function

        subroutine c_pcre_free(code) bind(C, name="pcre_free")
            import
            type(c_ptr) :: code
        end subroutine c_pcre_free
    end interface

contains

    subroutine compile(self, pattern, flags)
        class(regex_t) :: self
        character(*), intent(in) :: pattern
        character(len=len(pattern)+1, kind=c_char) :: c_pattern
        type(C_ptr) :: c_error_msg
        integer(C_int)    :: error_offset

        integer :: flags_ = PCRE_CONFIG_UTF8
        integer(C_int), optional :: flags
        if (present(flags)) flags_ = flags

        ! C - strings must be NULL terminated
        c_pattern = pattern // c_null_char

        self % pcre_ptr = c_pcre_compile(c_pattern, flags_, c_error_msg, &
        &                       error_offset, c_null_ptr)

        if (.not. c_associated(self % pcre_ptr)) then
            stop
        end if
    end subroutine

    subroutine match(self, string, flags, extra, info)
        class(regex_t) :: self

        character(*) :: string
        character(len=len(string)+1, kind=c_char) :: c_string
        integer :: start_offset = 0

        integer, parameter :: max_size = 30
        integer :: ovector(0:max_size-1)
        integer :: ret_code

        integer, optional     :: flags
        integer               :: flags_ = pcre_config_utf8
        type(c_ptr), optional :: extra
        type(c_ptr)           :: extra_ = c_null_ptr
        integer, optional     :: info
        integer :: i, start, ending, num_groups

        if (present(flags)) flags_ = flags
        if (present(extra)) extra_ = extra

        ! C - Strings must be null terminated
        c_string = string // c_null_char
        ret_code = c_pcre_exec(self % pcre_ptr, extra_, &
        &                      C_string, len(string),   &
        &                      start_offset, flags_,    &
        &                      ovector, size(ovector))

        self % matches = .false.

        ! if info is not provided the code will 
        ! be terminated if the ret_code is negative (error)
        ! except if it's a PCRE_ERROR_NOMATCH
        if (ret_code < -1) then
            if (present(info)) then
                info = ret_code
                return
            else
                error stop ret_code
            end if
        end if

        num_groups = ret_code

        if (allocated(self % groups)) deallocate(self % groups)
        allocate(self % groups(num_groups))

        do i = 1, num_groups-1
            start  = ovector(2*i) + 1
            ending = ovector(2*i + 1)
            self % groups(i) % content = string(start:ending)
        end do
        self % matches = .true.
    end subroutine

    function group(self, id) result(buff)
        class(regex_t) :: self
        character(:),allocatable :: buff
        integer :: id
        buff = self % groups(id) % content
    end function

    function full_info(self, extra, what, where) result(error)
        class(regex_t) :: self
        type(c_ptr), intent(in) :: extra
        integer(C_int), intent(in) :: what
        type(C_ptr), intent(out) :: where
        integer(C_int) :: error
        error = c_pcre_fullinfo(self % pcre_ptr, extra, what, where)
    end function

    subroutine free(self)
        class(regex_t) :: self
        integer :: i
        call c_pcre_free(self % pcre_ptr)
        do i=1,size(self % groups)
            deallocate(self % groups(i) % content)
        end do
        deallocate(self % groups)
    end subroutine
end module
