module fregex
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use pcre_constants
    use fmatch
    implicit none
    integer, parameter :: STDERR = 4

    type Regex
        type(C_ptr) :: pcre_ptr
    contains
        procedure :: fullinfo => fregex_fullinfo
        procedure :: compile  => fregex_compile
        procedure :: match    => fregex_match
        procedure :: free     => fregex_free
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

    subroutine check_pcre_error(error)
        integer, intent(in) :: error
        if (error < 0) then
            select case(error)
            case (0)
                print*, "Vector Overflow."
                stop
            case (PCRE_ERROR_NOMATCH)
                print*,"No match."
            case (PCRE_ERROR_NULL)
                print*, "PCRE_PTR is null."
                stop
            case (PCRE_ERROR_BADOPTION)
                print*, "Bad option."
                stop
            case (PCRE_ERROR_BADMAGIC)
                print*, "Bad magic."
                stop
            case (PCRE_ERROR_UNKNOWN_OPCODE)
                print*, "Unknown OpCode"
                stop
            case default
                print*,"Matching error ", error
                stop
            end select
        end if
    end subroutine

    subroutine fregex_compile(self, pattern, flags)
        implicit none
        class(Regex) :: self
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

        if (.not. C_associated(self % pcre_ptr)) then
            stop
        end if
    end subroutine

    function fregex_match(self, string, flags, extra) result(res_match)
        implicit none
        class(Regex) :: self

        character(*) :: string
        character(len=len(string)+1, kind=C_char) :: C_string

        type(Match) :: res_match
        integer :: start_offset = 0

        integer, parameter :: max_size = 30
        integer :: ovector(0:max_size-1)
        integer :: ret_code

        integer               :: flags_ = PCRE_CONFIG_UTF8
        type(C_ptr)           :: extra_ = C_NULL_PTR
        integer, optional     :: flags
        type(C_ptr), optional :: extra 

        if (present(flags)) flags_ = flags
        if (present(extra)) extra_ = extra

        ! C - Strings must be null terminated
        C_string = string // C_null_char
        ret_code = c_pcre_exec(self % pcre_ptr, extra_, &
        &                      C_string, len(string),   &
        &                      start_offset, flags_,    &
        &                      ovector, size(ovector))

        call check_pcre_error(ret_code)
        res_match = Match_(string, vector=ovector, num_groups=ret_code)
    end function

    function fregex_fullinfo(self, extra, what, where) result(error)
        implicit none
        class(Regex) :: self
        type(C_ptr), intent(in) :: extra
        integer(C_int), intent(in) :: what
        type(C_ptr), intent(out) :: where
        integer(C_int) :: error
        error = c_pcre_fullinfo(self % pcre_ptr, extra, what, where)
    end function

    subroutine fregex_free(self)
        implicit none
        class(Regex) :: self
        call c_pcre_free(self % pcre_ptr)
    end subroutine
end module
