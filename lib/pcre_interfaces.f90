module pcre_interfaces
    use iso_c_binding
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
            type(c_ptr),    value, intent(in) :: code
            type(c_ptr),    value, intent(in) :: extra
            integer(c_int), value, intent(in) :: what
            type(c_ptr),    value, intent(in) :: where
            integer(c_int) :: error
        end function

        ! void pcre_free(const pcre*);
        subroutine c_pcre_free(code) bind(C, name="free")
            import
            type(c_ptr), value, intent(in) :: code
        end subroutine
    end interface
end module
