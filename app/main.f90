program simple_example
    use fregex
    use pcre_constants
    use iso_c_binding
    implicit none
    type(regex_t) :: re
    type(match_t) :: match
    type(group_t) :: group

    character(:), allocatable :: subject
    character(:), allocatable :: pattern
    integer :: i, j, info

    ! Tests some invalid patterns / matches
    pattern = "1) should not compile!"
    subject = "2) should not match!"

    call re % compile(pattern, info=info)
    if (info /= PCRE_SUCCESS) &
        print '("As expected: it didn''t compile (",i0," bytes)")', re % memory_usage()

    call re % match(subject, info=info)
    if (info /= PCRE_SUCCESS) &
        print '("As expected: it didn''t match (",i0," matches)")', size(re % matches)

    ! Test a greedy pattern in a case sensitive context
    pattern = "(N.*T)"
    subject = "HFDEXVGBGTNVtGTGEDGBYHNVTGTHYHYNN"
    !                      ^ - lowercase t
    ! Compiling with ungreedy and caseless flags
    call re % compile(pattern, flags=[PCRE_UNGREEDY, PCRE_CASELESS], info=info)
    if (info == PCRE_SUCCESS) then
        print '("Compiled successfully: ''", A, "'' (",i0," bytes)")', pattern, re % memory_usage()
        print '(" With flags: ",z0.4)', re % options()
        print '("   PCRE_UNGREEDY: ",z0.4)', PCRE_UNGREEDY
        print '("   PCRE_CASELESS: ",z0.4)', PCRE_CASELESS
    end if

    call re % match(subject, info=info)
    ! If no error is reported
    if (info == PCRE_SUCCESS) then
        print '("Found ",i0," matches: ")', size(re % matches)
        do i=1,size(re % matches)
            match = re % matches(i)
            print '("- match #",i0," ",A)', i, subject(match%start:match%ending)
            do j=1,size(match % groups)
                group = match % groups(j)
                print '("   - group #",i0," ",A," ",A)', j, subject(group%start:group%ending)
            end do
        end do
    end if
    call re % free()

end program
