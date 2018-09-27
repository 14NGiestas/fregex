! Test the Fortran PCRE wrapper
!
! Examples taken from
! https://github.com/niklongstone/regular-expression-cheat-sheet
program test_pcre

    use, intrinsic :: iso_c_binding
    use fregex

    implicit none
    type token_type
     character(len=:), allocatable :: token
    end type
    character(:), allocatable :: pattern, subject
    type(token_type), dimension(:), allocatable :: tokens

    type(pcre_type) :: regex

    integer, parameter :: ovecsize = 30
    integer, dimension(0:ovecsize-1) :: ovector

    integer :: error
    integer :: total_matches


    pattern = "([+-]?[0-9]*[.]?[0-9]+)([+-]{1}[0-9]*[.]?[0-9]+)[ijIJ]{1}";
    subject = "8+4j 8.33+4i"
    regex = pcre_compile(pattern, 0)

    if (.not. c_associated(regex%regex)) then
       print*,"PCRE compilation failed"
       stop
    end if

    error = pcre_exec(regex, c_null_ptr, subject, 0, 0, ovector)


    total_matches = error
    allocate(tokens(total_matches))

    block
      integer :: substring_start
      integer :: substring_end
      integer :: i

      do i = 0, error - 1
         substring_start = ovector(2*i) + 1
         substring_end = ovector(2*i + 1)
         tokens(i+1) = token_type(subject(substring_start:substring_end))
      end do
    end block

    block
      integer :: options
      integer :: start_offset

      do
         options = 0
         start_offset = ovector(1)

         if (ovector(0) == ovector(1)) then
            if (ovector(0) == len(subject)) exit
         end if

         error = pcre_exec(regex, c_null_ptr, subject, start_offset, options, ovector)

         if (error == PCRE_ERROR_NOMATCH) then
            if (options == 0) exit
            ovector(1) = start_offset + 1
            continue
         end if

         if (error < 0) then
            print('(A,I0)'), "Matching error ", error
            allocate(tokens(0))
            return
         end if

         block
           integer :: i
           integer :: substring_start
           integer :: substring_end
           type(token_type) :: found_token

           do i = 0, error-1
              total_matches = total_matches + 1
              substring_start = ovector(2*i) + 1
              substring_end = ovector((2*i)+1)
              found_token = token_type(subject(substring_start:substring_end))
              tokens = [tokens, found_token]
           end do
         end block
      end do
    end block

    block
        integer :: i
        do i=1,size(tokens)
            print*, i, tokens(i) % token
        end do
    end block

end program test_pcre
