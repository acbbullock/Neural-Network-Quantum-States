!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  This module file contains common i/o operations for arrays of real and integer type. Common operations include
!!  printing arrays to std out with a specified format, reading/writing arrays from/to csv files and binary files.
!!
module io_mod
    use, intrinsic :: iso_fortran_env, only: real64, real32, int64, int32, int16, int8, input_unit, output_unit
    implicit none (type,external)
    private

    !~~~~============ Public Module Subprograms ============~~~~!

    public :: arrayprint, csvwrite, echo, csvread, datwrite, datread
    public :: logtoint, logtochar

    !~~~~============ Module Subprograms Interface to Respective Module Procedures in Submodules ============~~~~!

    !~~~~====== Procedures for printing data to stdout ======~~~~!

    ! call arrayprint(x=, fmt=)
    ! rank(x) = 1,2 and kind(x) = r64,r32,i64,i32,i16,i8, & character(len=*)
    interface arrayprint      ! Submodule debugging
        module impure subroutine arrayprint_1dr64(x, fmt)
            real(real64), intent(in) :: x(:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_1dr64
        module impure subroutine arrayprint_1dr32(x, fmt)
            real(real32), intent(in) :: x(:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_1dr32

        module impure subroutine arrayprint_2dr64(x, fmt)
            real(real64), intent(in) :: x(:,:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_2dr64
        module impure subroutine arrayprint_2dr32(x, fmt)
            real(real32), intent(in) :: x(:,:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_2dr32

        module impure subroutine arrayprint_1di64(x, fmt)
            integer(int64), intent(in) :: x(:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_1di64
        module impure subroutine arrayprint_1di32(x, fmt)
            integer(int32), intent(in) :: x(:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_1di32
        module impure subroutine arrayprint_1di16(x, fmt)
            integer(int16), intent(in) :: x(:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_1di16
        module impure subroutine arrayprint_1di8(x, fmt)
            integer(int8), intent(in) :: x(:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_1di8

        module impure subroutine arrayprint_2di64(x, fmt)
            integer(int64), intent(in) :: x(:,:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_2di64
        module impure subroutine arrayprint_2di32(x, fmt)
            integer(int32), intent(in) :: x(:,:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_2di32
        module impure subroutine arrayprint_2di16(x, fmt)
            integer(int16), intent(in) :: x(:,:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_2di16
        module impure subroutine arrayprint_2di8(x, fmt)
            integer(int8), intent(in) :: x(:,:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_2di8

        module impure subroutine arrayprint_1dchar(x, fmt)
            character(len=*), intent(in) :: x(:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_1dchar

        module impure subroutine arrayprint_2dchar(x, fmt)
            character(len=*), intent(in) :: x(:,:)
            character(len=*), intent(in) :: fmt
        end subroutine arrayprint_2dchar
    end interface

    !~~~~====== Procedures for writing data to text files ======~~~~!

    ! call csvwrite(x=, file_name='')
    ! rank(x) = 1,2 and kind(x) = r64,r32,i32
    interface csvwrite        ! Submodule text_io
        module impure subroutine csvwrite_1dr64(x, file_name)
            real(real64), intent(in) :: x(:)
            character(len=*), intent(in) :: file_name
        end subroutine csvwrite_1dr64
        module impure subroutine csvwrite_1dr32(x, file_name)
            real(real32), intent(in) :: x(:)
            character(len=*), intent(in) :: file_name
        end subroutine csvwrite_1dr32

        module impure subroutine csvwrite_2dr64(x, file_name)
            real(real64), intent(in) :: x(:,:)
            character(len=*), intent(in) :: file_name
        end subroutine csvwrite_2dr64
        module impure subroutine csvwrite_2dr32(x, file_name)
            real(real32), intent(in) :: x(:,:)
            character(len=*), intent(in) :: file_name
        end subroutine csvwrite_2dr32

        module impure subroutine csvwrite_1di32(x, file_name)
            integer(int32), intent(in) :: x(:)
            character(len=*), intent(in) :: file_name
        end subroutine csvwrite_1di32

        module impure subroutine csvwrite_2di32(x, file_name)
            integer(int32), intent(in) :: x(:,:)
            character(len=*), intent(in) :: file_name
        end subroutine csvwrite_2di32
    end interface

    ! call echo(string=, file_name='')
    ! type(string) = character(len=*)
    ! append affects only existing files - file will be replaced unless append=.true.
    interface echo            ! Submodule text_io
        module impure subroutine echo_string(string, file_name, append)
            character(len=*), intent(in) :: string
            character(len=*), intent(in) :: file_name
            logical, optional, intent(in) :: append
        end subroutine echo_string
    end interface

    !~~~~====== Procedures for reading data from text files ======~~~~!

    ! call csvread(file_name='', into=)
    ! rank(into) = 1,2 for kind(into) = r64,r32,i32
    interface csvread         ! Submodule text_io
        module impure subroutine csvread_1dr64(file_name, into)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, intent(out) :: into(:)
        end subroutine csvread_1dr64
        module impure subroutine csvread_1dr32(file_name, into)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, intent(out) :: into(:)
        end subroutine csvread_1dr32

        module impure subroutine csvread_2dr64(file_name, into)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, intent(out) :: into(:,:)
        end subroutine csvread_2dr64
        module impure subroutine csvread_2dr32(file_name, into)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, intent(out) :: into(:,:)
        end subroutine csvread_2dr32

        module impure subroutine csvread_1di32(file_name, into)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, intent(out) :: into(:)
        end subroutine csvread_1di32

        module impure subroutine csvread_2di32(file_name, into)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, intent(out) :: into(:,:)
        end subroutine csvread_2di32
    end interface

    !~~~~====== Procedures for writing data to binary files ======~~~~!

    ! call datwrite(x=, file_name='')
    ! rank(x) = 1,…,5 for kind(x) = r64,r32
    ! rank(x) = 1,…,3 for kind(x) = i64,i32,i16,i8
    interface datwrite        ! Submodule binary_io
        module impure subroutine datwrite_1dr64(x, file_name)
            real(real64), intent(in) :: x(:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_1dr64
        module impure subroutine datwrite_1dr32(x, file_name)
            real(real32), intent(in) :: x(:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_1dr32

        module impure subroutine datwrite_2dr64(x, file_name)
            real(real64), intent(in) :: x(:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_2dr64
        module impure subroutine datwrite_2dr32(x, file_name)
            real(real32), intent(in) :: x(:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_2dr32

        module impure subroutine datwrite_3dr64(x, file_name)
            real(real64), intent(in) :: x(:,:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_3dr64
        module impure subroutine datwrite_3dr32(x, file_name)
            real(real32), intent(in) :: x(:,:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_3dr32

        module impure subroutine datwrite_4dr64(x, file_name)
            real(real64), intent(in) :: x(:,:,:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_4dr64
        module impure subroutine datwrite_4dr32(x, file_name)
            real(real32), intent(in) :: x(:,:,:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_4dr32

        module impure subroutine datwrite_5dr64(x, file_name)
            real(real64), intent(in) :: x(:,:,:,:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_5dr64
        module impure subroutine datwrite_5dr32(x, file_name)
            real(real32), intent(in) :: x(:,:,:,:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_5dr32

        module impure subroutine datwrite_1di64(x, file_name)
            integer(int64), intent(in) :: x(:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_1di64
        module impure subroutine datwrite_1di32(x, file_name)
            integer(int32), intent(in) :: x(:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_1di32
        module impure subroutine datwrite_1di16(x, file_name)
            integer(int16), intent(in) :: x(:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_1di16
        module impure subroutine datwrite_1di8(x, file_name)
            integer(int8), intent(in) :: x(:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_1di8

        module impure subroutine datwrite_2di64(x, file_name)
            integer(int64), intent(in) :: x(:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_2di64
        module impure subroutine datwrite_2di32(x, file_name)
            integer(int32), intent(in) :: x(:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_2di32
        module impure subroutine datwrite_2di16(x, file_name)
            integer(int16), intent(in) :: x(:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_2di16
        module impure subroutine datwrite_2di8(x, file_name)
            integer(int8), intent(in) :: x(:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_2di8

        module impure subroutine datwrite_3di64(x, file_name)
            integer(int64), intent(in) :: x(:,:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_3di64
        module impure subroutine datwrite_3di32(x, file_name)
            integer(int32), intent(in) :: x(:,:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_3di32
        module impure subroutine datwrite_3di16(x, file_name)
            integer(int16), intent(in) :: x(:,:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_3di16
        module impure subroutine datwrite_3di8(x, file_name)
            integer(int8), intent(in) :: x(:,:,:)
            character(len=*), intent(in) :: file_name
        end subroutine datwrite_3di8
    end interface

    !~~~~====== Procedures for reading data from binary files ======~~~~!

    ! call datread(file_name='', into=, data_shape=)
    ! rank(into) = 1,…,5 for kind(into) = r64,r32
    ! rank(into) = 1,…,3 for kind(into) = i64,i32,i16,i8
    ! rank(data_shape) = 1, kind(data_shape) = i32, size(data_shape) = rank(into)
    interface datread         ! Submodule binary_io
        module impure subroutine datread_1dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, intent(out) :: into(:)
            integer(int32), intent(in) :: data_shape(1)
        end subroutine datread_1dr64
        module impure subroutine datread_1dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, intent(out) :: into(:)
            integer(int32), intent(in) :: data_shape(1)
        end subroutine datread_1dr32

        module impure subroutine datread_2dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, intent(out) :: into(:,:)
            integer(int32), intent(in) :: data_shape(2)
        end subroutine datread_2dr64
        module impure subroutine datread_2dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, intent(out) :: into(:,:)
            integer(int32), intent(in) :: data_shape(2)
        end subroutine datread_2dr32

        module impure subroutine datread_3dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, intent(out) :: into(:,:,:)
            integer(int32), intent(in) :: data_shape(3)
        end subroutine datread_3dr64
        module impure subroutine datread_3dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, intent(out) :: into(:,:,:)
            integer(int32), intent(in) :: data_shape(3)
        end subroutine datread_3dr32

        module impure subroutine datread_4dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, intent(out) :: into(:,:,:,:)
            integer(int32), intent(in) :: data_shape(4)
        end subroutine datread_4dr64
        module impure subroutine datread_4dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, intent(out) :: into(:,:,:,:)
            integer(int32), intent(in) :: data_shape(4)
        end subroutine datread_4dr32

        module impure subroutine datread_5dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, intent(out) :: into(:,:,:,:,:)
            integer(int32), intent(in) :: data_shape(5)
        end subroutine datread_5dr64
        module impure subroutine datread_5dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, intent(out) :: into(:,:,:,:,:)
            integer(int32), intent(in) :: data_shape(5)
        end subroutine datread_5dr32

        module impure subroutine datread_1di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, intent(out) :: into(:)
            integer(int32), intent(in) :: data_shape(1)
        end subroutine datread_1di64
        module impure subroutine datread_1di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, intent(out) :: into(:)
            integer(int32), intent(in) :: data_shape(1)
        end subroutine datread_1di32
        module impure subroutine datread_1di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, intent(out) :: into(:)
            integer(int32), intent(in) :: data_shape(1)
        end subroutine datread_1di16
        module impure subroutine datread_1di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, intent(out) :: into(:)
            integer(int32), intent(in) :: data_shape(1)
        end subroutine datread_1di8

        module impure subroutine datread_2di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, intent(out) :: into(:,:)
            integer(int32), intent(in) :: data_shape(2)
        end subroutine datread_2di64
        module impure subroutine datread_2di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, intent(out) :: into(:,:)
            integer(int32), intent(in) :: data_shape(2)
        end subroutine datread_2di32
        module impure subroutine datread_2di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, intent(out) :: into(:,:)
            integer(int32), intent(in) :: data_shape(2)
        end subroutine datread_2di16
        module impure subroutine datread_2di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, intent(out) :: into(:,:)
            integer(int32), intent(in) :: data_shape(2)
        end subroutine datread_2di8

        module impure subroutine datread_3di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, intent(out) :: into(:,:,:)
            integer(int32), intent(in) :: data_shape(3)
        end subroutine datread_3di64
        module impure subroutine datread_3di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, intent(out) :: into(:,:,:)
            integer(int32), intent(in) :: data_shape(3)
        end subroutine datread_3di32
        module impure subroutine datread_3di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, intent(out) :: into(:,:,:)
            integer(int32), intent(in) :: data_shape(3)
        end subroutine datread_3di16
        module impure subroutine datread_3di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, intent(out) :: into(:,:,:)
            integer(int32), intent(in) :: data_shape(3)
        end subroutine datread_3di8
    end interface

    contains
    !~~~~====== Simple functions for transforming boolean data into int/str ======~~~~!
    pure elemental integer(int8) function logtoint(x) result(y)
        logical, intent(in) :: x

        if ( x ) then
            y = int(1,int8)
        else
            y = int(0,int8)
        end if
    end function logtoint
    pure elemental character(len=1) function logtochar(x) result(y)
        logical, intent(in) :: x

        if ( x ) then
            y = 'T'
        else
            y = 'F'
        end if
    end function logtochar

end module io_mod

submodule (io_mod) debugging
    contains
    module procedure arrayprint_1dr64
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure arrayprint_1dr64
    module procedure arrayprint_1dr32
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure arrayprint_1dr32

    module procedure arrayprint_2dr64
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure arrayprint_2dr64
    module procedure arrayprint_2dr32
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure arrayprint_2dr32

    module procedure arrayprint_1di64
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure arrayprint_1di64
    module procedure arrayprint_1di32
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure arrayprint_1di32
    module procedure arrayprint_1di16
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure arrayprint_1di16
    module procedure arrayprint_1di8
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure arrayprint_1di8

    module procedure arrayprint_2di64
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure arrayprint_2di64
    module procedure arrayprint_2di32
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure arrayprint_2di32
    module procedure arrayprint_2di16
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure arrayprint_2di16
    module procedure arrayprint_2di8
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure arrayprint_2di8

    module procedure arrayprint_1dchar
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure arrayprint_1dchar

    module procedure arrayprint_2dchar
        integer i

        do i = lbound(x,dim=1), ubound(x,dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure arrayprint_2dchar
end submodule debugging

submodule (io_mod) text_io
    contains
    !~~~~====== Writing Procedures ======~~~~!
    module procedure csvwrite_1dr64
        logical exists
        integer file_unit, i
        character(len=16) number_string

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                write(unit=number_string, fmt='(f16.8)') x(i)
                write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                write(unit=number_string, fmt='(f16.8)') x(i)
                write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
            end do
        end if

        close(file_unit)
    end procedure csvwrite_1dr64
    module procedure csvwrite_1dr32
        logical exists
        integer file_unit, i
        character(len=16) number_string

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                write(unit=number_string, fmt='(f16.8)') x(i)
                write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                write(unit=number_string, fmt='(f16.8)') x(i)
                write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
            end do
        end if

        close(file_unit)
    end procedure csvwrite_1dr32

    module procedure csvwrite_2dr64
        logical exists
        integer file_unit, i, j
        character(len=16) number_string

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                do j = lbound(x,dim=2), ubound(x,dim=2)
                if ( j < ubound(x,dim=2) ) then
                    write(unit=number_string, fmt='(f16.8)') x(i,j)
                    write(unit=file_unit, fmt='(a,a)', advance='no') trim(adjustl(number_string)), ','
                else
                    write(unit=number_string, fmt='(f16.8)') x(i,j)
                    write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
                end if
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                do j = lbound(x,dim=2), ubound(x,dim=2)
                if ( j < ubound(x,dim=2) ) then
                    write(unit=number_string, fmt='(f16.8)') x(i,j)
                    write(unit=file_unit, fmt='(a,a)', advance='no') trim(adjustl(number_string)), ','
                else
                    write(unit=number_string, fmt='(f16.8)') x(i,j)
                    write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
                end if
                end do
            end do
        end if

        close(file_unit)
    end procedure csvwrite_2dr64
    module procedure csvwrite_2dr32
        logical exists
        integer file_unit, i, j
        character(len=16) number_string

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                do j = lbound(x,dim=2), ubound(x,dim=2)
                if ( j < ubound(x,dim=2) ) then
                    write(unit=number_string, fmt='(f16.8)') x(i,j)
                    write(unit=file_unit, fmt='(a,a)', advance='no') trim(adjustl(number_string)), ','
                else
                    write(unit=number_string, fmt='(f16.8)') x(i,j)
                    write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
                end if
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                do j = lbound(x,dim=2), ubound(x,dim=2)
                if ( j < ubound(x,dim=2) ) then
                    write(unit=number_string, fmt='(f16.8)') x(i,j)
                    write(unit=file_unit, fmt='(a,a)', advance='no') trim(adjustl(number_string)), ','
                else
                    write(unit=number_string, fmt='(f16.8)') x(i,j)
                    write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
                end if
                end do
            end do
        end if

        close(file_unit)
    end procedure csvwrite_2dr32

    module procedure csvwrite_1di32
        logical exists
        integer file_unit, i
        character(len=12) number_string

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                write(unit=number_string, fmt='(i12)') x(i)
                write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                write(unit=number_string, fmt='(i12)') x(i)
                write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
            end do
        end if

        close(file_unit)
    end procedure csvwrite_1di32

    module procedure csvwrite_2di32
        logical exists
        integer file_unit, i, j
        character(len=12) number_string

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                do j = lbound(x,dim=2), ubound(x,dim=2)
                if ( j < ubound(x,dim=2) ) then
                    write(unit=number_string, fmt='(i12)') x(i,j)
                    write(unit=file_unit, fmt='(a,a)', advance='no') trim(adjustl(number_string)), ','
                else
                    write(unit=number_string, fmt='(i12)') x(i,j)
                    write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
                end if
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', action='write', access='sequential', position='rewind' )
            do i = lbound(x,dim=1), ubound(x,dim=1)
                do j = lbound(x,dim=2), ubound(x,dim=2)
                if ( j < ubound(x,dim=2) ) then
                    write(unit=number_string, fmt='(i12)') x(i,j)
                    write(unit=file_unit, fmt='(a,a)', advance='no') trim(adjustl(number_string)), ','
                else
                    write(unit=number_string, fmt='(i12)') x(i,j)
                    write(unit=file_unit, fmt='(a)') trim(adjustl(number_string))
                end if
                end do
            end do
        end if

        close(file_unit)
    end procedure csvwrite_2di32

    module procedure echo_string
        logical exists
        integer file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', action='write', access='sequential', position='rewind' )
            write(unit=file_unit, fmt='(a)') string
        else
            if ( (.not. present(append)) .or. (.not. append) ) then
                open( newunit=file_unit, file=file_name, status='replace', form='formatted', action='write', access='sequential', position='rewind' )
                write(unit=file_unit, fmt='(a)') string
            else if ( append ) then
                open( newunit=file_unit, file=file_name, status='old', form='formatted', action='write', access='sequential', position='append' )
                write(unit=file_unit, fmt='(a)') string
            end if
        end if

        close(file_unit)
    end procedure echo_string

    !~~~~====== Reading Procedures ======~~~~!
    module procedure csvread_1dr64
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='formatted', action='read', access='sequential', position='rewind' )
            rows = number_of_rows_in_file(file_unit)
            allocate( into(rows) )
            do i = 1, rows
                read(unit=file_unit, fmt=*, iostat=status) into(i)
            end do
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure csvread_1dr64
    module procedure csvread_1dr32
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='formatted', action='read', access='sequential', position='rewind' )
            rows = number_of_rows_in_file(file_unit)
            allocate( into(rows) )
            do i = 1, rows
                read(unit=file_unit, fmt=*, iostat=status) into(i)
            end do
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure csvread_1dr32

    module procedure csvread_2dr64
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows, columns

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='formatted', action='read', access='sequential', position='rewind' )
            rows = number_of_rows_in_file(file_unit)
            columns = number_of_columns_in_file(file_unit)
            allocate( into(rows,columns) )
            do i = 1, rows
                read(unit=file_unit, fmt=*, iostat=status) into(i,:)
            end do
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure csvread_2dr64
    module procedure csvread_2dr32
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows, columns

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='formatted', action='read', access='sequential', position='rewind' )
            rows = number_of_rows_in_file(file_unit)
            columns = number_of_columns_in_file(file_unit)
            allocate( into(rows,columns) )
            do i = 1, rows
                read(unit=file_unit, fmt=*, iostat=status) into(i,:)
            end do
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure csvread_2dr32

    module procedure csvread_1di32
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='formatted', action='read', access='sequential', position='rewind' )
            rows = number_of_rows_in_file(file_unit)
            allocate( into(rows) )
            do i = 1, rows
                read(unit=file_unit, fmt=*, iostat=status) into(i)
            end do
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure csvread_1di32

    module procedure csvread_2di32
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows, columns

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='formatted', action='read', access='sequential', position='rewind' )
            rows = number_of_rows_in_file(file_unit)
            columns = number_of_columns_in_file(file_unit)
            allocate( into(rows,columns) )
            do i = 1, rows
                read(unit=file_unit, fmt=*, iostat=status) into(i,:)
            end do
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure csvread_2di32

    !~~~~====== Submodule Procedures ======~~~~!
    impure integer(int32) function number_of_rows_in_file(file_unit) result(n_rows)
        integer(int32), intent(in) :: file_unit   ! the file unit number (assumed to be open)

        character(len=1) line
        integer(int32) status

        rewind(file_unit)

        n_rows = 0
        do
            read(unit=file_unit, fmt='(a1)', iostat=status) line
            if ( is_iostat_end(status) ) exit
            n_rows = n_rows + 1
        end do

        rewind(file_unit)
    end function number_of_rows_in_file
    impure integer(int32) function number_of_columns_in_file(file_unit) result(n_columns)
        integer(int32), intent(in) :: file_unit   ! the file unit number (assumed to be open)

        character(len=4096) line
        integer(int32) status, i

        rewind(file_unit)
        read(unit=file_unit, fmt='(a)', iostat=status) line

        n_columns = 0
        do i = 1, len_trim(line)
            if ( line(i:i) /= ',' ) cycle
            n_columns = n_columns + 1
        end do
        n_columns = n_columns + 1

        rewind(file_unit)
    end function number_of_columns_in_file
end submodule text_io

submodule (io_mod) binary_io
    contains
    !~~~~====== Writing Procedures ======~~~~!
    module procedure datwrite_1dr64
        logical exists
        integer file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        end if

        close(file_unit)
    end procedure datwrite_1dr64
    module procedure datwrite_1dr32
        logical exists
        integer file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        end if
        
        close(file_unit)
    end procedure datwrite_1dr32

    module procedure datwrite_2dr64
        logical exists
        integer file_unit, i

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        end if

        close(file_unit)
    end procedure datwrite_2dr64
    module procedure datwrite_2dr32
        logical exists
        integer file_unit, i

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        end if

        close(file_unit)
    end procedure datwrite_2dr32

    module procedure datwrite_3dr64
        logical exists
        integer file_unit, i, j

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        end if

        close(file_unit)
    end procedure datwrite_3dr64
    module procedure datwrite_3dr32
        logical exists
        integer file_unit, i, j

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        end if

        close(file_unit)
    end procedure datwrite_3dr32

    module procedure datwrite_4dr64
        logical exists
        integer file_unit, i, j, k

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=4), ubound(x, dim=4)
                do j = lbound(x, dim=3), ubound(x, dim=3)
                    do k = lbound(x, dim=2), ubound(x, dim=2)
                        write(unit=file_unit) x(:,k,j,i)
                    end do
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=4), ubound(x, dim=4)
                do j = lbound(x, dim=3), ubound(x, dim=3)
                    do k = lbound(x, dim=2), ubound(x, dim=2)
                        write(unit=file_unit) x(:,k,j,i)
                    end do
                end do
            end do
        end if

        close(file_unit)
    end procedure datwrite_4dr64
    module procedure datwrite_4dr32
        logical exists
        integer file_unit, i, j, k

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=4), ubound(x, dim=4)
                do j = lbound(x, dim=3), ubound(x, dim=3)
                    do k = lbound(x, dim=2), ubound(x, dim=2)
                        write(unit=file_unit) x(:,k,j,i)
                    end do
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=4), ubound(x, dim=4)
                do j = lbound(x, dim=3), ubound(x, dim=3)
                    do k = lbound(x, dim=2), ubound(x, dim=2)
                        write(unit=file_unit) x(:,k,j,i)
                    end do
                end do
            end do
        end if

        close(file_unit)
    end procedure datwrite_4dr32

    module procedure datwrite_5dr64
        logical exists
        integer file_unit, i, j, k, l

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=5), ubound(x, dim=5)
                do j = lbound(x, dim=4), ubound(x, dim=4)
                    do k = lbound(x, dim=3), ubound(x, dim=3)
                        do l = lbound(x, dim=2), ubound(x, dim=2)
                            write(unit=file_unit) x(:,l,k,j,i)
                        end do
                    end do
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=5), ubound(x, dim=5)
                do j = lbound(x, dim=4), ubound(x, dim=4)
                    do k = lbound(x, dim=3), ubound(x, dim=3)
                        do l = lbound(x, dim=2), ubound(x, dim=2)
                            write(unit=file_unit) x(:,l,k,j,i)
                        end do
                    end do
                end do
            end do
        end if

        close(file_unit)
    end procedure datwrite_5dr64
    module procedure datwrite_5dr32
        logical exists
        integer file_unit, i, j, k, l

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=5), ubound(x, dim=5)
                do j = lbound(x, dim=4), ubound(x, dim=4)
                    do k = lbound(x, dim=3), ubound(x, dim=3)
                        do l = lbound(x, dim=2), ubound(x, dim=2)
                            write(unit=file_unit) x(:,l,k,j,i)
                        end do
                    end do
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=5), ubound(x, dim=5)
                do j = lbound(x, dim=4), ubound(x, dim=4)
                    do k = lbound(x, dim=3), ubound(x, dim=3)
                        do l = lbound(x, dim=2), ubound(x, dim=2)
                            write(unit=file_unit) x(:,l,k,j,i)
                        end do
                    end do
                end do
            end do
        end if

        close(file_unit)
    end procedure datwrite_5dr32

    module procedure datwrite_1di64
        logical exists
        integer file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        end if

        close(file_unit)
    end procedure datwrite_1di64
    module procedure datwrite_1di32
        logical exists
        integer file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        end if

        close(file_unit)
    end procedure datwrite_1di32
    module procedure datwrite_1di16
        logical exists
        integer file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        end if

        close(file_unit)
    end procedure datwrite_1di16
    module procedure datwrite_1di8
        logical exists
        integer file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            write(unit=file_unit) x
        end if

        close(file_unit)
    end procedure datwrite_1di8

    module procedure datwrite_2di64
        logical exists
        integer file_unit, i

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        end if

        close(file_unit)
    end procedure datwrite_2di64
    module procedure datwrite_2di32
        logical exists
        integer file_unit, i

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        end if

        close(file_unit)
    end procedure datwrite_2di32
    module procedure datwrite_2di16
        logical exists
        integer file_unit, i

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        end if

        close(file_unit)
    end procedure datwrite_2di16
    module procedure datwrite_2di8
        logical exists
        integer file_unit, i

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,i)
            end do
        end if

        close(file_unit)
    end procedure datwrite_2di8

    module procedure datwrite_3di64
        logical exists
        integer file_unit, i, j

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        end if

        close(file_unit)
    end procedure datwrite_3di64
    module procedure datwrite_3di32
        logical exists
        integer file_unit, i, j

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        end if

        close(file_unit)
    end procedure datwrite_3di32
    module procedure datwrite_3di16
        logical exists
        integer file_unit, i, j

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        end if

        close(file_unit)
    end procedure datwrite_3di16
    module procedure datwrite_3di8
        logical exists
        integer file_unit, i, j

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', action='write', access='stream' )
            do i = lbound(x, dim=3), ubound(x, dim=3)
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,j,i)
                end do
            end do
        end if

        close(file_unit)
    end procedure datwrite_3di8

    !~~~~====== Reading Procedures ======~~~~!
    module procedure datread_1dr64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_1dr64
    module procedure datread_1dr32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_1dr32

    module procedure datread_2dr64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_2dr64
    module procedure datread_2dr32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_2dr32

    module procedure datread_3dr64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_3dr64
    module procedure datread_3dr32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_3dr32

    module procedure datread_4dr64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_4dr64
    module procedure datread_4dr32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_4dr32

    module procedure datread_5dr64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_5dr64
    module procedure datread_5dr32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_5dr32

    module procedure datread_1di64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_1di64
    module procedure datread_1di32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_1di32
    module procedure datread_1di16
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_1di16
    module procedure datread_1di8
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_1di8

    module procedure datread_2di64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_2di64
    module procedure datread_2di32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_2di32
    module procedure datread_2di16
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_2di16
    module procedure datread_2di8
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_2di8

    module procedure datread_3di64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_3di64
    module procedure datread_3di32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_3di32
    module procedure datread_3di16
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_3di16
    module procedure datread_3di8
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', action='read', access='stream' )
            allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
            read(unit=file_unit, iostat=status) into
        else
            write(*,*)
            error stop 'Error reading file "'//file_name//'". No such file exists.'
            write(*,*)
        end if

        close(file_unit)
    end procedure datread_3di8
end submodule binary_io