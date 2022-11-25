!!---------------------------------------------------------------------------------------------------------------------
!!  This module file contains common i/o operations for arrays of real and integer type. Common operations include
!!  printing arrays to stdout with a specified format, reading/writing arrays from/to csv files and binary files.
!!---------------------------------------------------------------------------------------------------------------------
module io_mod
    use, intrinsic :: iso_fortran_env, only: real64, real32, int64, int32, int16, int8, input_unit, output_unit
    implicit none (type,external)
    private

    !! Public APIs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    public :: aprint, to_text, from_text, to_binary, from_binary, to_str                                   !! Array I/O
    public :: ext_of, echo, str                                                                           !! String I/O
    public :: nl, text_ext, binary_ext                                                                     !! Constants

    !! Definitions and Interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    character(len=1), parameter :: nl = new_line('')                                              !! New line character
    character(len=*), dimension(*), parameter :: text_ext = ['csv', 'txt', 'ods', 'odf', 'odm', 'odt', 'xls', 'xlsx', &
                                                             'doc', 'docx', 'md', 'log', 'rtf', 'org', 'embed']
    character(len=*), dimension(*), parameter :: binary_ext = ['dat', 'bin']

    !! call aprint(x=, fmt=)
    !! rank(x) = 1,2 and kind(x) = r64,r32,i64,i32,i16,i8, & character(len=*)
    interface aprint                                                                             !! Submodule debugging
        module impure subroutine aprint_1dr64(x, fmt)
            real(real64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_1dr64
        module impure subroutine aprint_1dr32(x, fmt)
            real(real32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_1dr32

        module impure subroutine aprint_2dr64(x, fmt)
            real(real64), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_2dr64
        module impure subroutine aprint_2dr32(x, fmt)
            real(real32), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_2dr32

        module impure subroutine aprint_1di64(x, fmt)
            integer(int64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_1di64
        module impure subroutine aprint_1di32(x, fmt)
            integer(int32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_1di32
        module impure subroutine aprint_1di16(x, fmt)
            integer(int16), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_1di16
        module impure subroutine aprint_1di8(x, fmt)
            integer(int8), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_1di8

        module impure subroutine aprint_2di64(x, fmt)
            integer(int64), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_2di64
        module impure subroutine aprint_2di32(x, fmt)
            integer(int32), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_2di32
        module impure subroutine aprint_2di16(x, fmt)
            integer(int16), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_2di16
        module impure subroutine aprint_2di8(x, fmt)
            integer(int8), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_2di8

        module impure subroutine aprint_1dchar(x, fmt)
            character(len=*), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_1dchar

        module impure subroutine aprint_2dchar(x, fmt)
            character(len=*), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: fmt
        end subroutine aprint_2dchar
    end interface

    !! call to_text(x=, file_name='')
    !! rank(x) = 1,2 and kind(x) = r64,r32,i32
    interface to_text                                                                              !! Submodule text_io
        module impure subroutine to_text_1dr64(x, file_name, header)
            real(real64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), intent(in), optional :: header
        end subroutine to_text_1dr64
        module impure subroutine to_text_1dr32(x, file_name, header)
            real(real32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), intent(in), optional :: header
        end subroutine to_text_1dr32

        module impure subroutine to_text_2dr64(x, file_name, header)
            real(real64), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), contiguous, dimension(:), intent(in), optional :: header
        end subroutine to_text_2dr64
        module impure subroutine to_text_2dr32(x, file_name, header)
            real(real32), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), contiguous, dimension(:), intent(in), optional :: header
        end subroutine to_text_2dr32

        module impure subroutine to_text_1di64(x, file_name, header)
            integer(int64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), intent(in), optional :: header
        end subroutine to_text_1di64
        module impure subroutine to_text_1di32(x, file_name, header)
            integer(int32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), intent(in), optional :: header
        end subroutine to_text_1di32
        module impure subroutine to_text_1di16(x, file_name, header)
            integer(int16), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), intent(in), optional :: header
        end subroutine to_text_1di16
        module impure subroutine to_text_1di8(x, file_name, header)
            integer(int8), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), intent(in), optional :: header
        end subroutine to_text_1di8

        module impure subroutine to_text_2di64(x, file_name, header)
            integer(int64), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), contiguous, dimension(:), intent(in), optional :: header
        end subroutine to_text_2di64
        module impure subroutine to_text_2di32(x, file_name, header)
            integer(int32), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), contiguous, dimension(:), intent(in), optional :: header
        end subroutine to_text_2di32
        module impure subroutine to_text_2di16(x, file_name, header)
            integer(int16), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), contiguous, dimension(:), intent(in), optional :: header
        end subroutine to_text_2di16
        module impure subroutine to_text_2di8(x, file_name, header)
            integer(int8), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), contiguous, dimension(:), intent(in), optional :: header
        end subroutine to_text_2di8
    end interface

    !! call echo(string=, file_name='')
    !! type(string) = character(len=*)
    !! append affects only existing files - file will be replaced unless append=.true.
    interface echo                                                                                 !! Submodule text_io
        module impure subroutine echo_string(string, file_name, append)
            character(len=*), intent(in) :: string
            character(len=*), intent(in) :: file_name
            logical, optional, intent(in) :: append
        end subroutine echo_string
    end interface

    !! stringvar = str(number=, d=) for kind(number)=r64,r32 and kind(d)=i32 where d is the number of decimals to use
    !! stringvar = str(number=) where kind(number)=i64,i32,i16,i8
    interface str                                                                                  !! Submodule text_io
        module pure function str_r64(number, d) result(number_str)
            real(real64), intent(in) :: number
            integer, intent(in) :: d
            character(len=:), allocatable :: number_str
        end function str_r64
        module pure function str_r32(number, d) result(number_str)
            real(real32), intent(in) :: number
            integer, intent(in) :: d
            character(len=:), allocatable :: number_str
        end function str_r32

        module pure function str_i64(number) result(number_str)
            integer(int64), intent(in) :: number
            character(len=:), allocatable :: number_str
        end function str_i64
        module pure function str_i32(number) result(number_str)
            integer(int32), intent(in) :: number
            character(len=:), allocatable :: number_str
        end function str_i32
        module pure function str_i16(number) result(number_str)
            integer(int16), intent(in) :: number
            character(len=:), allocatable :: number_str
        end function str_i16
        module pure function str_i8(number) result(number_str)
            integer(int8), intent(in) :: number
            character(len=:), allocatable :: number_str
        end function str_i8
    end interface

    !! call from_text(file_name='', into=)
    !! rank(into) = 1,2 for kind(into) = r64,r32,i32
    interface from_text                                                                            !! Submodule text_io
        module impure subroutine from_text_1dr64(file_name, into, header)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_1dr64
        module impure subroutine from_text_1dr32(file_name, into, header)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_1dr32

        module impure subroutine from_text_2dr64(file_name, into, header)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_2dr64
        module impure subroutine from_text_2dr32(file_name, into, header)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_2dr32

        module impure subroutine from_text_1di64(file_name, into, header)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_1di64
        module impure subroutine from_text_1di32(file_name, into, header)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_1di32
        module impure subroutine from_text_1di16(file_name, into, header)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_1di16
        module impure subroutine from_text_1di8(file_name, into, header)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_1di8

        module impure subroutine from_text_2di64(file_name, into, header)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_2di64
        module impure subroutine from_text_2di32(file_name, into, header)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_2di32
        module impure subroutine from_text_2di16(file_name, into, header)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_2di16
        module impure subroutine from_text_2di8(file_name, into, header)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
        end subroutine from_text_2di8
    end interface

    !! call to_binary(x=, file_name='')
    !! rank(x) = 1,…,5 for kind(x) = r64,r32
    !! rank(x) = 1,…,3 for kind(x) = i64,i32,i16,i8
    interface to_binary                                                                          !! Submodule binary_io
        module impure subroutine to_binary_1dr64(x, file_name)
            real(real64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_1dr64
        module impure subroutine to_binary_1dr32(x, file_name)
            real(real32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_1dr32

        module impure subroutine to_binary_2dr64(x, file_name)
            real(real64), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_2dr64
        module impure subroutine to_binary_2dr32(x, file_name)
            real(real32), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_2dr32

        module impure subroutine to_binary_3dr64(x, file_name)
            real(real64), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_3dr64
        module impure subroutine to_binary_3dr32(x, file_name)
            real(real32), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_3dr32

        module impure subroutine to_binary_4dr64(x, file_name)
            real(real64), dimension(:,:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_4dr64
        module impure subroutine to_binary_4dr32(x, file_name)
            real(real32), dimension(:,:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_4dr32

        module impure subroutine to_binary_5dr64(x, file_name)
            real(real64), dimension(:,:,:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_5dr64
        module impure subroutine to_binary_5dr32(x, file_name)
            real(real32), dimension(:,:,:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_5dr32

        module impure subroutine to_binary_1di64(x, file_name)
            integer(int64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_1di64
        module impure subroutine to_binary_1di32(x, file_name)
            integer(int32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_1di32
        module impure subroutine to_binary_1di16(x, file_name)
            integer(int16), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_1di16
        module impure subroutine to_binary_1di8(x, file_name)
            integer(int8), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_1di8

        module impure subroutine to_binary_2di64(x, file_name)
            integer(int64), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_2di64
        module impure subroutine to_binary_2di32(x, file_name)
            integer(int32), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_2di32
        module impure subroutine to_binary_2di16(x, file_name)
            integer(int16), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_2di16
        module impure subroutine to_binary_2di8(x, file_name)
            integer(int8), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_2di8

        module impure subroutine to_binary_3di64(x, file_name)
            integer(int64), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_3di64
        module impure subroutine to_binary_3di32(x, file_name)
            integer(int32), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_3di32
        module impure subroutine to_binary_3di16(x, file_name)
            integer(int16), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_3di16
        module impure subroutine to_binary_3di8(x, file_name)
            integer(int8), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_binary_3di8
    end interface

    !! call from_binary(file_name='', into=, data_shape=)
    !! rank(into) = 1,…,5 for kind(into) = r64,r32
    !! rank(into) = 1,…,3 for kind(into) = i64,i32,i16,i8
    !! rank(data_shape) = 1, kind(data_shape) = i32, size(data_shape) = rank(into)
    interface from_binary                                                                        !! Submodule binary_io
        module impure subroutine from_binary_1dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(1)
        end subroutine from_binary_1dr64
        module impure subroutine from_binary_1dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(1)
        end subroutine from_binary_1dr32

        module impure subroutine from_binary_2dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(2)
        end subroutine from_binary_2dr64
        module impure subroutine from_binary_2dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(2)
        end subroutine from_binary_2dr32

        module impure subroutine from_binary_3dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(3)
        end subroutine from_binary_3dr64
        module impure subroutine from_binary_3dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(3)
        end subroutine from_binary_3dr32

        module impure subroutine from_binary_4dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:,:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(4)
        end subroutine from_binary_4dr64
        module impure subroutine from_binary_4dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:,:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(4)
        end subroutine from_binary_4dr32

        module impure subroutine from_binary_5dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:,:,:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(5)
        end subroutine from_binary_5dr64
        module impure subroutine from_binary_5dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:,:,:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(5)
        end subroutine from_binary_5dr32

        module impure subroutine from_binary_1di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(1)
        end subroutine from_binary_1di64
        module impure subroutine from_binary_1di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(1)
        end subroutine from_binary_1di32
        module impure subroutine from_binary_1di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(1)
        end subroutine from_binary_1di16
        module impure subroutine from_binary_1di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(1)
        end subroutine from_binary_1di8

        module impure subroutine from_binary_2di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(2)
        end subroutine from_binary_2di64
        module impure subroutine from_binary_2di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(2)
        end subroutine from_binary_2di32
        module impure subroutine from_binary_2di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(2)
        end subroutine from_binary_2di16
        module impure subroutine from_binary_2di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(2)
        end subroutine from_binary_2di8

        module impure subroutine from_binary_3di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:,:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(3)
        end subroutine from_binary_3di64
        module impure subroutine from_binary_3di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:,:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(3)
        end subroutine from_binary_3di32
        module impure subroutine from_binary_3di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:,:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(3)
        end subroutine from_binary_3di16
        module impure subroutine from_binary_3di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:,:,:), intent(out) :: into
            integer(int32), intent(in) :: data_shape(3)
        end subroutine from_binary_3di8
    end interface

    contains
    pure function ext_of(file_name) result(ext)
        character(len=*), intent(in) :: file_name
        character(len=:), allocatable :: ext

        integer :: i, l

        l = len_trim(file_name)

        do i = l, 1, -1
            if ( file_name(i:i) == '.' ) exit
        end do

        ext = trim(adjustl(file_name(i+1:l)))
    end function ext_of

    pure function to_str(x, delim) result(x_str)
        character(len=*), dimension(:), intent(in) :: x
        character(len=*), intent(in) :: delim
        character(len=:), allocatable :: x_str

        integer :: i

        x_str = ''
        do i = 1, size(x)
            x_str = x_str//trim(adjustl(x(i)))//delim
        end do
    end function to_str

end module io_mod

submodule (io_mod) debugging
    contains
    module procedure aprint_1dr64
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure aprint_1dr64
    module procedure aprint_1dr32
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure aprint_1dr32

    module procedure aprint_2dr64
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure aprint_2dr64
    module procedure aprint_2dr32
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure aprint_2dr32

    module procedure aprint_1di64
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure aprint_1di64
    module procedure aprint_1di32
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure aprint_1di32
    module procedure aprint_1di16
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure aprint_1di16
    module procedure aprint_1di8
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure aprint_1di8

    module procedure aprint_2di64
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure aprint_2di64
    module procedure aprint_2di32
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure aprint_2di32
    module procedure aprint_2di16
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure aprint_2di16
    module procedure aprint_2di8
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure aprint_2di8

    module procedure aprint_1dchar
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i)
        end do
        write(*,*)
    end procedure aprint_1dchar

    module procedure aprint_2dchar
        integer i

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(*, fmt='(a)', advance='no') '    '
            write(unit=*, fmt=fmt) x(i,:)
        end do
        write(*,*)
    end procedure aprint_2dchar
end submodule debugging

submodule (io_mod) text_io
    contains
    !! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure to_text_1dr64
        logical exists
        integer file_unit, i

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            write(unit=file_unit, fmt='(a)') header
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') str(x(i), d=15)
        end do

        close(file_unit)
    end procedure to_text_1dr64
    module procedure to_text_1dr32
        logical exists
        integer file_unit, i

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            write(unit=file_unit, fmt='(a)') header
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') str(x(i), d=7)
        end do

        close(file_unit)
    end procedure to_text_1dr32

    module procedure to_text_2dr64
        logical exists
        integer file_unit, i, j

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            if ( size(header) == 1 ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(1)))//' '//str(j)//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))//' '//str(j)
                    end if
                end do
            else if ( size(header) == size(x, dim=2) ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(j)))//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(j)))
                    end if
                end do
            else
                write(*,*) nl//'Header length mismatch... omitting header for file '//file_name//nl
            end if
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                if ( j < ubound(x, dim=2) ) then
                    write(unit=file_unit, fmt='(a)', advance='no') str(x(i,j), d=15)//','
                else
                    write(unit=file_unit, fmt='(a)') str(x(i,j), d=15)
                end if
            end do
        end do

        close(file_unit)
    end procedure to_text_2dr64
    module procedure to_text_2dr32
        logical exists
        integer file_unit, i, j

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            if ( size(header) == 1 ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(1)))//' '//str(j)//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))//' '//str(j)
                    end if
                end do
            else if ( size(header) == size(x, dim=2) ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(j)))//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(j)))
                    end if
                end do
            else
                write(*,*) nl//'Header length mismatch... omitting header for file '//file_name//nl
            end if
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                if ( j < ubound(x, dim=2) ) then
                    write(unit=file_unit, fmt='(a)', advance='no') str(x(i,j), d=7)//','
                else
                    write(unit=file_unit, fmt='(a)') str(x(i,j), d=7)
                end if
            end do
        end do

        close(file_unit)
    end procedure to_text_2dr32

    module procedure to_text_1di64
        logical exists
        integer file_unit, i

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            write(unit=file_unit, fmt='(a)') header
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') str(x(i))
        end do

        close(file_unit)
    end procedure to_text_1di64
    module procedure to_text_1di32
        logical exists
        integer file_unit, i

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            write(unit=file_unit, fmt='(a)') header
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') str(x(i))
        end do

        close(file_unit)
    end procedure to_text_1di32
    module procedure to_text_1di16
        logical exists
        integer file_unit, i

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            write(unit=file_unit, fmt='(a)') header
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') str(x(i))
        end do

        close(file_unit)
    end procedure to_text_1di16
    module procedure to_text_1di8
        logical exists
        integer file_unit, i

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            write(unit=file_unit, fmt='(a)') header
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') str(x(i))
        end do

        close(file_unit)
    end procedure to_text_1di8

    module procedure to_text_2di64
        logical exists
        integer file_unit, i, j

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            if ( size(header) == 1 ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(1)))//' '//str(j)//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))//' '//str(j)
                    end if
                end do
            else if ( size(header) == size(x, dim=2) ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(j)))//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(j)))
                    end if
                end do
            else
                write(*,*) nl//'Header length mismatch... omitting header for file '//file_name//nl
            end if
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                if ( j < ubound(x, dim=2) ) then
                    write(unit=file_unit, fmt='(a)', advance='no') str(x(i,j))//','
                else
                    write(unit=file_unit, fmt='(a)') str(x(i,j))
                end if
            end do
        end do

        close(file_unit)
    end procedure to_text_2di64
    module procedure to_text_2di32
        logical exists
        integer file_unit, i, j

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            if ( size(header) == 1 ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(1)))//' '//str(j)//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))//' '//str(j)
                    end if
                end do
            else if ( size(header) == size(x, dim=2) ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(j)))//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(j)))
                    end if
                end do
            else
                write(*,*) nl//'Header length mismatch... omitting header for file '//file_name//nl
            end if
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                if ( j < ubound(x, dim=2) ) then
                    write(unit=file_unit, fmt='(a)', advance='no') str(x(i,j))//','
                else
                    write(unit=file_unit, fmt='(a)') str(x(i,j))
                end if
            end do
        end do

        close(file_unit)
    end procedure to_text_2di32
    module procedure to_text_2di16
        logical exists
        integer file_unit, i, j

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            if ( size(header) == 1 ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(1)))//' '//str(j)//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))//' '//str(j)
                    end if
                end do
            else if ( size(header) == size(x, dim=2) ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(j)))//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(j)))
                    end if
                end do
            else
                write(*,*) nl//'Header length mismatch... omitting header for file '//file_name//nl
            end if
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                if ( j < ubound(x, dim=2) ) then
                    write(unit=file_unit, fmt='(a)', advance='no') str(x(i,j))//','
                else
                    write(unit=file_unit, fmt='(a)') str(x(i,j))
                end if
            end do
        end do

        close(file_unit)
    end procedure to_text_2di16
    module procedure to_text_2di8
        logical exists
        integer file_unit, i, j

        if ( .not. any(text_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_text.'// &
                       nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( present(header) ) then
            if ( size(header) == 1 ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(1)))//' '//str(j)//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))//' '//str(j)
                    end if
                end do
            else if ( size(header) == size(x, dim=2) ) then
                do j = lbound(x, dim=2), ubound(x, dim=2)
                    if ( j < ubound(x, dim=2) ) then
                        write(unit=file_unit, fmt='(a)', advance='no') trim(adjustl(header(j)))//','
                    else
                        write(unit=file_unit, fmt='(a)') trim(adjustl(header(j)))
                    end if
                end do
            else
                write(*,*) nl//'Header length mismatch... omitting header for file '//file_name//nl
            end if
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                if ( j < ubound(x, dim=2) ) then
                    write(unit=file_unit, fmt='(a)', advance='no') str(x(i,j))//','
                else
                    write(unit=file_unit, fmt='(a)') str(x(i,j))
                end if
            end do
        end do

        close(file_unit)
    end procedure to_text_2di8

    module procedure echo_string
        logical exists
        integer file_unit

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            if ( (.not. present(append)) .or. (.not. append) ) then
                open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                      action='write', access='sequential', position='rewind' )
            else if ( append ) then
                open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                      action='write', access='sequential', position='append' )
            end if
        end if

        write(unit=file_unit, fmt='(a)') string

        close(file_unit)
    end procedure echo_string

    module procedure str_r64
        real(real64) :: comparison_number
        integer :: i, max_decimals, decimals
        character(len=11) :: decimals_str
        character(len=32) :: str_tmp

        if ( (abs(number) < 1e15_real64) .and. (abs(number) > 1e-3_real64) ) then
            comparison_number = 1.0_real64

            find_max_decimals: do i = 15, 0, -1
                if ( abs(number) < comparison_number ) then
                    max_decimals = i
                    exit find_max_decimals
                end if
                comparison_number = 10.0_real64*comparison_number
            end do find_max_decimals

            if ( d < 0 ) then
                decimals = 0
            else if ( d > max_decimals ) then
                decimals = max_decimals
            else
                decimals = d
            end if

            write(unit=decimals_str, fmt='(i11)') decimals
            write(unit=str_tmp, fmt='(f32.'//trim(adjustl(decimals_str))//')') number
            number_str = trim(adjustl(str_tmp))
        else
            if ( d < 0 ) then
                decimals = 0
            else if ( d > 15 ) then
                decimals = 15
            else
                decimals = d
            end if

            write(unit=decimals_str, fmt='(i11)') decimals
            write(unit=str_tmp, fmt='(es32.'//trim(adjustl(decimals_str))//')') number
            number_str = trim(adjustl(str_tmp))
        end if
    end procedure str_r64
    module procedure str_r32
        real(real32) :: comparison_number
        integer :: i, max_decimals, decimals
        character(len=7) :: decimals_str
        character(len=32) :: str_tmp

        if ( (abs(number) < 1e7_real32) .and. (abs(number) > 1e-3_real32) ) then
            comparison_number = 1.0_real32

            find_max_decimals: do i = 7, 0, -1
                if ( abs(number) < comparison_number ) then
                    max_decimals = i
                    exit find_max_decimals
                end if
                comparison_number = 10.0_real32*comparison_number
            end do find_max_decimals

            if ( d < 0 ) then
                decimals = 0
            else if ( d > max_decimals ) then
                decimals = max_decimals
            else
                decimals = d
            end if

            write(unit=decimals_str, fmt='(i7)') decimals
            write(unit=str_tmp, fmt='(f32.'//trim(adjustl(decimals_str))//')') number
            number_str = trim(adjustl(str_tmp))
        else
            if ( d < 0 ) then
                decimals = 0
            else if ( d > 7 ) then
                decimals = 7
            else
                decimals = d
            end if

            write(unit=decimals_str, fmt='(i7)') decimals
            write(unit=str_tmp, fmt='(es32.'//trim(adjustl(decimals_str))//')') number
            number_str = trim(adjustl(str_tmp))
        end if
    end procedure str_r32

    module procedure str_i64
        character(len=21) :: str_tmp

        write(unit=str_tmp, fmt='(i21)') number
        number_str = trim(adjustl(str_tmp))
    end procedure str_i64
    module procedure str_i32
        character(len=11) :: str_tmp

        write(unit=str_tmp, fmt='(i11)') number
        number_str = trim(adjustl(str_tmp))
    end procedure str_i32
    module procedure str_i16
        character(len=6) :: str_tmp

        write(unit=str_tmp, fmt='(i6)') number
        number_str = trim(adjustl(str_tmp))
    end procedure str_i16
    module procedure str_i8
        character(len=4) :: str_tmp

        write(unit=str_tmp, fmt='(i4)') number
        number_str = trim(adjustl(str_tmp))
    end procedure str_i8

    !! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure from_text_1dr64
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1) )
            read(unit=file_unit, fmt=*, iostat=status) into(1)
        else
            allocate( into(rows) )
        end if

        do i = 1, size(into)
            read(unit=file_unit, fmt=*, iostat=status) into(i)
        end do

        close(file_unit)
    end procedure from_text_1dr64
    module procedure from_text_1dr32
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1) )
            read(unit=file_unit, fmt=*, iostat=status) into(1)
        else
            allocate( into(rows) )
        end if

        do i = 1, size(into)
            read(unit=file_unit, fmt=*, iostat=status) into(i)
        end do

        close(file_unit)
    end procedure from_text_1dr32

    module procedure from_text_2dr64
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows, columns

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)
        columns = number_of_columns_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1,columns) )
            read(unit=file_unit, fmt=*, iostat=status) into(1,:)
        else
            allocate( into(rows,columns) )
        end if

        do i = 1, size(into, dim=1)
            read(unit=file_unit, fmt=*, iostat=status) into(i,:)
        end do

        close(file_unit)
    end procedure from_text_2dr64
    module procedure from_text_2dr32
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows, columns

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)
        columns = number_of_columns_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1,columns) )
            read(unit=file_unit, fmt=*, iostat=status) into(1,:)
        else
            allocate( into(rows,columns) )
        end if

        do i = 1, size(into, dim=1)
            read(unit=file_unit, fmt=*, iostat=status) into(i,:)
        end do

        close(file_unit)
    end procedure from_text_2dr32

    module procedure from_text_1di64
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1) )
            read(unit=file_unit, fmt=*, iostat=status) into(1)
        else
            allocate( into(rows) )
        end if

        do i = 1, size(into)
            read(unit=file_unit, fmt=*, iostat=status) into(i)
        end do

        close(file_unit)
    end procedure from_text_1di64
    module procedure from_text_1di32
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1) )
            read(unit=file_unit, fmt=*, iostat=status) into(1)
        else
            allocate( into(rows) )
        end if

        do i = 1, size(into)
            read(unit=file_unit, fmt=*, iostat=status) into(i)
        end do

        close(file_unit)
    end procedure from_text_1di32
    module procedure from_text_1di16
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1) )
            read(unit=file_unit, fmt=*, iostat=status) into(1)
        else
            allocate( into(rows) )
        end if

        do i = 1, size(into)
            read(unit=file_unit, fmt=*, iostat=status) into(i)
        end do

        close(file_unit)
    end procedure from_text_1di16
    module procedure from_text_1di8
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1) )
            read(unit=file_unit, fmt=*, iostat=status) into(1)
        else
            allocate( into(rows) )
        end if

        do i = 1, size(into)
            read(unit=file_unit, fmt=*, iostat=status) into(i)
        end do

        close(file_unit)
    end procedure from_text_1di8

    module procedure from_text_2di64
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows, columns

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)
        columns = number_of_columns_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1,columns) )
            read(unit=file_unit, fmt=*, iostat=status) into(1,:)
        else
            allocate( into(rows,columns) )
        end if

        do i = 1, size(into, dim=1)
            read(unit=file_unit, fmt=*, iostat=status) into(i,:)
        end do

        close(file_unit)
    end procedure from_text_2di64
    module procedure from_text_2di32
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows, columns

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)
        columns = number_of_columns_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1,columns) )
            read(unit=file_unit, fmt=*, iostat=status) into(1,:)
        else
            allocate( into(rows,columns) )
        end if

        do i = 1, size(into, dim=1)
            read(unit=file_unit, fmt=*, iostat=status) into(i,:)
        end do

        close(file_unit)
    end procedure from_text_2di32
    module procedure from_text_2di16
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows, columns

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)
        columns = number_of_columns_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1,columns) )
            read(unit=file_unit, fmt=*, iostat=status) into(1,:)
        else
            allocate( into(rows,columns) )
        end if

        do i = 1, size(into, dim=1)
            read(unit=file_unit, fmt=*, iostat=status) into(i,:)
        end do

        close(file_unit)
    end procedure from_text_2di16
    module procedure from_text_2di8
        logical exists
        integer(int32) file_unit, status, i
        integer(int32) rows, columns

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(text_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_text.'// &
                           nl//'Try one of the following: '//to_str(text_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='formatted', &
                  action='read', access='sequential', position='rewind' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        rows = number_of_rows_in_file(file_unit)
        columns = number_of_columns_in_file(file_unit)

        if ( header ) then
            allocate( into(rows-1,columns) )
            read(unit=file_unit, fmt=*, iostat=status) into(1,:)
        else
            allocate( into(rows,columns) )
        end if

        do i = 1, size(into, dim=1)
            read(unit=file_unit, fmt=*, iostat=status) into(i,:)
        end do

        close(file_unit)
    end procedure from_text_2di8

    !! Internal Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    !! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure to_binary_1dr64
        logical exists
        integer file_unit

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1dr64
    module procedure to_binary_1dr32
        logical exists
        integer file_unit

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        write(unit=file_unit) x
        
        close(file_unit)
    end procedure to_binary_1dr32

    module procedure to_binary_2dr64
        logical exists
        integer file_unit, i

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=2), ubound(x, dim=2)
            write(unit=file_unit) x(:,i)
        end do

        close(file_unit)
    end procedure to_binary_2dr64
    module procedure to_binary_2dr32
        logical exists
        integer file_unit, i

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=2), ubound(x, dim=2)
            write(unit=file_unit) x(:,i)
        end do

        close(file_unit)
    end procedure to_binary_2dr32

    module procedure to_binary_3dr64
        logical exists
        integer file_unit, i, j

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=3), ubound(x, dim=3)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,j,i)
            end do
        end do

        close(file_unit)
    end procedure to_binary_3dr64
    module procedure to_binary_3dr32
        logical exists
        integer file_unit, i, j

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=3), ubound(x, dim=3)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,j,i)
            end do
        end do

        close(file_unit)
    end procedure to_binary_3dr32

    module procedure to_binary_4dr64
        logical exists
        integer file_unit, i, j, k

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=4), ubound(x, dim=4)
            do j = lbound(x, dim=3), ubound(x, dim=3)
                do k = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,k,j,i)
                end do
            end do
        end do

        close(file_unit)
    end procedure to_binary_4dr64
    module procedure to_binary_4dr32
        logical exists
        integer file_unit, i, j, k

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=4), ubound(x, dim=4)
            do j = lbound(x, dim=3), ubound(x, dim=3)
                do k = lbound(x, dim=2), ubound(x, dim=2)
                    write(unit=file_unit) x(:,k,j,i)
                end do
            end do
        end do

        close(file_unit)
    end procedure to_binary_4dr32

    module procedure to_binary_5dr64
        logical exists
        integer file_unit, i, j, k, l

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=5), ubound(x, dim=5)
            do j = lbound(x, dim=4), ubound(x, dim=4)
                do k = lbound(x, dim=3), ubound(x, dim=3)
                    do l = lbound(x, dim=2), ubound(x, dim=2)
                        write(unit=file_unit) x(:,l,k,j,i)
                    end do
                end do
            end do
        end do

        close(file_unit)
    end procedure to_binary_5dr64
    module procedure to_binary_5dr32
        logical exists
        integer file_unit, i, j, k, l

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=5), ubound(x, dim=5)
            do j = lbound(x, dim=4), ubound(x, dim=4)
                do k = lbound(x, dim=3), ubound(x, dim=3)
                    do l = lbound(x, dim=2), ubound(x, dim=2)
                        write(unit=file_unit) x(:,l,k,j,i)
                    end do
                end do
            end do
        end do

        close(file_unit)
    end procedure to_binary_5dr32

    module procedure to_binary_1di64
        logical exists
        integer file_unit

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1di64
    module procedure to_binary_1di32
        logical exists
        integer file_unit

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1di32
    module procedure to_binary_1di16
        logical exists
        integer file_unit

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1di16
    module procedure to_binary_1di8
        logical exists
        integer file_unit

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        write(unit=file_unit) x

        close(file_unit)
    end procedure to_binary_1di8

    module procedure to_binary_2di64
        logical exists
        integer file_unit, i

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=2), ubound(x, dim=2)
            write(unit=file_unit) x(:,i)
        end do

        close(file_unit)
    end procedure to_binary_2di64
    module procedure to_binary_2di32
        logical exists
        integer file_unit, i

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=2), ubound(x, dim=2)
            write(unit=file_unit) x(:,i)
        end do

        close(file_unit)
    end procedure to_binary_2di32
    module procedure to_binary_2di16
        logical exists
        integer file_unit, i

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=2), ubound(x, dim=2)
            write(unit=file_unit) x(:,i)
        end do

        close(file_unit)
    end procedure to_binary_2di16
    module procedure to_binary_2di8
        logical exists
        integer file_unit, i

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=2), ubound(x, dim=2)
            write(unit=file_unit) x(:,i)
        end do

        close(file_unit)
    end procedure to_binary_2di8

    module procedure to_binary_3di64
        logical exists
        integer file_unit, i, j

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=3), ubound(x, dim=3)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,j,i)
            end do
        end do

        close(file_unit)
    end procedure to_binary_3di64
    module procedure to_binary_3di32
        logical exists
        integer file_unit, i, j

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=3), ubound(x, dim=3)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,j,i)
            end do
        end do

        close(file_unit)
    end procedure to_binary_3di32
    module procedure to_binary_3di16
        logical exists
        integer file_unit, i, j

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=3), ubound(x, dim=3)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,j,i)
            end do
        end do

        close(file_unit)
    end procedure to_binary_3di16
    module procedure to_binary_3di8
        logical exists
        integer file_unit, i, j

        if ( .not. any(binary_ext == ext_of(file_name)) ) then
            error stop nl//'Unsupported file extension for file "'//file_name//'" in call to to_binary.'// &
                       nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
        end if

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='unformatted', &
                  action='write', access='stream' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='unformatted', &
                  action='write', access='stream' )
        end if

        do i = lbound(x, dim=3), ubound(x, dim=3)
            do j = lbound(x, dim=2), ubound(x, dim=2)
                write(unit=file_unit) x(:,j,i)
            end do
        end do

        close(file_unit)
    end procedure to_binary_3di8

    !! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure from_binary_1dr64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_1dr64
    module procedure from_binary_1dr32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_1dr32

    module procedure from_binary_2dr64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_2dr64
    module procedure from_binary_2dr32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_2dr32

    module procedure from_binary_3dr64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_3dr64
    module procedure from_binary_3dr32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_3dr32

    module procedure from_binary_4dr64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_4dr64
    module procedure from_binary_4dr32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_4dr32

    module procedure from_binary_5dr64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_5dr64
    module procedure from_binary_5dr32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_5dr32

    module procedure from_binary_1di64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_1di64
    module procedure from_binary_1di32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_1di32
    module procedure from_binary_1di16
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_1di16
    module procedure from_binary_1di8
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_1di8

    module procedure from_binary_2di64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_2di64
    module procedure from_binary_2di32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_2di32
    module procedure from_binary_2di16
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_2di16
    module procedure from_binary_2di8
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_2di8

    module procedure from_binary_3di64
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_3di64
    module procedure from_binary_3di32
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_3di32
    module procedure from_binary_3di16
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_3di16
    module procedure from_binary_3di8
        logical exists
        integer file_unit, status

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            if ( .not. any(binary_ext == ext_of(file_name)) ) then
                error stop nl//'Unsupported file extension for file "'//file_name//'" in call to from_binary.'// &
                           nl//'Try one of the following: '//to_str(binary_ext, delim=' ')//nl
            end if
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream' )
        else
            error stop nl//'Error reading file "'//file_name//'". No such file exists.'//nl
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=status) into

        close(file_unit)
    end procedure from_binary_3di8
end submodule binary_io
