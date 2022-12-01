!!---------------------------------------------------------------------------------------------------------------------
!!  This module file contains common i/o procedures for arrays of real and integer type. Common operations include
!!  printing arrays to stdout with a specified format, reading/writing arrays from/to text files and binary files.
!!  Convenience functions for number -> string conversion are provided as well as vector -> string conversion with
!!  a specified delimiter.
!!---------------------------------------------------------------------------------------------------------------------
module io_mod
    use, intrinsic :: iso_fortran_env, only: real64, real32, int64, int32, int16, int8, input_unit, output_unit
    implicit none (type,external)
    private

    !! Public APIs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    public :: aprint, to_file, from_file, to_str                                                           !! Array I/O
    public :: ext_of, echo, str                                                                           !! String I/O
    public :: nl                                                                                           !! Constants

    !! Definitions and Interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    character(len=1), parameter :: nl = new_line('a')                                             !! New line character

    character(len=*), dimension(*), parameter :: text_ext = ['csv', 'txt', 'ods', 'odf', 'odm', 'odt', 'xls', 'xlsx', &
                                                             'doc', 'docx', 'md', 'log', 'rtf', 'org', 'embed']
    character(len=*), dimension(*), parameter :: binary_ext = ['dat', 'bin']

    character(len=*), dimension(*), parameter :: non_separating_chars_us = ['0', '1', '2', '3', '4', '5', '6', '7', &
                                                                            '8', '9', '.', 'e', 'E', '+', '-', '"']
    character(len=*), dimension(*), parameter :: non_separating_chars_eu = ['0', '1', '2', '3', '4', '5', '6', '7', &
                                                                            '8', '9', ',', 'e', 'E', '+', '-', '"']

    character(len=*), dimension(*), parameter :: letters = ['a', 'A', 'b', 'B', 'c', 'C', 'd', 'D', 'e', 'E', &
                                                            'f', 'F', 'g', 'G', 'h', 'H', 'i', 'I', 'j', 'J', &
                                                            'k', 'K', 'l', 'L', 'm', 'M', 'n', 'N', 'o', 'O', &
                                                            'p', 'P', 'q', 'Q', 'r', 'R', 's', 'S', 't', 'T', &
                                                            'u', 'U', 'v', 'V', 'w', 'W', 'x', 'X', 'y', 'Y', 'z', 'Z']

    interface aprint                                                                        !! Submodule array_printing
        module impure subroutine aprint_1dr64(x, fmt, decimals)
            real(real64), dimension(:), intent(in) :: x
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
        end subroutine aprint_1dr64
        module impure subroutine aprint_1dr32(x, fmt, decimals)
            real(real32), dimension(:), intent(in) :: x
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
        end subroutine aprint_1dr32

        module impure subroutine aprint_2dr64(x, fmt, decimals)
            real(real64), dimension(:,:), intent(in) :: x
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
        end subroutine aprint_2dr64
        module impure subroutine aprint_2dr32(x, fmt, decimals)
            real(real32), dimension(:,:), intent(in) :: x
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
        end subroutine aprint_2dr32

        module impure subroutine aprint_1di64(x)
            integer(int64), dimension(:), intent(in) :: x
        end subroutine aprint_1di64
        module impure subroutine aprint_1di32(x)
            integer(int32), dimension(:), intent(in) :: x
        end subroutine aprint_1di32
        module impure subroutine aprint_1di16(x)
            integer(int16), dimension(:), intent(in) :: x
        end subroutine aprint_1di16
        module impure subroutine aprint_1di8(x)
            integer(int8), dimension(:), intent(in) :: x
        end subroutine aprint_1di8

        module impure subroutine aprint_2di64(x)
            integer(int64), dimension(:,:), intent(in) :: x
        end subroutine aprint_2di64
        module impure subroutine aprint_2di32(x)
            integer(int32), dimension(:,:), intent(in) :: x
        end subroutine aprint_2di32
        module impure subroutine aprint_2di16(x)
            integer(int16), dimension(:,:), intent(in) :: x
        end subroutine aprint_2di16
        module impure subroutine aprint_2di8(x)
            integer(int8), dimension(:,:), intent(in) :: x
        end subroutine aprint_2di8

        module impure subroutine aprint_1dchar(x)
            character(len=*), dimension(:), intent(in) :: x
        end subroutine aprint_1dchar

        module impure subroutine aprint_2dchar(x)
            character(len=*), dimension(:,:), intent(in) :: x
        end subroutine aprint_2dchar
    end interface

    interface str                                                                              !! Submodule internal_io
        module pure function str_r64(x, fmt, decimals) result(x_str)
            real(real64), intent(in) :: x
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
            character(len=:), allocatable :: x_str
        end function str_r64
        module pure function str_r32(x, fmt, decimals) result(x_str)
            real(real32), intent(in) :: x
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
            character(len=:), allocatable :: x_str
        end function str_r32

        module pure function str_i64(x) result(x_str)
            integer(int64), intent(in) :: x
            character(len=:), allocatable :: x_str
        end function str_i64
        module pure function str_i32(x) result(x_str)
            integer(int32), intent(in) :: x
            character(len=:), allocatable :: x_str
        end function str_i32
        module pure function str_i16(x) result(x_str)
            integer(int16), intent(in) :: x
            character(len=:), allocatable :: x_str
        end function str_i16
        module pure function str_i8(x) result(x_str)
            integer(int8), intent(in) :: x
            character(len=:), allocatable :: x_str
        end function str_i8
    end interface

    interface to_str                                                                           !! Submodule internal_io
        module pure function to_str_charvec(x, delim) result(x_str)
            character(len=*), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: delim
            character(len=:), allocatable :: x_str
        end function to_str_charvec

        module pure function to_str_1dr64(x, delim, fmt, decimals) result(x_str)
            real(real64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: delim
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
            character(len=:), allocatable :: x_str
        end function to_str_1dr64
        module pure function to_str_1dr32(x, delim, fmt, decimals) result(x_str)
            real(real32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: delim
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
            character(len=:), allocatable :: x_str
        end function to_str_1dr32

        module pure function to_str_1di64(x, delim) result(x_str)
            integer(int64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: delim
            character(len=:), allocatable :: x_str
        end function to_str_1di64
        module pure function to_str_1di32(x, delim) result(x_str)
            integer(int32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: delim
            character(len=:), allocatable :: x_str
        end function to_str_1di32
        module pure function to_str_1di16(x, delim) result(x_str)
            integer(int16), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: delim
            character(len=:), allocatable :: x_str
        end function to_str_1di16
        module pure function to_str_1di8(x, delim) result(x_str)
            integer(int8), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: delim
            character(len=:), allocatable :: x_str
        end function to_str_1di8
    end interface

    interface to_file                                                                              !! Submodule file_io
        module impure subroutine to_file_1dr64(x, file_name, header, dim, delim, fmt, decimals)
            real(real64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            integer, intent(in), optional :: dim
            character(len=*), intent(in), optional :: delim
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
        end subroutine to_file_1dr64
        module impure subroutine to_file_1dr32(x, file_name, header, dim, delim, fmt, decimals)
            real(real32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            integer, intent(in), optional :: dim
            character(len=*), intent(in), optional :: delim
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
        end subroutine to_file_1dr32

        module impure subroutine to_file_2dr64(x, file_name, header, delim, fmt, decimals)
            real(real64), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            character(len=*), intent(in), optional :: delim
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
        end subroutine to_file_2dr64
        module impure subroutine to_file_2dr32(x, file_name, header, delim, fmt, decimals)
            real(real32), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            character(len=*), intent(in), optional :: delim
            character(len=1), intent(in), optional :: fmt
            integer, intent(in), optional :: decimals
        end subroutine to_file_2dr32

        module impure subroutine to_file_3dr64(x, file_name)
            real(real64), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_file_3dr64
        module impure subroutine to_file_3dr32(x, file_name)
            real(real32), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_file_3dr32

        module impure subroutine to_file_4dr64(x, file_name)
            real(real64), dimension(:,:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_file_4dr64
        module impure subroutine to_file_4dr32(x, file_name)
            real(real32), dimension(:,:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_file_4dr32

        module impure subroutine to_file_5dr64(x, file_name)
            real(real64), dimension(:,:,:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_file_5dr64
        module impure subroutine to_file_5dr32(x, file_name)
            real(real32), dimension(:,:,:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_file_5dr32

        module impure subroutine to_file_1di64(x, file_name, header, dim, delim)
            integer(int64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            integer, intent(in), optional :: dim
            character(len=*), intent(in), optional :: delim
        end subroutine to_file_1di64
        module impure subroutine to_file_1di32(x, file_name, header, dim, delim)
            integer(int32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            integer, intent(in), optional :: dim
            character(len=*), intent(in), optional :: delim
        end subroutine to_file_1di32
        module impure subroutine to_file_1di16(x, file_name, header, dim, delim)
            integer(int16), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            integer, intent(in), optional :: dim
            character(len=*), intent(in), optional :: delim
        end subroutine to_file_1di16
        module impure subroutine to_file_1di8(x, file_name, header, dim, delim)
            integer(int8), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            integer, intent(in), optional :: dim
            character(len=*), intent(in), optional :: delim
        end subroutine to_file_1di8

        module impure subroutine to_file_2di64(x, file_name, header, delim)
            integer(int64), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            character(len=*), intent(in), optional :: delim
        end subroutine to_file_2di64
        module impure subroutine to_file_2di32(x, file_name, header, delim)
            integer(int32), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            character(len=*), intent(in), optional :: delim
        end subroutine to_file_2di32
        module impure subroutine to_file_2di16(x, file_name, header, delim)
            integer(int16), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            character(len=*), intent(in), optional :: delim
        end subroutine to_file_2di16
        module impure subroutine to_file_2di8(x, file_name, header, delim)
            integer(int8), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in), optional :: header
            character(len=*), intent(in), optional :: delim
        end subroutine to_file_2di8

        module impure subroutine to_file_3di64(x, file_name)
            integer(int64), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_file_3di64
        module impure subroutine to_file_3di32(x, file_name)
            integer(int32), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_file_3di32
        module impure subroutine to_file_3di16(x, file_name)
            integer(int16), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_file_3di16
        module impure subroutine to_file_3di8(x, file_name)
            integer(int8), dimension(:,:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
        end subroutine to_file_3di8
    end interface

    interface from_file                                                                            !! Submodule file_io
        module impure subroutine from_textfile_1dr64(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_1dr64
        module impure subroutine from_binaryfile_1dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_1dr64
        module impure subroutine from_textfile_1dr32(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_1dr32
        module impure subroutine from_binaryfile_1dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_1dr32

        module impure subroutine from_textfile_2dr64(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_2dr64
        module impure subroutine from_binaryfile_2dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_2dr64
        module impure subroutine from_textfile_2dr32(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_2dr32
        module impure subroutine from_binaryfile_2dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_2dr32

        module impure subroutine from_file_3dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_file_3dr64
        module impure subroutine from_file_3dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_file_3dr32

        module impure subroutine from_file_4dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_file_4dr64
        module impure subroutine from_file_4dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_file_4dr32

        module impure subroutine from_file_5dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:,:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_file_5dr64
        module impure subroutine from_file_5dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:,:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_file_5dr32

        module impure subroutine from_textfile_1di64(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_1di64
        module impure subroutine from_binaryfile_1di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_1di64
        module impure subroutine from_textfile_1di32(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_1di32
        module impure subroutine from_binaryfile_1di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_1di32
        module impure subroutine from_textfile_1di16(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_1di16
        module impure subroutine from_binaryfile_1di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_1di16
        module impure subroutine from_textfile_1di8(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_1di8
        module impure subroutine from_binaryfile_1di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_1di8

        module impure subroutine from_textfile_2di64(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_2di64
        module impure subroutine from_binaryfile_2di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_2di64
        module impure subroutine from_textfile_2di32(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_2di32
        module impure subroutine from_binaryfile_2di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_2di32
        module impure subroutine from_textfile_2di16(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_2di16
        module impure subroutine from_binaryfile_2di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_2di16
        module impure subroutine from_textfile_2di8(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in), optional :: header
            character(len=2), intent(in), optional :: locale
        end subroutine from_textfile_2di8
        module impure subroutine from_binaryfile_2di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binaryfile_2di8

        module impure subroutine from_file_3di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_file_3di64
        module impure subroutine from_file_3di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_file_3di32
        module impure subroutine from_file_3di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_file_3di16
        module impure subroutine from_file_3di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_file_3di8
    end interface

    interface echo                                                                                 !! Submodule text_io
        module impure subroutine echo_string(string, file_name, append)
            character(len=*), intent(in) :: string
            character(len=*), intent(in) :: file_name
            logical, optional, intent(in) :: append
        end subroutine echo_string
    end interface

    interface to_text                                                                              !! Submodule text_io
        module impure subroutine to_text_1dr64(x, file_name, header, dim, delim, fmt, decimals)
            real(real64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            integer, intent(in) :: dim
            character(len=*), intent(in) :: delim
            character(len=1), intent(in) :: fmt
            integer, intent(in) :: decimals
        end subroutine to_text_1dr64
        module impure subroutine to_text_1dr32(x, file_name, header, dim, delim, fmt, decimals)
            real(real32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            integer, intent(in) :: dim
            character(len=*), intent(in) :: delim
            character(len=1), intent(in) :: fmt
            integer, intent(in) :: decimals
        end subroutine to_text_1dr32

        module impure subroutine to_text_2dr64(x, file_name, header, delim, fmt, decimals)
            real(real64), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            character(len=*), intent(in) :: delim
            character(len=1), intent(in) :: fmt
            integer, intent(in) :: decimals
        end subroutine to_text_2dr64
        module impure subroutine to_text_2dr32(x, file_name, header, delim, fmt, decimals)
            real(real32), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            character(len=*), intent(in) :: delim
            character(len=1), intent(in) :: fmt
            integer, intent(in) :: decimals
        end subroutine to_text_2dr32

        module impure subroutine to_text_1di64(x, file_name, header, dim, delim)
            integer(int64), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            integer, intent(in) :: dim
            character(len=*), intent(in) :: delim
        end subroutine to_text_1di64
        module impure subroutine to_text_1di32(x, file_name, header, dim, delim)
            integer(int32), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            integer, intent(in) :: dim
            character(len=*), intent(in) :: delim
        end subroutine to_text_1di32
        module impure subroutine to_text_1di16(x, file_name, header, dim, delim)
            integer(int16), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            integer, intent(in) :: dim
            character(len=*), intent(in) :: delim
        end subroutine to_text_1di16
        module impure subroutine to_text_1di8(x, file_name, header, dim, delim)
            integer(int8), dimension(:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            integer, intent(in) :: dim
            character(len=*), intent(in) :: delim
        end subroutine to_text_1di8

        module impure subroutine to_text_2di64(x, file_name, header, delim)
            integer(int64), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            character(len=*), intent(in) :: delim
        end subroutine to_text_2di64
        module impure subroutine to_text_2di32(x, file_name, header, delim)
            integer(int32), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            character(len=*), intent(in) :: delim
        end subroutine to_text_2di32
        module impure subroutine to_text_2di16(x, file_name, header, delim)
            integer(int16), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            character(len=*), intent(in) :: delim
        end subroutine to_text_2di16
        module impure subroutine to_text_2di8(x, file_name, header, delim)
            integer(int8), dimension(:,:), intent(in) :: x
            character(len=*), intent(in) :: file_name
            character(len=*), dimension(:), intent(in) :: header
            character(len=*), intent(in) :: delim
        end subroutine to_text_2di8
    end interface

    interface from_text                                                                            !! Submodule text_io
        module impure subroutine from_text_1dr64(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_1dr64
        module impure subroutine from_text_1dr32(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_1dr32

        module impure subroutine from_text_2dr64(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_2dr64
        module impure subroutine from_text_2dr32(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_2dr32

        module impure subroutine from_text_1di64(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_1di64
        module impure subroutine from_text_1di32(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_1di32
        module impure subroutine from_text_1di16(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_1di16
        module impure subroutine from_text_1di8(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_1di8

        module impure subroutine from_text_2di64(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_2di64
        module impure subroutine from_text_2di32(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_2di32
        module impure subroutine from_text_2di16(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_2di16
        module impure subroutine from_text_2di8(file_name, into, header, locale)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:,:), intent(out) :: into
            logical, intent(in) :: header
            character(len=2), intent(in) :: locale
        end subroutine from_text_2di8
    end interface

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

    interface from_binary                                                                        !! Submodule binary_io
        module impure subroutine from_binary_1dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_1dr64
        module impure subroutine from_binary_1dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_1dr32

        module impure subroutine from_binary_2dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_2dr64
        module impure subroutine from_binary_2dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_2dr32

        module impure subroutine from_binary_3dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_3dr64
        module impure subroutine from_binary_3dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_3dr32

        module impure subroutine from_binary_4dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_4dr64
        module impure subroutine from_binary_4dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_4dr32

        module impure subroutine from_binary_5dr64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real64), allocatable, dimension(:,:,:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_5dr64
        module impure subroutine from_binary_5dr32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            real(real32), allocatable, dimension(:,:,:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_5dr32

        module impure subroutine from_binary_1di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:), intent(out) :: into
            integer, intent(in) :: data_shape(1)
        end subroutine from_binary_1di64
        module impure subroutine from_binary_1di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:), intent(out) :: into
            integer, intent(in) :: data_shape(1)
        end subroutine from_binary_1di32
        module impure subroutine from_binary_1di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:), intent(out) :: into
            integer, intent(in) :: data_shape(1)
        end subroutine from_binary_1di16
        module impure subroutine from_binary_1di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:), intent(out) :: into
            integer, intent(in) :: data_shape(1)
        end subroutine from_binary_1di8

        module impure subroutine from_binary_2di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_2di64
        module impure subroutine from_binary_2di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_2di32
        module impure subroutine from_binary_2di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_2di16
        module impure subroutine from_binary_2di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_2di8

        module impure subroutine from_binary_3di64(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int64), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_3di64
        module impure subroutine from_binary_3di32(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int32), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_3di32
        module impure subroutine from_binary_3di16(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int16), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
        end subroutine from_binary_3di16
        module impure subroutine from_binary_3di8(file_name, into, data_shape)
            character(len=*), intent(in) :: file_name
            integer(int8), allocatable, dimension(:,:,:), intent(out) :: into
            integer, dimension(:), intent(in) :: data_shape
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

end module io_mod

submodule (io_mod) array_printing
    contains
    module procedure aprint_1dr64
        character(len=:), allocatable, dimension(:) :: x_str
        character(len=1) :: fmt_
        integer :: i, n_rows, decimals_

        if ( .not. present(fmt) ) then
            fmt_ = 'e'
        else
            if ( (fmt == 'f') .or. (fmt == 'e') ) then
                fmt_ = fmt
            else
                return
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 1
        else
            decimals_ = decimals
        end if

        n_rows = size(x)

        allocate( x_str(n_rows), source=str(x(1), fmt=fmt_, decimals=decimals_) )

        if ( n_rows > 1 ) then
            do concurrent (i = 2:n_rows)
                x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_)
            end do
        end if

        write(unit=*, fmt='(a)') nl//'   ┣ '//x_str(1)//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                write(unit=*, fmt='(a)') '   ┃ '//x_str(i)//' ┃'
            end do
        end if

        write(unit=*, fmt='(a)') '   ┣ '//x_str(n_rows)//' ┫'//nl
    end procedure aprint_1dr64
    module procedure aprint_1dr32
        character(len=:), allocatable, dimension(:) :: x_str
        character(len=1) :: fmt_
        integer :: i, n_rows, decimals_

        if ( .not. present(fmt) ) then
            fmt_ = 'e'
        else
            if ( (fmt == 'f') .or. (fmt == 'e') ) then
                fmt_ = fmt
            else
                return
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 1
        else
            decimals_ = decimals
        end if

        n_rows = size(x)

        allocate( x_str(n_rows), source=str(x(1), fmt=fmt_, decimals=decimals_) )

        if ( n_rows > 1 ) then
            do concurrent (i = 2:n_rows)
                x_str(i) = str(x(i), fmt=fmt_, decimals=decimals_)
            end do
        end if

        write(unit=*, fmt='(a)') nl//'   ┣ '//x_str(1)//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                write(unit=*, fmt='(a)') '   ┃ '//x_str(i)//' ┃'
            end do
        end if

        write(unit=*, fmt='(a)') '   ┣ '//x_str(n_rows)//' ┫'//nl
    end procedure aprint_1dr32

    module procedure aprint_2dr64
        character(len=:), allocatable, dimension(:,:) :: x_str
        character(len=1) :: fmt_
        integer :: i, j, n_rows, n_columns, decimals_

        if ( .not. present(fmt) ) then
            fmt_ = 'e'
        else
            if ( (fmt == 'f') .or. (fmt == 'e') ) then
                fmt_ = fmt
            else
                return
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 1
        else
            decimals_ = decimals
        end if

        n_rows = size(x, dim=1)
        n_columns = size(x, dim=2)

        allocate( x_str(n_rows, n_columns), source=str(x(1,1), fmt=fmt_, decimals=decimals_) )

        do concurrent (j = 1:n_columns, i = 1:n_rows)
            x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_)
        end do

        write(unit=*, fmt='(a)') nl//'   ┣ '//to_str(x_str(1,:), delim=' ')//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                write(unit=*, fmt='(a)') '   ┃ '//to_str(x_str(i,:), delim=' ')//' ┃'
            end do
        end if

        write(unit=*, fmt='(a)') '   ┣ '//to_str(x_str(n_rows,:), delim=' ')//' ┫'//nl
    end procedure aprint_2dr64
    module procedure aprint_2dr32
        character(len=:), allocatable, dimension(:,:) :: x_str
        character(len=1) :: fmt_
        integer :: i, j, n_rows, n_columns, decimals_

        if ( .not. present(fmt) ) then
            fmt_ = 'e'
        else
            if ( (fmt == 'f') .or. (fmt == 'e') ) then
                fmt_ = fmt
            else
                return
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 1
        else
            decimals_ = decimals
        end if

        n_rows = size(x, dim=1)
        n_columns = size(x, dim=2)

        allocate( x_str(n_rows, n_columns), source=str(x(1,1), fmt=fmt_, decimals=decimals_) )

        do concurrent (j = 1:n_columns, i = 1:n_rows)
            x_str(i,j) = str(x(i,j), fmt=fmt_, decimals=decimals_)
        end do

        write(unit=*, fmt='(a)') nl//'   ┣ '//to_str(x_str(1,:), delim=' ')//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                write(unit=*, fmt='(a)') '   ┃ '//to_str(x_str(i,:), delim=' ')//' ┃'
            end do
        end if

        write(unit=*, fmt='(a)') '   ┣ '//to_str(x_str(n_rows,:), delim=' ')//' ┫'//nl
    end procedure aprint_2dr32

    module procedure aprint_1di64
        character(len=:), allocatable, dimension(:) :: x_str
        character(len=:), allocatable :: x_max_str, x_min_str, source
        integer :: i, n_rows

        n_rows = size(x)

        x_max_str = str(maxval(x))
        x_min_str = str(minval(x))

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(n_rows), source=source )

        do concurrent (i = 1:n_rows)
            x_str(i) = str(x(i))
        end do

        write(unit=*, fmt='(a)') nl//'   ┣ '//adjustr(x_str(1))//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                write(unit=*, fmt='(a)') '   ┃ '//adjustr(x_str(i))//' ┃'
            end do
        end if

        write(unit=*, fmt='(a)') '   ┣ '//adjustr(x_str(n_rows))//' ┫'//nl
    end procedure aprint_1di64
    module procedure aprint_1di32
        character(len=:), allocatable, dimension(:) :: x_str
        character(len=:), allocatable :: x_max_str, x_min_str, source
        integer :: i, n_rows

        n_rows = size(x)

        x_max_str = str(maxval(x))
        x_min_str = str(minval(x))

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(n_rows), source=source )

        do concurrent (i = 1:n_rows)
            x_str(i) = str(x(i))
        end do

        write(unit=*, fmt='(a)') nl//'   ┣ '//adjustr(x_str(1))//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                write(unit=*, fmt='(a)') '   ┃ '//adjustr(x_str(i))//' ┃'
            end do
        end if

        write(unit=*, fmt='(a)') '   ┣ '//adjustr(x_str(n_rows))//' ┫'//nl
    end procedure aprint_1di32
    module procedure aprint_1di16
        character(len=:), allocatable, dimension(:) :: x_str
        character(len=:), allocatable :: x_max_str, x_min_str, source
        integer :: i, n_rows

        n_rows = size(x)

        x_max_str = str(maxval(x))
        x_min_str = str(minval(x))

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(n_rows), source=source )

        do concurrent (i = 1:n_rows)
            x_str(i) = str(x(i))
        end do

        write(unit=*, fmt='(a)') nl//'   ┣ '//adjustr(x_str(1))//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                write(unit=*, fmt='(a)') '   ┃ '//adjustr(x_str(i))//' ┃'
            end do
        end if

        write(unit=*, fmt='(a)') '   ┣ '//adjustr(x_str(n_rows))//' ┫'//nl
    end procedure aprint_1di16
    module procedure aprint_1di8
        character(len=:), allocatable, dimension(:) :: x_str
        character(len=:), allocatable :: x_max_str, x_min_str, source
        integer :: i, n_rows

        n_rows = size(x)

        x_max_str = str(maxval(x))
        x_min_str = str(minval(x))

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(n_rows), source=source )

        do concurrent (i = 1:n_rows)
            x_str(i) = str(x(i))
        end do

        write(unit=*, fmt='(a)') nl//'   ┣ '//adjustr(x_str(1))//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                write(unit=*, fmt='(a)') '   ┃ '//adjustr(x_str(i))//' ┃'
            end do
        end if

        write(unit=*, fmt='(a)') '   ┣ '//adjustr(x_str(n_rows))//' ┫'//nl
    end procedure aprint_1di8

    module procedure aprint_2di64
        character(len=:), allocatable, dimension(:,:) :: x_str
        character(len=:), allocatable :: x_max_str, x_min_str, source, str_tmp
        integer :: i, j, n_rows, n_columns

        n_rows = size(x, dim=1)
        n_columns = size(x, dim=2)

        x_max_str = str(maxval(x))
        x_min_str = str(minval(x))

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(n_rows, n_columns), source=source )

        do concurrent (j = 1:n_columns, i = 1:n_rows)
            x_str(i,j) = str(x(i,j))
        end do

        str_tmp = ''
        do j = 1, n_columns - 1
            str_tmp = str_tmp//adjustr(x_str(1,j))//' '
        end do
        str_tmp = str_tmp//adjustr(x_str(1,n_columns))

        write(unit=*, fmt='(a)') nl//'   ┣ '//str_tmp//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                str_tmp = ''
                do j = 1, n_columns - 1
                    str_tmp = str_tmp//adjustr(x_str(i,j))//' '
                end do
                str_tmp = str_tmp//adjustr(x_str(i,n_columns))

                write(unit=*, fmt='(a)') '   ┃ '//str_tmp//' ┃'
            end do
        end if

        str_tmp = ''
        do j = 1, n_columns - 1
            str_tmp = str_tmp//adjustr(x_str(n_rows,j))//' '
        end do
        str_tmp = str_tmp//adjustr(x_str(n_rows,n_columns))

        write(unit=*, fmt='(a)') '   ┣ '//str_tmp//' ┫'//nl
    end procedure aprint_2di64
    module procedure aprint_2di32
        character(len=:), allocatable, dimension(:,:) :: x_str
        character(len=:), allocatable :: x_max_str, x_min_str, source, str_tmp
        integer :: i, j, n_rows, n_columns

        n_rows = size(x, dim=1)
        n_columns = size(x, dim=2)

        x_max_str = str(maxval(x))
        x_min_str = str(minval(x))

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(n_rows, n_columns), source=source )

        do concurrent (j = 1:n_columns, i = 1:n_rows)
            x_str(i,j) = str(x(i,j))
        end do

        str_tmp = ''
        do j = 1, n_columns - 1
            str_tmp = str_tmp//adjustr(x_str(1,j))//' '
        end do
        str_tmp = str_tmp//adjustr(x_str(1,n_columns))

        write(unit=*, fmt='(a)') nl//'   ┣ '//str_tmp//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                str_tmp = ''
                do j = 1, n_columns - 1
                    str_tmp = str_tmp//adjustr(x_str(i,j))//' '
                end do
                str_tmp = str_tmp//adjustr(x_str(i,n_columns))

                write(unit=*, fmt='(a)') '   ┃ '//str_tmp//' ┃'
            end do
        end if

        str_tmp = ''
        do j = 1, n_columns - 1
            str_tmp = str_tmp//adjustr(x_str(n_rows,j))//' '
        end do
        str_tmp = str_tmp//adjustr(x_str(n_rows,n_columns))

        write(unit=*, fmt='(a)') '   ┣ '//str_tmp//' ┫'//nl
    end procedure aprint_2di32
    module procedure aprint_2di16
        character(len=:), allocatable, dimension(:,:) :: x_str
        character(len=:), allocatable :: x_max_str, x_min_str, source, str_tmp
        integer :: i, j, n_rows, n_columns

        n_rows = size(x, dim=1)
        n_columns = size(x, dim=2)

        x_max_str = str(maxval(x))
        x_min_str = str(minval(x))

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(n_rows, n_columns), source=source )

        do concurrent (j = 1:n_columns, i = 1:n_rows)
            x_str(i,j) = str(x(i,j))
        end do

        str_tmp = ''
        do j = 1, n_columns - 1
            str_tmp = str_tmp//adjustr(x_str(1,j))//' '
        end do
        str_tmp = str_tmp//adjustr(x_str(1,n_columns))

        write(unit=*, fmt='(a)') nl//'   ┣ '//str_tmp//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                str_tmp = ''
                do j = 1, n_columns - 1
                    str_tmp = str_tmp//adjustr(x_str(i,j))//' '
                end do
                str_tmp = str_tmp//adjustr(x_str(i,n_columns))

                write(unit=*, fmt='(a)') '   ┃ '//str_tmp//' ┃'
            end do
        end if

        str_tmp = ''
        do j = 1, n_columns - 1
            str_tmp = str_tmp//adjustr(x_str(n_rows,j))//' '
        end do
        str_tmp = str_tmp//adjustr(x_str(n_rows,n_columns))

        write(unit=*, fmt='(a)') '   ┣ '//str_tmp//' ┫'//nl
    end procedure aprint_2di16
    module procedure aprint_2di8
        character(len=:), allocatable, dimension(:,:) :: x_str
        character(len=:), allocatable :: x_max_str, x_min_str, source, str_tmp
        integer :: i, j, n_rows, n_columns

        n_rows = size(x, dim=1)
        n_columns = size(x, dim=2)

        x_max_str = str(maxval(x))
        x_min_str = str(minval(x))

        if ( len(x_max_str) > len(x_min_str) ) then
            source = x_max_str
        else
            source = x_min_str
        end if

        allocate( x_str(n_rows, n_columns), source=source )

        do concurrent (j = 1:n_columns, i = 1:n_rows)
            x_str(i,j) = str(x(i,j))
        end do

        str_tmp = ''
        do j = 1, n_columns - 1
            str_tmp = str_tmp//adjustr(x_str(1,j))//' '
        end do
        str_tmp = str_tmp//adjustr(x_str(1,n_columns))

        write(unit=*, fmt='(a)') nl//'   ┣ '//str_tmp//' ┫'

        if ( n_rows == 1 ) return

        if ( n_rows > 2 ) then
            do i = 2, n_rows - 1
                str_tmp = ''
                do j = 1, n_columns - 1
                    str_tmp = str_tmp//adjustr(x_str(i,j))//' '
                end do
                str_tmp = str_tmp//adjustr(x_str(i,n_columns))

                write(unit=*, fmt='(a)') '   ┃ '//str_tmp//' ┃'
            end do
        end if

        str_tmp = ''
        do j = 1, n_columns - 1
            str_tmp = str_tmp//adjustr(x_str(n_rows,j))//' '
        end do
        str_tmp = str_tmp//adjustr(x_str(n_rows,n_columns))

        write(unit=*, fmt='(a)') '   ┣ '//str_tmp//' ┫'//nl
    end procedure aprint_2di8

    module procedure aprint_1dchar
        integer :: i

        write(unit=*, fmt='(a)') nl//'   ┣ '//adjustr(x(lbound(x, dim=1)))//' ┫'

        if ( size(x) == 1 ) return

        if ( size(x) > 2 ) then
            do i = lbound(x, dim=1) + 1, ubound(x, dim=1) - 1
                write(unit=*, fmt='(a)') '   ┃ '//adjustr(x(i))//' ┃'
            end do
        end if
        
        write(unit=*, fmt='(a)') '   ┣ '//adjustr(x(ubound(x, dim=1)))//' ┫'//nl
    end procedure aprint_1dchar

    module procedure aprint_2dchar
        character(len=:), allocatable :: str_tmp
        integer :: i, j

        str_tmp = ''
        do j = lbound(x, dim=2), ubound(x, dim=2) - 1
            str_tmp = str_tmp//adjustr(x(lbound(x, dim=1), j))//' '
        end do
        str_tmp = str_tmp//adjustr(x(lbound(x, dim=1), ubound(x, dim=2)))

        write(unit=*, fmt='(a)') nl//'   ┣ '//str_tmp//' ┫'

        if ( size(x, dim=1) == 1 ) return

        if ( size(x, dim=1) > 2 ) then
            do i = lbound(x, dim=1) + 1, ubound(x, dim=1) - 1
                str_tmp = ''
                do j = lbound(x, dim=2), ubound(x, dim=2) - 1
                    str_tmp = str_tmp//adjustr(x(i, j))//' '
                end do
                str_tmp = str_tmp//adjustr(x(i, ubound(x, dim=2)))

                write(unit=*, fmt='(a)') '   ┃ '//str_tmp//' ┃'
            end do
        end if

        str_tmp = ''
        do j = lbound(x, dim=2), ubound(x, dim=2) - 1
            str_tmp = str_tmp//adjustr(x(ubound(x, dim=1), j))//' '
        end do
        str_tmp = str_tmp//adjustr(x(ubound(x, dim=1), ubound(x, dim=2)))
        
        write(unit=*, fmt='(a)') '   ┣ '//str_tmp//' ┫'//nl
    end procedure aprint_2dchar
end submodule array_printing

submodule (io_mod) internal_io
    contains
    module procedure str_r64
        real(real64) :: comparison_number
        integer :: i, max_decimals, decimals_
        character(len=1) :: fmt_
        character(len=11) :: decimals_str
        character(len=32) :: str_tmp

        if ( .not. present(fmt) ) then
            fmt_ = 'e'
        else
            if ( (fmt == 'f') .or. (fmt == 'e') ) then
                fmt_ = fmt
            else
                x_str = ''
                return
            end if
        end if

        if ( fmt_ == 'e' ) then
            if ( .not. present(decimals) ) then
                decimals_ = 15
            else
                if ( decimals < 0 ) then
                    decimals_ = 0
                else if ( decimals > 15 ) then
                    decimals_ = 15
                else
                    decimals_ = decimals
                end if
            end if

            write(unit=decimals_str, fmt='(i11)') decimals_
            write(unit=str_tmp, fmt='(es32.'//trim(adjustl(decimals_str))//')') x
            x_str = trim(adjustl(str_tmp))
        else if ( fmt_ == 'f' ) then
            comparison_number = 1.0_real64

            find_max_decimals: do i = 15, 0, -1
                if ( abs(x) < comparison_number ) then
                    max_decimals = i
                    exit find_max_decimals
                end if
                comparison_number = 10.0_real64*comparison_number
            end do find_max_decimals

            if ( .not. present(decimals) ) then
                decimals_ = max_decimals
            else
                if ( decimals < 0 ) then
                    decimals_ = 0
                else if ( decimals > max_decimals ) then
                    decimals_ = max_decimals
                else
                    decimals_ = decimals
                end if
            end if

            write(unit=decimals_str, fmt='(i11)') decimals_
            write(unit=str_tmp, fmt='(f32.'//trim(adjustl(decimals_str))//')') x
            x_str = trim(adjustl(str_tmp))
        end if
    end procedure str_r64
    module procedure str_r32
        real :: comparison_number
        integer :: i, max_decimals, decimals_
        character(len=1) :: fmt_
        character(len=11) :: decimals_str
        character(len=32) :: str_tmp

        if ( .not. present(fmt) ) then
            fmt_ = 'e'
        else
            if ( (fmt == 'f') .or. (fmt == 'e') ) then
                fmt_ = fmt
            else
                x_str = ''
                return
            end if
        end if

        if ( fmt_ == 'e' ) then
            if ( .not. present(decimals) ) then
                decimals_ = 7
            else
                if ( decimals < 0 ) then
                    decimals_ = 0
                else if ( decimals > 7 ) then
                    decimals_ = 7
                else
                    decimals_ = decimals
                end if
            end if

            write(unit=decimals_str, fmt='(i11)') decimals_
            write(unit=str_tmp, fmt='(es32.'//trim(adjustl(decimals_str))//')') x
            x_str = trim(adjustl(str_tmp))
        else if ( fmt_ == 'f' ) then
            comparison_number = 1.0

            find_max_decimals: do i = 7, 0, -1
                if ( abs(x) < comparison_number ) then
                    max_decimals = i
                    exit find_max_decimals
                end if
                comparison_number = 10.0*comparison_number
            end do find_max_decimals

            if ( .not. present(decimals) ) then
                decimals_ = max_decimals
            else
                if ( decimals < 0 ) then
                    decimals_ = 0
                else if ( decimals > max_decimals ) then
                    decimals_ = max_decimals
                else
                    decimals_ = decimals
                end if
            end if

            write(unit=decimals_str, fmt='(i11)') decimals_
            write(unit=str_tmp, fmt='(f32.'//trim(adjustl(decimals_str))//')') x
            x_str = trim(adjustl(str_tmp))
        end if
    end procedure str_r32

    module procedure str_i64
        character(len=21) :: str_tmp

        write(unit=str_tmp, fmt='(i21)') x
        x_str = trim(adjustl(str_tmp))
    end procedure str_i64
    module procedure str_i32
        character(len=11) :: str_tmp

        write(unit=str_tmp, fmt='(i11)') x
        x_str = trim(adjustl(str_tmp))
    end procedure str_i32
    module procedure str_i16
        character(len=6) :: str_tmp

        write(unit=str_tmp, fmt='(i6)') x
        x_str = trim(adjustl(str_tmp))
    end procedure str_i16
    module procedure str_i8
        character(len=4) :: str_tmp

        write(unit=str_tmp, fmt='(i4)') x
        x_str = trim(adjustl(str_tmp))
    end procedure str_i8

    module procedure to_str_charvec
        integer :: i

        x_str = ''
        do i = 1, size(x)-1
            x_str = x_str//trim(adjustl(x(i)))//delim
        end do
        x_str = x_str//trim(adjustl(x(size(x))))
    end procedure to_str_charvec

    module procedure to_str_1dr64
        integer :: i, decimals_
        character(len=1) :: fmt_

        if ( .not. present(fmt) ) then
            fmt_ = 'e'
        else
            if ( (fmt == 'f') .or. (fmt == 'e') ) then
                fmt_ = fmt
            else
                x_str = ''
                return
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 15
        else
            decimals_ = decimals
        end if

        x_str = ''
        do i = 1, size(x)-1
            x_str = x_str//str(x(i), fmt=fmt_, decimals=decimals_)//delim
        end do
        x_str = x_str//str(x(size(x)), fmt=fmt_, decimals=decimals_)
    end procedure to_str_1dr64
    module procedure to_str_1dr32
        integer :: i, decimals_
        character(len=1) :: fmt_

        if ( .not. present(fmt) ) then
            fmt_ = 'e'
        else
            if ( (fmt == 'f') .or. (fmt == 'e') ) then
                fmt_ = fmt
            else
                x_str = ''
                return
            end if
        end if

        if ( .not. present(decimals) ) then
            decimals_ = 7
        else
            decimals_ = decimals
        end if

        x_str = ''
        do i = 1, size(x)-1
            x_str = x_str//str(x(i), fmt=fmt_, decimals=decimals_)//delim
        end do
        x_str = x_str//str(x(size(x)), fmt=fmt_, decimals=decimals_)
    end procedure to_str_1dr32

    module procedure to_str_1di64
        integer :: i

        x_str = ''
        do i = 1, size(x)-1
            x_str = x_str//str(x(i))//delim
        end do
        x_str = x_str//str(x(size(x)))
    end procedure to_str_1di64
    module procedure to_str_1di32
        integer :: i

        x_str = ''
        do i = 1, size(x)-1
            x_str = x_str//str(x(i))//delim
        end do
        x_str = x_str//str(x(size(x)))
    end procedure to_str_1di32
    module procedure to_str_1di16
        integer :: i

        x_str = ''
        do i = 1, size(x)-1
            x_str = x_str//str(x(i))//delim
        end do
        x_str = x_str//str(x(size(x)))
    end procedure to_str_1di16
    module procedure to_str_1di8
        integer :: i

        x_str = ''
        do i = 1, size(x)-1
            x_str = x_str//str(x(i))//delim
        end do
        x_str = x_str//str(x(size(x)))
    end procedure to_str_1di8
end submodule internal_io

submodule (io_mod) file_io
    contains
    !! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure to_file_1dr64
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_
        character(len=1) :: fmt_
        integer :: decimals_, hstat, dim_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
                hstat = 0
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
                    header_ = ['']
                    hstat = -1
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x))//').'
                else
                    header_ = header
                    if ( size(header) == 1 ) then
                        hstat = 1
                    else
                        hstat = 2
                    end if
                end if
            end if

            if ( .not. present(dim) ) then
                if ( hstat == 2 ) then
                    dim_ = 2
                else
                    dim_ = 1
                end if
            else
                if ( hstat == 2 ) then
                    dim_ = 2
                    if ( dim /= 2 ) then
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (2).'
                    end if
                else
                    if ( dim == 1 ) then
                        dim_ = 1
                    else if ( dim == 2 ) then
                        dim_ = 2
                    else
                        dim_ = 1
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (1).'
                    end if
                end if
            end if

            if ( .not. present(delim) ) then
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = ','
                end if
            else
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = delim
                end if
            end if

            if ( .not. present(fmt) ) then
                fmt_ = 'e'
            else
                if ( (fmt == 'f') .or. (fmt == 'e') ) then
                    fmt_ = fmt
                else
                    fmt_ = 'e'
                    write(*,'(a)') nl//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'".'// &
                                   nl//'Format must be "f" or "e"... defaulting to "e".'
                end if
            end if

            if ( .not. present(decimals) ) then
                decimals_ = 15
            else
                decimals_ = decimals
            end if
            
            call to_text( x=x, file_name=file_name, header=header_, dim=dim_, delim=delim_, &
                          fmt=fmt_, decimals=decimals_ )
        else if ( any(binary_ext == ext) ) then
            if ( present(header) )   write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(dim) )      write(*,'(a)') nl//'WARNING: dim not supported for file type "'//ext//'".'
            if ( present(delim) )    write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'
            if ( present(fmt) )      write(*,'(a)') nl//'WARNING: fmt not supported for file type "'//ext//'".'
            if ( present(decimals) ) write(*,'(a)') nl//'WARNING: decimals not supported for file type "'//ext//'".'

            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_1dr64
    module procedure to_file_1dr32
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_
        character(len=1) :: fmt_
        integer :: decimals_, hstat, dim_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
                hstat = 0
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
                    header_ = ['']
                    hstat = -1
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x))//').'
                else
                    header_ = header
                    if ( size(header) == 1 ) then
                        hstat = 1
                    else
                        hstat = 2
                    end if
                end if
            end if

            if ( .not. present(dim) ) then
                if ( hstat == 2 ) then
                    dim_ = 2
                else
                    dim_ = 1
                end if
            else
                if ( hstat == 2 ) then
                    dim_ = 2
                    if ( dim /= 2 ) then
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (2).'
                    end if
                else
                    if ( dim == 1 ) then
                        dim_ = 1
                    else if ( dim == 2 ) then
                        dim_ = 2
                    else
                        dim_ = 1
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (1).'
                    end if
                end if
            end if

            if ( .not. present(delim) ) then
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = ','
                end if
            else
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = delim
                end if
            end if

            if ( .not. present(fmt) ) then
                fmt_ = 'e'
            else
                if ( (fmt == 'f') .or. (fmt == 'e') ) then
                    fmt_ = fmt
                else
                    fmt_ = 'e'
                    write(*,'(a)') nl//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'".'// &
                                   nl//'Format must be "f" or "e"... defaulting to "e".'
                end if
            end if

            if ( .not. present(decimals) ) then
                decimals_ = 7
            else
                decimals_ = decimals
            end if
            
            call to_text( x=x, file_name=file_name, header=header_, dim=dim_, delim=delim_, &
                          fmt=fmt_, decimals=decimals_ )
        else if ( any(binary_ext == ext) ) then
            if ( present(header) )   write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(dim) )      write(*,'(a)') nl//'WARNING: dim not supported for file type "'//ext//'".'
            if ( present(delim) )    write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'
            if ( present(fmt) )      write(*,'(a)') nl//'WARNING: fmt not supported for file type "'//ext//'".'
            if ( present(decimals) ) write(*,'(a)') nl//'WARNING: decimals not supported for file type "'//ext//'".'
            
            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_1dr32

    module procedure to_file_2dr64
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_
        character(len=1) :: fmt_
        integer :: decimals_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
                    header_ = ['']
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x, dim=2))//').'
                else
                    header_ = header
                end if
            end if

            if ( .not. present(delim) ) then
                delim_ = ','
            else
                delim_ = delim
            end if

            if ( .not. present(fmt) ) then
                fmt_ = 'e'
            else
                if ( (fmt == 'f') .or. (fmt == 'e') ) then
                    fmt_ = fmt
                else
                    fmt_ = 'e'
                    write(*,'(a)') nl//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'".'// &
                                   nl//'Format must be "f" or "e"... defaulting to "e".'
                end if
            end if

            if ( .not. present(decimals) ) then
                decimals_ = 15
            else
                decimals_ = decimals
            end if
            
            call to_text(x=x, file_name=file_name, header=header_, delim=delim_, fmt=fmt_, decimals=decimals_)
        else if ( any(binary_ext == ext) ) then
            if ( present(header) )   write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(delim) )    write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'
            if ( present(fmt) )      write(*,'(a)') nl//'WARNING: fmt not supported for file type "'//ext//'".'
            if ( present(decimals) ) write(*,'(a)') nl//'WARNING: decimals not supported for file type "'//ext//'".'

            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_2dr64
    module procedure to_file_2dr32
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_
        character(len=1) :: fmt_
        integer :: decimals_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
                    header_ = ['']
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x, dim=2))//').'
                else
                    header_ = header
                end if
            end if

            if ( .not. present(delim) ) then
                delim_ = ','
            else
                delim_ = delim
            end if

            if ( .not. present(fmt) ) then
                fmt_ = 'e'
            else
                if ( (fmt == 'f') .or. (fmt == 'e') ) then
                    fmt_ = fmt
                else
                    fmt_ = 'e'
                    write(*,'(a)') nl//'WARNING: Invalid format "'//fmt//'" for file "'//file_name//'".'// &
                                   nl//'Format must be "f" or "e"... defaulting to "e".'
                end if
            end if

            if ( .not. present(decimals) ) then
                decimals_ = 7
            else
                decimals_ = decimals
            end if
            
            call to_text(x=x, file_name=file_name, header=header_, delim=delim_, fmt=fmt_, decimals=decimals_)
        else if ( any(binary_ext == ext) ) then
            if ( present(header) )   write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(delim) )    write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'
            if ( present(fmt) )      write(*,'(a)') nl//'WARNING: fmt not supported for file type "'//ext//'".'
            if ( present(decimals) ) write(*,'(a)') nl//'WARNING: decimals not supported for file type "'//ext//'".'

            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_2dr32

    module procedure to_file_3dr64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            call to_binary(x=x, file_name=file_name)
        else
            if ( any(text_ext == ext) ) then
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
                                    'dimension ('//str(rank(x))//') to text.'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            else
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                    'due to unsupported file extension "'//ext//'".'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure to_file_3dr64
    module procedure to_file_3dr32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            call to_binary(x=x, file_name=file_name)
        else
            if ( any(text_ext == ext) ) then
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
                                    'dimension ('//str(rank(x))//') to text.'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            else
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                    'due to unsupported file extension "'//ext//'".'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure to_file_3dr32

    module procedure to_file_4dr64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            call to_binary(x=x, file_name=file_name)
        else
            if ( any(text_ext == ext) ) then
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
                                    'dimension ('//str(rank(x))//') to text.'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            else
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                    'due to unsupported file extension "'//ext//'".'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure to_file_4dr64
    module procedure to_file_4dr32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            call to_binary(x=x, file_name=file_name)
        else
            if ( any(text_ext == ext) ) then
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
                                    'dimension ('//str(rank(x))//') to text.'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            else
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                    'due to unsupported file extension "'//ext//'".'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure to_file_4dr32

    module procedure to_file_5dr64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            call to_binary(x=x, file_name=file_name)
        else
            if ( any(text_ext == ext) ) then
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
                                    'dimension ('//str(rank(x))//') to text.'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            else
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                    'due to unsupported file extension "'//ext//'".'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure to_file_5dr64
    module procedure to_file_5dr32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            call to_binary(x=x, file_name=file_name)
        else
            if ( any(text_ext == ext) ) then
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
                                    'dimension ('//str(rank(x))//') to text.'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            else
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                    'due to unsupported file extension "'//ext//'".'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure to_file_5dr32

    module procedure to_file_1di64
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_
        integer :: hstat, dim_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
                hstat = 0
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
                    header_ = ['']
                    hstat = -1
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x))//').'
                else
                    header_ = header
                    if ( size(header) == 1 ) then
                        hstat = 1
                    else
                        hstat = 2
                    end if
                end if
            end if

            if ( .not. present(dim) ) then
                if ( hstat == 2 ) then
                    dim_ = 2
                else
                    dim_ = 1
                end if
            else
                if ( hstat == 2 ) then
                    dim_ = 2
                    if ( dim /= 2 ) then
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (2).'
                    end if
                else
                    if ( dim == 1 ) then
                        dim_ = 1
                    else if ( dim == 2 ) then
                        dim_ = 2
                    else
                        dim_ = 1
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (1).'
                    end if
                end if
            end if

            if ( .not. present(delim) ) then
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = ','
                end if
            else
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = delim
                end if
            end if
            
            call to_text(x=x, file_name=file_name, header=header_, dim=dim_, delim=delim_)
        else if ( any(binary_ext == ext) ) then
            if ( present(header) ) write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(dim) )    write(*,'(a)') nl//'WARNING: dim not supported for file type "'//ext//'".'
            if ( present(delim) )  write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'

            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_1di64
    module procedure to_file_1di32
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_
        integer :: hstat, dim_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
                hstat = 0
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
                    header_ = ['']
                    hstat = -1
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x))//').'
                else
                    header_ = header
                    if ( size(header) == 1 ) then
                        hstat = 1
                    else
                        hstat = 2
                    end if
                end if
            end if

            if ( .not. present(dim) ) then
                if ( hstat == 2 ) then
                    dim_ = 2
                else
                    dim_ = 1
                end if
            else
                if ( hstat == 2 ) then
                    dim_ = 2
                    if ( dim /= 2 ) then
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (2).'
                    end if
                else
                    if ( dim == 1 ) then
                        dim_ = 1
                    else if ( dim == 2 ) then
                        dim_ = 2
                    else
                        dim_ = 1
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (1).'
                    end if
                end if
            end if

            if ( .not. present(delim) ) then
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = ','
                end if
            else
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = delim
                end if
            end if
            
            call to_text(x=x, file_name=file_name, header=header_, dim=dim_, delim=delim_)
        else if ( any(binary_ext == ext) ) then
            if ( present(header) ) write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(dim) )    write(*,'(a)') nl//'WARNING: dim not supported for file type "'//ext//'".'
            if ( present(delim) )  write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'

            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_1di32
    module procedure to_file_1di16
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_
        integer :: hstat, dim_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
                hstat = 0
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
                    header_ = ['']
                    hstat = -1
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x))//').'
                else
                    header_ = header
                    if ( size(header) == 1 ) then
                        hstat = 1
                    else
                        hstat = 2
                    end if
                end if
            end if

            if ( .not. present(dim) ) then
                if ( hstat == 2 ) then
                    dim_ = 2
                else
                    dim_ = 1
                end if
            else
                if ( hstat == 2 ) then
                    dim_ = 2
                    if ( dim /= 2 ) then
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (2).'
                    end if
                else
                    if ( dim == 1 ) then
                        dim_ = 1
                    else if ( dim == 2 ) then
                        dim_ = 2
                    else
                        dim_ = 1
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (1).'
                    end if
                end if
            end if

            if ( .not. present(delim) ) then
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = ','
                end if
            else
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = delim
                end if
            end if
            
            call to_text(x=x, file_name=file_name, header=header_, dim=dim_, delim=delim_)
        else if ( any(binary_ext == ext) ) then
            if ( present(header) ) write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(dim) )    write(*,'(a)') nl//'WARNING: dim not supported for file type "'//ext//'".'
            if ( present(delim) )  write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'

            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_1di16
    module procedure to_file_1di8
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_
        integer :: hstat, dim_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
                hstat = 0
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x)) ) then
                    header_ = ['']
                    hstat = -1
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x))//').'
                else
                    header_ = header
                    if ( size(header) == 1 ) then
                        hstat = 1
                    else
                        hstat = 2
                    end if
                end if
            end if

            if ( .not. present(dim) ) then
                if ( hstat == 2 ) then
                    dim_ = 2
                else
                    dim_ = 1
                end if
            else
                if ( hstat == 2 ) then
                    dim_ = 2
                    if ( dim /= 2 ) then
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (2).'
                    end if
                else
                    if ( dim == 1 ) then
                        dim_ = 1
                    else if ( dim == 2 ) then
                        dim_ = 2
                    else
                        dim_ = 1
                        write(*,'(a)') nl//'WARNING: Invalid dim ('//str(dim)//') in write to file "'// &
                                       file_name//'" for given header... defaulting to (1).'
                    end if
                end if
            end if

            if ( .not. present(delim) ) then
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = ','
                end if
            else
                if ( dim_ == 1 ) then
                    delim_ = ''
                else
                    delim_ = delim
                end if
            end if
            
            call to_text(x=x, file_name=file_name, header=header_, dim=dim_, delim=delim_)
        else if ( any(binary_ext == ext) ) then
            if ( present(header) ) write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(dim) )    write(*,'(a)') nl//'WARNING: dim not supported for file type "'//ext//'".'
            if ( present(delim) )  write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'

            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_1di8

    module procedure to_file_2di64
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
                    header_ = ['']
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x, dim=2))//').'
                else
                    header_ = header
                end if
            end if

            if ( .not. present(delim) ) then
                delim_ = ','
            else
                delim_ = delim
            end if
            
            call to_text(x=x, file_name=file_name, header=header_, delim=delim_)
        else if ( any(binary_ext == ext) ) then
            if ( present(header) ) write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(delim) )  write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'

            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_2di64
    module procedure to_file_2di32
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
                    header_ = ['']
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x, dim=2))//').'
                else
                    header_ = header
                end if
            end if

            if ( .not. present(delim) ) then
                delim_ = ','
            else
                delim_ = delim
            end if
            
            call to_text(x=x, file_name=file_name, header=header_, delim=delim_)
        else if ( any(binary_ext == ext) ) then
            if ( present(header) ) write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(delim) )  write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'

            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_2di32
    module procedure to_file_2di16
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
                    header_ = ['']
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x, dim=2))//').'
                else
                    header_ = header
                end if
            end if

            if ( .not. present(delim) ) then
                delim_ = ','
            else
                delim_ = delim
            end if
            
            call to_text(x=x, file_name=file_name, header=header_, delim=delim_)
        else if ( any(binary_ext == ext) ) then
            if ( present(header) ) write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(delim) )  write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'

            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_2di16
    module procedure to_file_2di8
        character(len=:), allocatable :: ext, delim_
        character(len=:), allocatable, dimension(:) :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = ['']
            else
                if ( (size(header) /= 1) .and. (size(header) /= size(x, dim=2)) ) then
                    header_ = ['']
                    write(*,'(a)') nl//'WARNING: Invalid header for file "'//file_name//'".'// &
                                   nl//'Header for this data must have size (1) or '// & 
                                       '('//str(size(x, dim=2))//').'
                else
                    header_ = header
                end if
            end if

            if ( .not. present(delim) ) then
                delim_ = ','
            else
                delim_ = delim
            end if
            
            call to_text(x=x, file_name=file_name, header=header_, delim=delim_)
        else if ( any(binary_ext == ext) ) then
            if ( present(header) ) write(*,'(a)') nl//'WARNING: header not supported for file type "'//ext//'".'
            if ( present(delim) )  write(*,'(a)') nl//'WARNING: delim not supported for file type "'//ext//'".'

            call to_binary(x=x, file_name=file_name)
        else
            write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                'due to unsupported file extension "'//ext//'".'// &
                            nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                                to_str(binary_ext, delim=' ')
        end if
    end procedure to_file_2di8

    module procedure to_file_3di64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            call to_binary(x=x, file_name=file_name)
        else
            if ( any(text_ext == ext) ) then
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
                                    'dimension ('//str(rank(x))//') to text.'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            else
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                    'due to unsupported file extension "'//ext//'".'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure to_file_3di64
    module procedure to_file_3di32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            call to_binary(x=x, file_name=file_name)
        else
            if ( any(text_ext == ext) ) then
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
                                    'dimension ('//str(rank(x))//') to text.'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            else
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                    'due to unsupported file extension "'//ext//'".'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure to_file_3di32
    module procedure to_file_3di16
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            call to_binary(x=x, file_name=file_name)
        else
            if ( any(text_ext == ext) ) then
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
                                    'dimension ('//str(rank(x))//') to text.'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            else
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                    'due to unsupported file extension "'//ext//'".'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure to_file_3di16
    module procedure to_file_3di8
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            call to_binary(x=x, file_name=file_name)
        else
            if ( any(text_ext == ext) ) then
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'". Cannot write array of '// &
                                    'dimension ('//str(rank(x))//') to text.'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            else
                write(*,'(a)')  nl//'WARNING: Skipping write to "'//file_name//'" '// &
                                    'due to unsupported file extension "'//ext//'".'// &
                                nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure to_file_3di8

    !! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure from_textfile_1dr64
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_1dr64
    module procedure from_binaryfile_1dr64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_1dr64
    module procedure from_textfile_1dr32
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_1dr32
    module procedure from_binaryfile_1dr32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_1dr32

    module procedure from_textfile_2dr64
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_2dr64
    module procedure from_binaryfile_2dr64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_2dr64
    module procedure from_textfile_2dr32
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_2dr32
    module procedure from_binaryfile_2dr32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_2dr32

    module procedure from_file_3dr64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
                               'arrays of dimension greater than (2).'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_file_3dr64
    module procedure from_file_3dr32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
                               'arrays of dimension greater than (2).'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_file_3dr32

    module procedure from_file_4dr64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
                               'arrays of dimension greater than (2).'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_file_4dr64
    module procedure from_file_4dr32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
                               'arrays of dimension greater than (2).'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_file_4dr32

    module procedure from_file_5dr64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
                               'arrays of dimension greater than (2).'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_file_5dr64
    module procedure from_file_5dr32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
                               'arrays of dimension greater than (2).'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_file_5dr32

    module procedure from_textfile_1di64
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_1di64
    module procedure from_binaryfile_1di64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_1di64
    module procedure from_textfile_1di32
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_1di32
    module procedure from_binaryfile_1di32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_1di32
    module procedure from_textfile_1di16
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_1di16
    module procedure from_binaryfile_1di16
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_1di16
    module procedure from_textfile_1di8
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_1di8
    module procedure from_binaryfile_1di8
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_1di8

    module procedure from_textfile_2di64
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_2di64
    module procedure from_binaryfile_2di64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_2di64
    module procedure from_textfile_2di32
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_2di32
    module procedure from_binaryfile_2di32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_2di32
    module procedure from_textfile_2di16
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_2di16
    module procedure from_binaryfile_2di16
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_2di16
    module procedure from_textfile_2di8
        character(len=:), allocatable :: ext
        character(len=2) :: locale_
        logical :: header_

        ext = ext_of(file_name)

        if ( any(text_ext == ext) ) then
            if ( .not. present(header) ) then
                header_ = .false.
            else
                header_ = header
            end if

            if ( .not. present(locale) ) then
                locale_ = 'us'
            else
                if ( (locale == 'us') .or. (locale == 'eu') ) then
                    locale_ = locale
                else
                    locale_ = 'us'
                    write(*,'(a)') nl//'WARNING: Invalid locale "'//locale//'" for file "'//file_name//'".'// &
                                   nl//'Locale must be "us" or "eu"... defaulting to "us".'
                end if
            end if
            
            call from_text(file_name=file_name, into=into, header=header_, locale=locale_)
        else
            if ( any(binary_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must be specified '// &
                               'for binary data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_textfile_2di8
    module procedure from_binaryfile_2di8
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'", data_shape must not be specified '// &
                               'for textual data.'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(text_ext, delim=' ')//' '// &
                           to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_binaryfile_2di8

    module procedure from_file_3di64
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
                               'arrays of dimension greater than (2).'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_file_3di64
    module procedure from_file_3di32
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
                               'arrays of dimension greater than (2).'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_file_3di32
    module procedure from_file_3di16
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
                               'arrays of dimension greater than (2).'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_file_3di16
    module procedure from_file_3di8
        character(len=:), allocatable :: ext

        ext = ext_of(file_name)

        if ( any(binary_ext == ext) ) then
            if ( size(data_shape) /= rank(into) ) then
                error stop nl//'FATAL: Shape mismatch in read of file "'//file_name//'".'// &
                           nl//'Output array has dimension ('//str(rank(into))//') while data_shape has size (' &
                             //str(size(data_shape))//'). These must match.'
            end if

            call from_binary(file_name=file_name, into=into, data_shape=data_shape)
        else
            if ( any(text_ext == ext) ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". Textual data cannot be read into '// &
                               'arrays of dimension greater than (2).'
            else
                error stop nl//'FATAL: Unsupported file extension "'//ext//'" for file "'//file_name//'".'// &
                           nl//'Supported file extensions: '//to_str(binary_ext, delim=' ')
            end if
        end if
    end procedure from_file_3di8
end submodule file_io

submodule (io_mod) text_io
    contains
    !! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure echo_string
        logical :: exists
        integer :: file_unit

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

    module procedure to_text_1dr64
        logical :: exists
        integer :: file_unit, i
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            if ( dim == 1 ) then
                write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))
            else if ( dim == 2 ) then
                label = trim(adjustl(header(1)))
                do i = lbound(x, dim=1), ubound(x, dim=1) - 1
                    write(unit=file_unit, fmt='(a)', advance='no') label//str(i)//delim
                end do
                write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=1))
            end if
        else if ( size(header) == size(x) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        if ( dim == 1 ) then
            do i = lbound(x, dim=1), ubound(x, dim=1)
                write(unit=file_unit, fmt='(a)') str(x(i), fmt=fmt, decimals=decimals)
            end do
        else if ( dim == 2 ) then
            write(unit=file_unit, fmt='(a)') to_str(x, delim=delim, fmt=fmt, decimals=decimals)
        end if

        close(file_unit)
    end procedure to_text_1dr64
    module procedure to_text_1dr32
        logical :: exists
        integer :: file_unit, i
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            if ( dim == 1 ) then
                write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))
            else if ( dim == 2 ) then
                label = trim(adjustl(header(1)))
                do i = lbound(x, dim=1), ubound(x, dim=1) - 1
                    write(unit=file_unit, fmt='(a)', advance='no') label//str(i)//delim
                end do
                write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=1))
            end if
        else if ( size(header) == size(x) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        if ( dim == 1 ) then
            do i = lbound(x, dim=1), ubound(x, dim=1)
                write(unit=file_unit, fmt='(a)') str(x(i), fmt=fmt, decimals=decimals)
            end do
        else if ( dim == 2 ) then
            write(unit=file_unit, fmt='(a)') to_str(x, delim=delim, fmt=fmt, decimals=decimals)
        end if

        close(file_unit)
    end procedure to_text_1dr32

    module procedure to_text_2dr64
        logical :: exists
        integer :: file_unit, i, j
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            label = trim(adjustl(header(1)))
            do j = lbound(x, dim=2), ubound(x, dim=2) - 1
                write(unit=file_unit, fmt='(a)', advance='no') label//str(j)//delim
            end do
            write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=2))
        else if ( size(header) == size(x, dim=2) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') to_str(x(i,:), delim=delim, fmt=fmt, decimals=decimals)
        end do

        close(file_unit)
    end procedure to_text_2dr64
    module procedure to_text_2dr32
        logical :: exists
        integer :: file_unit, i, j
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            label = trim(adjustl(header(1)))
            do j = lbound(x, dim=2), ubound(x, dim=2) - 1
                write(unit=file_unit, fmt='(a)', advance='no') label//str(j)//delim
            end do
            write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=2))
        else if ( size(header) == size(x, dim=2) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') to_str(x(i,:), delim=delim, fmt=fmt, decimals=decimals)
        end do

        close(file_unit)
    end procedure to_text_2dr32

    module procedure to_text_1di64
        logical :: exists
        integer :: file_unit, i
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            if ( dim == 1 ) then
                write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))
            else if ( dim == 2 ) then
                label = trim(adjustl(header(1)))
                do i = lbound(x, dim=1), ubound(x, dim=1) - 1
                    write(unit=file_unit, fmt='(a)', advance='no') label//str(i)//delim
                end do
                write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=1))
            end if
        else if ( size(header) == size(x) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        if ( dim == 1 ) then
            do i = lbound(x, dim=1), ubound(x, dim=1)
                write(unit=file_unit, fmt='(a)') str(x(i))
            end do
        else if ( dim == 2 ) then
            write(unit=file_unit, fmt='(a)') to_str(x, delim=delim)
        end if

        close(file_unit)
    end procedure to_text_1di64
    module procedure to_text_1di32
        logical :: exists
        integer :: file_unit, i
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            if ( dim == 1 ) then
                write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))
            else if ( dim == 2 ) then
                label = trim(adjustl(header(1)))
                do i = lbound(x, dim=1), ubound(x, dim=1) - 1
                    write(unit=file_unit, fmt='(a)', advance='no') label//str(i)//delim
                end do
                write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=1))
            end if
        else if ( size(header) == size(x) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        if ( dim == 1 ) then
            do i = lbound(x, dim=1), ubound(x, dim=1)
                write(unit=file_unit, fmt='(a)') str(x(i))
            end do
        else if ( dim == 2 ) then
            write(unit=file_unit, fmt='(a)') to_str(x, delim=delim)
        end if

        close(file_unit)
    end procedure to_text_1di32
    module procedure to_text_1di16
        logical :: exists
        integer :: file_unit, i
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            if ( dim == 1 ) then
                write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))
            else if ( dim == 2 ) then
                label = trim(adjustl(header(1)))
                do i = lbound(x, dim=1), ubound(x, dim=1) - 1
                    write(unit=file_unit, fmt='(a)', advance='no') label//str(i)//delim
                end do
                write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=1))
            end if
        else if ( size(header) == size(x) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        if ( dim == 1 ) then
            do i = lbound(x, dim=1), ubound(x, dim=1)
                write(unit=file_unit, fmt='(a)') str(x(i))
            end do
        else if ( dim == 2 ) then
            write(unit=file_unit, fmt='(a)') to_str(x, delim=delim)
        end if

        close(file_unit)
    end procedure to_text_1di16
    module procedure to_text_1di8
        logical :: exists
        integer :: file_unit, i
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            if ( dim == 1 ) then
                write(unit=file_unit, fmt='(a)') trim(adjustl(header(1)))
            else if ( dim == 2 ) then
                label = trim(adjustl(header(1)))
                do i = lbound(x, dim=1), ubound(x, dim=1) - 1
                    write(unit=file_unit, fmt='(a)', advance='no') label//str(i)//delim
                end do
                write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=1))
            end if
        else if ( size(header) == size(x) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        if ( dim == 1 ) then
            do i = lbound(x, dim=1), ubound(x, dim=1)
                write(unit=file_unit, fmt='(a)') str(x(i))
            end do
        else if ( dim == 2 ) then
            write(unit=file_unit, fmt='(a)') to_str(x, delim=delim)
        end if

        close(file_unit)
    end procedure to_text_1di8

    module procedure to_text_2di64
        logical :: exists
        integer :: file_unit, i, j
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            label = trim(adjustl(header(1)))
            do j = lbound(x, dim=2), ubound(x, dim=2) - 1
                write(unit=file_unit, fmt='(a)', advance='no') label//str(j)//delim
            end do
            write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=2))
        else if ( size(header) == size(x, dim=2) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') to_str(x(i,:), delim=delim)
        end do

        close(file_unit)
    end procedure to_text_2di64
    module procedure to_text_2di32
        logical :: exists
        integer :: file_unit, i, j
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            label = trim(adjustl(header(1)))
            do j = lbound(x, dim=2), ubound(x, dim=2) - 1
                write(unit=file_unit, fmt='(a)', advance='no') label//str(j)//delim
            end do
            write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=2))
        else if ( size(header) == size(x, dim=2) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') to_str(x(i,:), delim=delim)
        end do

        close(file_unit)
    end procedure to_text_2di32
    module procedure to_text_2di16
        logical :: exists
        integer :: file_unit, i, j
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            label = trim(adjustl(header(1)))
            do j = lbound(x, dim=2), ubound(x, dim=2) - 1
                write(unit=file_unit, fmt='(a)', advance='no') label//str(j)//delim
            end do
            write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=2))
        else if ( size(header) == size(x, dim=2) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') to_str(x(i,:), delim=delim)
        end do

        close(file_unit)
    end procedure to_text_2di16
    module procedure to_text_2di8
        logical :: exists
        integer :: file_unit, i, j
        character(len=:), allocatable :: label

        inquire( file=file_name, exist=exists )

        file_unit = output_unit

        if ( .not. exists ) then
            open( newunit=file_unit, file=file_name, status='new', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        else
            open( newunit=file_unit, file=file_name, status='replace', form='formatted', &
                  action='write', access='sequential', position='rewind' )
        end if

        if ( all(header == '') ) then
            continue
        else if ( size(header) == 1 ) then
            label = trim(adjustl(header(1)))
            do j = lbound(x, dim=2), ubound(x, dim=2) - 1
                write(unit=file_unit, fmt='(a)', advance='no') label//str(j)//delim
            end do
            write(unit=file_unit, fmt='(a)') label//str(ubound(x, dim=2))
        else if ( size(header) == size(x, dim=2) ) then
            write(unit=file_unit, fmt='(a)') to_str(header, delim=delim)
        end if

        do i = lbound(x, dim=1), ubound(x, dim=1)
            write(unit=file_unit, fmt='(a)') to_str(x(i,:), delim=delim)
        end do

        close(file_unit)
    end procedure to_text_2di8

    !! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure from_text_1dr64
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, ind, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        if ( (n_rows > 1) .and. (n_columns > 1) ) then
            error stop nl//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
            return
        else if ( n_columns == 1 ) then
            allocate( into(n_rows) )
        else if ( n_rows == 1 ) then
            allocate( into(n_columns) )
        end if
    
        ind = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(ind)
                    if ( ind /= size(into) ) then
                        ind = ind + 1
                    else
                        exit read_into
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_1dr64
    module procedure from_text_1dr32
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, ind, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        if ( (n_rows > 1) .and. (n_columns > 1) ) then
            error stop nl//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
            return
        else if ( n_columns == 1 ) then
            allocate( into(n_rows) )
        else if ( n_rows == 1 ) then
            allocate( into(n_columns) )
        end if
    
        ind = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(ind)
                    if ( ind /= size(into) ) then
                        ind = ind + 1
                    else
                        exit read_into
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_1dr32

    module procedure from_text_2dr64
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, row, column, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        allocate( into(n_rows, n_columns) )
    
        row = 1
        column = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(row, column)
                    if ( column /= n_columns ) then
                        column = column + 1
                    else
                        if ( row /= n_rows ) then
                            row = row + 1
                            column = 1
                        else
                            exit read_into
                        end if
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_2dr64
    module procedure from_text_2dr32
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, row, column, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        allocate( into(n_rows, n_columns) )
    
        row = 1
        column = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(row, column)
                    if ( column /= n_columns ) then
                        column = column + 1
                    else
                        if ( row /= n_rows ) then
                            row = row + 1
                            column = 1
                        else
                            exit read_into
                        end if
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_2dr32

    module procedure from_text_1di64
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, ind, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        if ( (n_rows > 1) .and. (n_columns > 1) ) then
            error stop nl//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
            return
        else if ( n_columns == 1 ) then
            allocate( into(n_rows) )
        else if ( n_rows == 1 ) then
            allocate( into(n_columns) )
        end if
    
        ind = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(ind)
                    if ( ind /= size(into) ) then
                        ind = ind + 1
                    else
                        exit read_into
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_1di64
    module procedure from_text_1di32
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, ind, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        if ( (n_rows > 1) .and. (n_columns > 1) ) then
            error stop nl//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
            return
        else if ( n_columns == 1 ) then
            allocate( into(n_rows) )
        else if ( n_rows == 1 ) then
            allocate( into(n_columns) )
        end if
    
        ind = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(ind)
                    if ( ind /= size(into) ) then
                        ind = ind + 1
                    else
                        exit read_into
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_1di32
    module procedure from_text_1di16
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, ind, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        if ( (n_rows > 1) .and. (n_columns > 1) ) then
            error stop nl//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
            return
        else if ( n_columns == 1 ) then
            allocate( into(n_rows) )
        else if ( n_rows == 1 ) then
            allocate( into(n_columns) )
        end if
    
        ind = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(ind)
                    if ( ind /= size(into) ) then
                        ind = ind + 1
                    else
                        exit read_into
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_1di16
    module procedure from_text_1di8
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, ind, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        if ( (n_rows > 1) .and. (n_columns > 1) ) then
            error stop nl//'Error reading file "'//file_name//'". File data cannot fit into one-dimensional array.'
            return
        else if ( n_columns == 1 ) then
            allocate( into(n_rows) )
        else if ( n_rows == 1 ) then
            allocate( into(n_columns) )
        end if
    
        ind = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(ind)
                    if ( ind /= size(into) ) then
                        ind = ind + 1
                    else
                        exit read_into
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_1di8

    module procedure from_text_2di64
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, row, column, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        allocate( into(n_rows, n_columns) )
    
        row = 1
        column = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(row, column)
                    if ( column /= n_columns ) then
                        column = column + 1
                    else
                        if ( row /= n_rows ) then
                            row = row + 1
                            column = 1
                        else
                            exit read_into
                        end if
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_2di64
    module procedure from_text_2di32
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, row, column, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        allocate( into(n_rows, n_columns) )
    
        row = 1
        column = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(row, column)
                    if ( column /= n_columns ) then
                        column = column + 1
                    else
                        if ( row /= n_rows ) then
                            row = row + 1
                            column = 1
                        else
                            exit read_into
                        end if
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_2di32
    module procedure from_text_2di16
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, row, column, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        allocate( into(n_rows, n_columns) )
    
        row = 1
        column = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(row, column)
                    if ( column /= n_columns ) then
                        column = column + 1
                    else
                        if ( row /= n_rows ) then
                            row = row + 1
                            column = 1
                        else
                            exit read_into
                        end if
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_2di16
    module procedure from_text_2di8
        logical :: exists
        integer :: file_unit, iostat
        integer :: n_rows, n_columns, file_length
        integer :: i, row, column, l1, l2

        character(len=:), allocatable, dimension(:) :: non_separating_chars
        character(len=:), allocatable :: file, decimal
        character(len=1) :: prev_char, current_char

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        inquire( file=file_name, size=file_length )

        if ( file_length == 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty.'
            return
        end if

        allocate( character(len=file_length) :: file )
        read(unit=file_unit, iostat=iostat) file
        close(file_unit)

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        if ( header ) then
            do i = 1, file_length
                if ( file(i:i) == nl ) then
                    file = file(i+1:)
                    file_length = len(file)
                    exit
                else if ( i == file_length ) then
                    file = file//nl
                    file_length = file_length + 1
                    write(*,'(a)') 'WARNING: Ignoring erroneous value of (T) for header in read of file "'// &
                                    file_name//'". File has one line.'
                end if
            end do

            if ( file_length == 0 ) then
                error stop nl//'FATAL: Error reading file "'//file_name//'". File is empty after header.'
                return
            end if
        end if

        n_rows = 0

        do i = 1, file_length
            if ( file(i:i) == nl ) then
                n_rows = n_rows + 1
            else if ( i == file_length ) then
                file = file//nl
                file_length = file_length + 1
                n_rows = n_rows + 1
            end if
        end do

        if ( locale == 'us' ) then
            non_separating_chars = non_separating_chars_us
            decimal = 'POINT'
        else
            non_separating_chars = non_separating_chars_eu
            decimal = 'COMMA'
        end if

        prev_char = '0'
        n_columns = 0

        do i = 1, file_length
            current_char = file(i:i)

            if ( any(non_separating_chars == current_char) .or. any(letters == current_char) ) then
                prev_char = current_char
            else
                if ( any(non_separating_chars == prev_char) .or. any(letters == prev_char) ) then
                    prev_char = current_char
                    n_columns = n_columns + 1
                else
                    prev_char = current_char
                end if
            end if

            if ( current_char == nl ) exit
        end do

        allocate( into(n_rows, n_columns) )
    
        row = 1
        column = 1
        l1 = 1
        l2 = 1

        read_into: do i = 1, file_length
            if ( any(non_separating_chars == file(i:i)) ) then
                if ( .not. any(non_separating_chars == file(l2:l2)) ) l1 = i
                l2 = i
            else
                if ( any(non_separating_chars == file(l2:l2) ) ) then
                    read(unit=file(l1:l2), fmt=*, decimal=decimal) into(row, column)
                    if ( column /= n_columns ) then
                        column = column + 1
                    else
                        if ( row /= n_rows ) then
                            row = row + 1
                            column = 1
                        else
                            exit read_into
                        end if
                    end if
                    l2 = i
                else
                    l2 = i
                end if
            end if
        end do read_into
    end procedure from_text_2di8
end submodule text_io

submodule (io_mod) binary_io
    contains
    !! Writing Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure to_binary_1dr64
        logical :: exists
        integer :: file_unit

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
        logical :: exists
        integer :: file_unit

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
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_2dr64
    module procedure to_binary_2dr32
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_2dr32

    module procedure to_binary_3dr64
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_3dr64
    module procedure to_binary_3dr32
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_3dr32

    module procedure to_binary_4dr64
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_4dr64
    module procedure to_binary_4dr32
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_4dr32

    module procedure to_binary_5dr64
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_5dr64
    module procedure to_binary_5dr32
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_5dr32

    module procedure to_binary_1di64
        logical :: exists
        integer :: file_unit

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
        logical :: exists
        integer :: file_unit

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
        logical :: exists
        integer :: file_unit

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
        logical :: exists
        integer :: file_unit

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
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_2di64
    module procedure to_binary_2di32
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_2di32
    module procedure to_binary_2di16
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_2di16
    module procedure to_binary_2di8
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_2di8

    module procedure to_binary_3di64
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_3di64
    module procedure to_binary_3di32
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_3di32
    module procedure to_binary_3di16
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_3di16
    module procedure to_binary_3di8
        logical :: exists
        integer :: file_unit

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
    end procedure to_binary_3di8

    !! Reading Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    module procedure from_binary_1dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_1dr64
    module procedure from_binary_1dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_1dr32

    module procedure from_binary_2dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_2dr64
    module procedure from_binary_2dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_2dr32

    module procedure from_binary_3dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_3dr64
    module procedure from_binary_3dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_3dr32

    module procedure from_binary_4dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_4dr64
    module procedure from_binary_4dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_4dr32

    module procedure from_binary_5dr64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_5dr64
    module procedure from_binary_5dr32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_5dr32

    module procedure from_binary_1di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_1di64
    module procedure from_binary_1di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_1di32
    module procedure from_binary_1di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_1di16
    module procedure from_binary_1di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_1di8

    module procedure from_binary_2di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_2di64
    module procedure from_binary_2di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_2di32
    module procedure from_binary_2di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_2di16
    module procedure from_binary_2di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_2di8

    module procedure from_binary_3di64
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_3di64
    module procedure from_binary_3di32
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_3di32
    module procedure from_binary_3di16
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_3di16
    module procedure from_binary_3di8
        logical :: exists
        integer :: file_unit, iostat

        inquire( file=file_name, exist=exists )

        file_unit = input_unit

        if ( exists ) then
            open( newunit=file_unit, file=file_name, status='old', form='unformatted', &
                  action='read', access='stream', position='rewind' )
        else
            error stop nl//'FATAL: Error reading file "'//file_name//'". No such file exists.'
            return
        end if

        allocate( into(data_shape(1), data_shape(2), data_shape(3)) )
        read(unit=file_unit, iostat=iostat) into

        if ( iostat > 0 ) then
            error stop nl//'FATAL: Error reading file "'//file_name//'".'
            return
        end if

        close(file_unit)
    end procedure from_binary_3di8
end submodule binary_io
