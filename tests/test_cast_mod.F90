! Copyright (C) 2017-2018  Elias Rabel
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as published by 
! the Free Software Foundation, either version 3 of the License, or 
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of 
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module test_cast_mod
use unittest_mod
use forpy_mod
use forpy_tests_common_mod, only: setUp_forpy_test, tearDown_forpy_test, gettotalrefcount
use iso_fortran_env
use iso_c_binding
implicit none

type(module_py), save :: test_mod

CONTAINS

#include "unittest_mod.inc"

subroutine test_cast_tuple_to_list
  integer ierror
  type(tuple) :: tu
  type(list) :: li
  logical :: exc_correct
  ierror = tuple_create(tu, 0)
  ASSERT(ierror==0)
  ierror = cast(li, tu)
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(TypeError)
  ASSERT(exc_correct)
  call err_clear
  call tu%destroy
  call li%destroy
end subroutine

subroutine test_cast_float_to_int
  integer ierror
  type(object) :: fl
  integer :: num
  logical :: exc_correct
  ierror = cast(fl, 3.14)
  ASSERT(ierror==0)
  ierror = cast(num, fl)
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(TypeError)
  ASSERT(exc_correct)
  call err_clear
  call fl%destroy
end subroutine

subroutine test_cast_float_to_logical
  integer ierror
  type(object) :: fl
  logical :: flag
  logical :: exc_correct
  ierror = cast(fl, 3.14)
  ASSERT(ierror==0)
  ierror = cast(flag, fl)
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(TypeError)
  ASSERT(exc_correct)
  call err_clear
  call fl%destroy
end subroutine

subroutine test_cast_int_to_float
  integer ierror
  type(object) :: ii
  real :: num
  logical :: exc_correct
  ierror = cast(ii, 101)
  ASSERT(ierror==0)
  ierror = cast(num, ii)
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(TypeError)
  ASSERT(exc_correct)
  call err_clear
  call ii%destroy
end subroutine

subroutine test_cast_bool_to_int
  integer ierror
  type(object) :: ii
  integer :: num
  ierror = cast(ii, .true.)
  ASSERT(ierror==0)
  ierror = cast(num, ii)
  ! This works, because in Python bool is a subtype of int
  ASSERT(ierror==0)
  call ii%destroy
end subroutine

subroutine test_cast_int_to_logical
  integer ierror
  type(object) :: ii
  logical :: flag
  logical :: exc_correct
  ierror = cast(ii, 101)
  ASSERT(ierror==0)
  ierror = cast(flag, ii)
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(TypeError)
  ASSERT(exc_correct)
  call err_clear
  call ii%destroy
end subroutine

subroutine test_cast_nonstrict_real
  integer ierror
  type(object) :: a_float
  integer :: f_int
  complex(kind=real64) :: f_complex
  logical :: f_logical
  real(kind=real64), parameter :: test_real = 3.14159_real64
  ierror = cast(a_float, test_real)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(f_int, a_float)
  ASSERT(ierror==0)
  ASSERT(f_int==3)
  ierror = cast_nonstrict(f_complex, a_float)
  ASSERT(ierror==0)
  ASSERT(f_complex==test_real)
  ierror = cast_nonstrict(f_logical, a_float)
  ASSERT(ierror==0)
  ASSERT(f_logical)
  call a_float%destroy
end subroutine

subroutine test_cast_nonstrict_complex
  integer ierror
  type(object) :: a_number
  integer(kind=int64) :: f_int
  real(kind=real64) :: f_real
  logical :: f_logical
  complex(kind=real64), parameter :: test_complex = (3.14159_real64, -6.28_real64)
  ierror = cast(a_number, test_complex)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(f_int, a_number)
  ASSERT(ierror==EXCEPTION_ERROR)
  call err_clear
  ierror = cast_nonstrict(f_real, a_number)
  ASSERT(ierror==EXCEPTION_ERROR)
  call err_clear
  ierror = cast_nonstrict(f_logical, a_number)
  ASSERT(ierror==0)
  ASSERT(f_logical)
  call a_number%destroy
end subroutine

subroutine test_cast_nonstrict_int
  integer ierror
  type(object) :: a_number
  complex(kind=real32) :: f_complex
  real(kind=real64) :: f_real
  logical :: f_logical
  integer(kind=int64), parameter :: test_int = 1234
  real(kind=real64) :: test_real
  complex(kind=real32) :: test_complex
  test_real = real(test_int, kind=real64)
  test_complex = cmplx(test_int, kind=real32)  
  ierror = cast(a_number, test_int)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(f_complex, a_number)
  ASSERT(ierror==0)
  ASSERT(f_complex==test_complex)
  ierror = cast_nonstrict(f_real, a_number)
  ASSERT(ierror==0)
  ASSERT(f_real==test_real)
  ierror = cast_nonstrict(f_logical, a_number)
  ASSERT(ierror==0)
  ASSERT(f_logical)
  call a_number%destroy
end subroutine

subroutine test_cast_nonstrict_list
  integer ierror
  type(list) :: a_list
  integer :: f_int
  complex(kind=real64) :: f_complex
  real(kind=real64) :: f_real
  logical :: f_logical
  ierror = list_create(a_list)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(f_int, a_list)
  ASSERT(ierror==EXCEPTION_ERROR)
  call err_clear
  ierror = cast_nonstrict(f_complex, a_list)
  ASSERT(ierror==EXCEPTION_ERROR)
  call err_clear
  ierror = cast_nonstrict(f_real, a_list)
  ASSERT(ierror==EXCEPTION_ERROR)
  call err_clear
  ierror = cast_nonstrict(f_logical, a_list)
  ASSERT(ierror==0)
  ! [] has truth value 'False'
  ASSERT(.not. f_logical)
  call a_list%destroy
end subroutine

subroutine test_cast_nonstrict_numeric()
  ! Testing objects that can be converted to numbers
  ! because they have magic methods, such as __complex__ 
  integer :: ierror
  type(object) :: obj
  complex(kind=C_DOUBLE), parameter :: SOLUTION_COMPLEX = (-12.3_C_DOUBLE, 4.56_C_DOUBLE)
  complex(kind=C_DOUBLE) :: a_complex
  real(kind=C_DOUBLE) :: a_real
  integer(kind=int64) :: a_int
  
  a_complex = 0.0
  ierror = call_py(obj, test_mod, "ConvertibleNumber")
  ASSERT(ierror==0)
  
  ierror = cast_nonstrict(a_complex, obj)
  ASSERT(ierror==0)
  ASSERT(a_complex==SOLUTION_COMPLEX)
  
  ierror = cast_nonstrict(a_real, obj)
  ASSERT(ierror==0)
  ASSERT(a_real==-12.3_C_DOUBLE)
  
  ierror = cast_nonstrict(a_int, obj)
  ASSERT(ierror==0)
  ASSERT(a_int==-12_int64)
  
  call obj%destroy
end subroutine

subroutine test_cast_nonstrict_list_to_list
  integer ierror
  type(list) :: some_list
  type(list) :: li
  ierror = list_create(some_list)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(li, some_list)
  ASSERT(ierror==0)
  call some_list%destroy
  call li%destroy
end subroutine

subroutine test_cast_nonstrict_tuple_to_list
  integer ierror
  type(tuple) :: tu
  type(list) :: li
  ierror = tuple_create(tu, 0)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(li, tu)
  ASSERT(ierror==0)
  ASSERT(is_list(li))
  call tu%destroy
  call li%destroy
end subroutine

subroutine test_cast_nonstrict_int_to_list
  integer ierror
  type(object) :: an_int
  type(list) :: li
  logical :: exc_correct
  ierror = cast(an_int, 345)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(li, an_int)
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(TypeError)
  ASSERT(exc_correct)
  call err_clear
  call an_int%destroy
  call li%destroy
end subroutine

subroutine test_cast_nonstrict_list_to_tuple
  integer ierror
  type(list) :: some_list
  type(tuple) :: tu
  ierror = list_create(some_list)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(tu, some_list)
  ASSERT(ierror==0)
  ASSERT(is_tuple(tu))
  call some_list%destroy
  call tu%destroy
end subroutine

subroutine test_cast_nonstrict_tuple_to_tuple
  integer ierror
  type(tuple) :: some_tuple
  type(tuple) :: tu
  ierror = tuple_create(some_tuple, 0)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(tu, some_tuple)
  ASSERT(ierror==0)
  ASSERT(is_tuple(tu))
  call tu%destroy
  call some_tuple%destroy
end subroutine

subroutine test_cast_nonstrict_int_to_tuple
  integer ierror
  type(object) :: an_int
  type(tuple) :: tu
  logical :: exc_correct
  ierror = cast(an_int, 345)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(tu, an_int)
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(TypeError)
  ASSERT(exc_correct)
  call err_clear
  call an_int%destroy
  call tu%destroy
end subroutine

subroutine test_cast_nonstrict_list_to_chars
  integer ierror
  type(list) :: some_list
  character(kind=C_CHAR, len=:), allocatable :: string
  ierror = list_create(some_list)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(string, some_list)
  ASSERT(ierror==0)
  ASSERT(string=='[]')
  call some_list%destroy
end subroutine

subroutine test_cast_nonstrict_tuple_to_chars
  integer ierror
  type(tuple) :: some_tuple
  character(kind=C_CHAR, len=:), allocatable :: string
  ierror = tuple_create(some_tuple, 0)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(string, some_tuple)
  ASSERT(ierror==0)
  ASSERT(string=='()')
  call some_tuple%destroy
end subroutine

subroutine test_cast_nonstrict_int_to_chars
  integer ierror
  type(object) :: an_int
  character(kind=C_CHAR, len=:), allocatable :: string
  ierror = cast(an_int, 345)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(string, an_int)
  ASSERT(ierror==0)
  ASSERT(string=='345')
  call an_int%destroy
end subroutine

subroutine test_cast_nonstrict_bytes_to_chars
  integer ierror
  type(bytes) :: some_bytes
  character(kind=C_CHAR, len=:), allocatable :: string
  ierror = bytes_create(some_bytes, "abcdefgh")
  ASSERT(ierror==0)
  ierror = cast_nonstrict(string, some_bytes)
  ASSERT(ierror==0)
  ! check that we do not get "b'abcdefgh'" instead
  ASSERT(string=='abcdefgh')
  call some_bytes%destroy
end subroutine

subroutine test_cast_nonstrict_none_to_str
  integer :: ierror
  type(NoneType) :: the_none
  type(str) :: a_str
  character(kind=C_CHAR,len=:), allocatable :: res
  
  ierror = NoneType_create(the_none)
  ASSERT(ierror==0)
  ierror = cast_nonstrict(a_str, the_none)
  ASSERT(ierror==0)
  ierror = cast(res, a_str)
  ASSERT(ierror==0)
  ASSERT(res=='None')
  call the_none%destroy
  call a_str%destroy
end subroutine

subroutine test_cast_bytes_to_chars
  integer ierror
  type(bytes) :: some_bytes
  character(kind=C_CHAR, len=:), allocatable :: string
  ierror = bytes_create(some_bytes, "abcdefgh")
  ASSERT(ierror==0)
  ierror = cast(string, some_bytes)
  ASSERT(ierror==0)
  ASSERT(string=='abcdefgh')
  call some_bytes%destroy
end subroutine

subroutine test_cast_chars_to_object
  integer ierror
  type(object) :: a_string
  character(kind=C_CHAR, len=8), parameter :: fstring = "abcdefgh"
  character(kind=C_CHAR, len=:), allocatable :: res
  
  ierror = cast(a_string, fstring)
  ASSERT(ierror==0)
  ierror = cast(res, a_string)
  ASSERT(ierror==0)
  ASSERT(res==fstring)
  call a_string%destroy
end subroutine

subroutine test_cast_to_str
  integer ierror
  type(object) :: a_string
  type(str) :: str_specific
  character(kind=C_CHAR, len=8), parameter :: fstring = "abcdefgh"
  character(kind=C_CHAR, len=:), allocatable :: res
  
  ierror = cast(a_string, fstring)
  ASSERT(ierror==0)
  ierror = cast(str_specific, a_string)
  ASSERT(ierror==0)
  ierror = cast(res, str_specific)
  ASSERT(ierror==0)
  ASSERT(res==fstring)
  call a_string%destroy
  call str_specific%destroy
end subroutine

subroutine test_cast_to_bytes
  integer ierror
  type(bytes) :: some_bytes
  type(object) :: some_object
  type(bytes) :: bytes_specific
  character(kind=C_CHAR, len=8), parameter :: fstring = "abcdefgh"
  character(kind=C_CHAR, len=:), allocatable :: res
  
  ierror = bytes_create(some_bytes, fstring)
  ASSERT(ierror==0)
  ierror = cast(some_object, some_bytes)
  ASSERT(ierror==0)
  ierror = cast(bytes_specific, some_object)
  ASSERT(ierror==0)
  ierror = cast(res, bytes_specific)
  ASSERT(ierror==0)
  ASSERT(res==fstring)
  call some_bytes%destroy
  call some_object%destroy
  call bytes_specific%destroy
end subroutine

subroutine test_cast_to_unicode
  integer ierror
  type(unicode) :: some_unicode
  type(object) :: some_object
  type(unicode) :: unicode_specific
  character(kind=C_CHAR, len=5) :: fstring
  character(kind=C_CHAR, len=:), allocatable :: res
  
  fstring = "s" // char(195) // char(188) // char(195) // char(159) 
  
  ierror = unicode_create(some_unicode, fstring)
  ASSERT(ierror==0)
  ierror = cast(some_object, some_unicode)
  ASSERT(ierror==0)
  ierror = cast(unicode_specific, some_object)
  ASSERT(ierror==0)
  ierror = cast(res, unicode_specific)
  ASSERT(ierror==0)
  ASSERT(res==fstring)
  call some_unicode%destroy
  call some_object%destroy
  call unicode_specific%destroy
end subroutine

subroutine setUp()
 call setUp_forpy_test
end subroutine

subroutine tearDown()
 call tearDown_forpy_test
end subroutine

subroutine setUpClass()
  integer ierror
  ierror = forpy_initialize()
  
  if (ierror < 0) then
    write (*,*) "Initialisation of forpy failed!!! Tests might fail. Errorcode: ", ierror
  endif
  
  ! add current dir to search path
  ierror = run_string(C_CHAR_"import sys" // C_NEW_LINE // C_CHAR_"sys.path.append('.')"//C_NEW_LINE // C_NULL_CHAR)
  if (ierror /= 0) then
    write(*,*) "Error setting PYTHONPATH. Cannot test...", ierror
    call err_print
    STOP
  endif

  ierror = import_py(test_mod, "test_cast")
  if (ierror /= 0) then
    write(*,*) "Could not import test module 'test_cast'. Cannot test..."
    STOP
  endif
end subroutine

subroutine tearDownClass()
  call test_mod%destroy
  call forpy_finalize()
  
  call print_test_count
end subroutine

end module
