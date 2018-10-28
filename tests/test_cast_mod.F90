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

subroutine test_cast_real_nostrict
  integer ierror
  type(object) :: a_float
  integer :: f_int
  complex(kind=real64) :: f_complex
  logical :: f_logical
  real(kind=real64), parameter :: test_real = 3.14159_real64
  ierror = cast(a_float, test_real)
  ASSERT(ierror==0)
  ierror = cast(f_int, a_float, strict=.false.)
  ASSERT(ierror==0)
  ASSERT(f_int==3)
  ierror = cast(f_complex, a_float, strict=.false.)
  ASSERT(ierror==0)
  ASSERT(f_complex==test_real)
  ierror = cast(f_logical, a_float, strict=.false.)
  ASSERT(ierror==0)
  ASSERT(f_logical)
  call a_float%destroy
end subroutine

subroutine test_cast_complex_nostrict
  integer ierror
  type(object) :: a_number
  integer(kind=int64) :: f_int
  real(kind=real64) :: f_real
  logical :: f_logical
  complex(kind=real64), parameter :: test_complex = (3.14159_real64, -6.28_real64)
  ierror = cast(a_number, test_complex)
  ASSERT(ierror==0)
  ierror = cast(f_int, a_number, strict=.false.)
  ASSERT(ierror==EXCEPTION_ERROR)
  call err_clear
  ierror = cast(f_real, a_number, strict=.false.)
  ASSERT(ierror==EXCEPTION_ERROR)
  call err_clear
  ierror = cast(f_logical, a_number, strict=.false.)
  ASSERT(ierror==0)
  ASSERT(f_logical)
  call a_number%destroy
end subroutine

subroutine test_cast_int_nostrict
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
  ierror = cast(f_complex, a_number, strict=.false.)
  ASSERT(ierror==0)
  ASSERT(f_complex==test_complex)
  ierror = cast(f_real, a_number, strict=.false.)
  ASSERT(ierror==0)
  ASSERT(f_real==test_real)
  ierror = cast(f_logical, a_number, strict=.false.)
  ASSERT(ierror==0)
  ASSERT(f_logical)
  call a_number%destroy
end subroutine

subroutine test_cast_list_nostrict
  integer ierror
  type(list) :: a_list
  integer :: f_int
  complex(kind=real64) :: f_complex
  real(kind=real64) :: f_real
  logical :: f_logical
  ierror = list_create(a_list)
  ASSERT(ierror==0)
  ierror = cast(f_int, a_list, strict=.false.)
  ASSERT(ierror==EXCEPTION_ERROR)
  call err_clear
  ierror = cast(f_complex, a_list, strict=.false.)
  ASSERT(ierror==EXCEPTION_ERROR)
  call err_clear
  ierror = cast(f_real, a_list, strict=.false.)
  ASSERT(ierror==EXCEPTION_ERROR)
  call err_clear
  ierror = cast(f_logical, a_list, strict=.false.)
  ASSERT(ierror==0)
  ! [] has truth value 'False'
  ASSERT(.not. f_logical)
  call a_list%destroy
end subroutine

subroutine test_nonstrict_cast_numeric()
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
  
  ierror = cast(a_complex, obj, strict=.false.)
  ASSERT(ierror==0)
  ASSERT(a_complex==SOLUTION_COMPLEX)
  
  ierror = cast(a_real, obj, strict=.false.)
  ASSERT(ierror==0)
  ASSERT(a_real==-12.3_C_DOUBLE)
  
  ierror = cast(a_int, obj, strict=.false.)
  ASSERT(ierror==0)
  ASSERT(a_int==-12_int64)
  
  call obj%destroy
end subroutine

subroutine setUp()

end subroutine

subroutine tearDown()
  !check if there is an uncleared exception - if yes, fail the test and clear
  if (have_exception()) then
    call fail_test
    write(*,*) "The test did not clear the following exception:"
    call err_print
  endif
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
