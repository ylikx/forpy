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

module test_basics_mod
use unittest_mod
use forpy_mod
use iso_fortran_env
use iso_c_binding
implicit none

type(module_py), save :: test_mod

CONTAINS

#include "unittest_mod.inc"

subroutine test_multiple_inits
  integer ierror, ierror2
  logical ok
  ! it must be safe to call forpy_initialize multiple times
  ierror = forpy_initialize()
  ok = (ierror == 0 .or. ierror == NO_NUMPY_ERROR)
  ASSERT(ok)
  ierror2 = forpy_initialize()
  ASSERT(ierror==ierror2)
  ierror2 = forpy_initialize()
  ASSERT(ierror==ierror2)  
end subroutine

subroutine test_simple_call
  integer ierror
  type(object) :: retval
  type(tuple) :: args
  type(dict) :: kwargs
  ierror = tuple_create(args, 0)
  ierror = dict_create(kwargs)
  ierror = call_py(retval, test_mod, "do_nothing", args, kwargs)
  ASSERT(ierror==0)
  ASSERT(is_none(retval))
  call retval%destroy
  call args%destroy
  call kwargs%destroy
end subroutine

subroutine test_check_args01
  integer ierror
  type(object) :: retval
  integer res
  type(tuple) :: args
  type(dict) :: kwargs
  ierror = tuple_create(args, 1)
  ierror = args%setitem(0, 101)
  ierror = dict_create(kwargs)
  ierror = kwargs%setitem("greeting", "hi")
  ierror = call_py(retval, test_mod, "check_args_kwargs", args, kwargs)
  ASSERT(ierror==0)
  ierror = cast(res, retval)
  ASSERT(res == 3)
  call retval%destroy
  call args%destroy
  call kwargs%destroy
end subroutine

subroutine test_check_args02
  integer ierror
  type(object) :: retval
  integer res
  type(tuple) :: args
  ierror = tuple_create(args, 1)
  ierror = args%setitem(0, 101)
  ierror = call_py(retval, test_mod, "check_args_kwargs", args)
  ASSERT(ierror==0)
  ierror = cast(res, retval)
  ASSERT(res == 1)
  call retval%destroy
  call args%destroy
end subroutine

subroutine test_check_args03
  integer ierror
  type(object) :: retval
  integer res
  type(dict) :: kwargs
  ierror = dict_create(kwargs)
  ierror = kwargs%setitem("greeting", "hi")
  ierror = call_py(retval, test_mod, "check_args_kwargs", kwargs=kwargs)
  ASSERT(ierror==0)
  ierror = cast(res, retval)
  ASSERT(res == 2)
  call retval%destroy
  call kwargs%destroy
end subroutine

subroutine test_check_args04
  integer ierror
  type(object) :: retval
  integer res
  ierror = call_py(retval, test_mod, "check_args_kwargs")
  ASSERT(ierror==0)
  ierror = cast(res, retval)
  ASSERT(res == 0)
  call retval%destroy
end subroutine

subroutine test_does_not_exist
  integer ierror
  type(object) :: retval
  type(tuple) :: args
  type(dict) :: kwargs
  logical :: exc_correct
  ierror = tuple_create(args, 0)
  ierror = dict_create(kwargs)
  ierror = call_py(retval, test_mod, "does_not_exist", args, kwargs)
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(AttributeError)
  ASSERT(exc_correct)
  call err_clear
  call args%destroy
  call kwargs%destroy
end subroutine

subroutine test_raise_exc
  integer ierror
  type(object) :: retval
  type(tuple) :: args
  type(dict) :: kwargs
  logical :: exc_correct
  ierror = tuple_create(args, 0)
  ierror = dict_create(kwargs)
  ierror = call_py(retval, test_mod, "raise_exc", args, kwargs)
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(StopIteration)
  ASSERT(exc_correct)
  call err_clear
  call retval%destroy
  call args%destroy
  call kwargs%destroy
end subroutine

subroutine test_raise_exc2
  logical exc_correct
  call raise_exception(RuntimeError, "test")
  ASSERT(have_exception())
  exc_correct = exception_matches(RuntimeError)
  ASSERT(exc_correct)
  call err_clear
end subroutine

subroutine test_check_arg
  integer ierror
  type(object) :: retval
  type(tuple) :: args
  type(dict) :: kwargs
  logical :: flag
  flag = .false.
  ierror = tuple_create(args, 1)
  ierror = args%setitem(0, 42)
  ierror = dict_create(kwargs)
  ierror = call_py(retval, test_mod, "check_arg", args, kwargs)
  ASSERT(ierror==0)
  ierror = cast(flag, retval)
  ASSERT(ierror==0)
  ASSERT(flag)
  call retval%destroy
  call args%destroy
  call kwargs%destroy
end subroutine

subroutine test_check_kwarg
  integer ierror
  type(object) :: retval
  type(tuple) :: args
  type(dict) :: kwargs
  logical :: flag
  flag = .false.
  ierror = tuple_create(args, 0)
  ierror = dict_create(kwargs)
  ierror = kwargs%setitem("hello", 42)
  ierror = call_py(retval, test_mod, "check_kwarg", args, kwargs)
  ASSERT(ierror==0)
  ierror = cast(flag, retval)
  ASSERT(ierror==0)
  ASSERT(flag)
  call retval%destroy
  call args%destroy
  call kwargs%destroy
end subroutine

subroutine test_instantiate
  integer ierror
  type(object) :: instance, property
  type(tuple) :: args
  type(dict) :: kwargs
  logical :: flag
  flag = .false.
  ierror = tuple_create(args, 0)
  ierror = dict_create(kwargs)
  ierror = call_py(instance, test_mod, "MyClass", args, kwargs)
  ASSERT(ierror==0)
  ierror = instance%getattribute(property, "x")
  ASSERT(ierror==0)
  call instance%destroy
  call property%destroy
  call args%destroy
  call kwargs%destroy
end subroutine

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

subroutine test_getitem_char_1d_bad
  integer ierror
  type(list) :: li
  logical :: exc_correct
  character(kind=C_CHAR), dimension(:), pointer :: ptr
  ierror = list_create(li)
  ASSERT(ierror==0)
  ierror = li%append(101)
  ASSERT(ierror==0)
  ierror = li%getitem(ptr, 0)
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(TypeError)
  ASSERT(exc_correct)
  call err_clear
  call li%destroy
end subroutine

subroutine test_bad_utf8
  integer ierror
  character(kind=C_CHAR, len=2) :: s
  type(unicode) :: uni
  logical :: exc_correct
  s(1:1) = char(128)
  s(2:2) = '@'
  ierror = unicode_create(uni, s)
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(UnicodeDecodeError)
  ASSERT(exc_correct)
  call err_clear
  ASSERT(is_null(uni))
end subroutine

subroutine test_is_int
  integer ierror
  type(object) :: ii
  ierror = cast(ii, 101)
  ASSERT(is_int(ii))
  call ii%destroy
end subroutine

subroutine test_return_small
  integer ierror
  type(object) :: retval
  type(tuple) :: args
  type(dict) :: kwargs
  integer :: element
  ierror = tuple_create(args, 0)
  ierror = dict_create(kwargs)
  ierror = call_py(retval, test_mod, "return_small", args, kwargs)
  ASSERT(ierror==0) 
  ierror = cast(element, retval)
  ASSERT(is_int(retval))
  ASSERT(ierror==0)
  ASSERT(element==5)
  call retval%destroy
  call args%destroy
  call kwargs%destroy
end subroutine

subroutine test_return_large
  integer ierror
  type(object) :: retval
  type(tuple) :: args
  type(dict) :: kwargs
  logical :: exc_correct
  integer :: element
  ierror = tuple_create(args, 0)
  ierror = dict_create(kwargs)
  ierror = call_py(retval, test_mod, "return_large", args, kwargs)
  ASSERT(ierror==0)
  ierror = cast(element, retval)
  ASSERT(is_int(retval))
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(OverflowError)
  ASSERT(exc_correct)
  ASSERT(element==-1)
  call err_clear
  call retval%destroy
  call args%destroy
  call kwargs%destroy
end subroutine

subroutine test_int_overflow
  integer ierror
  type(object) :: retval
  type(tuple) :: args
  type(dict) :: kwargs
  logical :: exc_correct
  integer(kind=int32) :: element
  ierror = tuple_create(args, 0)
  ierror = dict_create(kwargs)
  ierror = call_py(retval, test_mod, "return_medium", args, kwargs)
  ASSERT(ierror==0)
  ierror = cast(element, retval)
  ASSERT(is_int(retval))
  ASSERT(ierror==EXCEPTION_ERROR)
  exc_correct = exception_matches(OverflowError)
  ASSERT(exc_correct)
  call err_clear
  call retval%destroy
  call args%destroy
  call kwargs%destroy
end subroutine

subroutine test_int64
  integer ierror
  type(object) :: retval
  type(tuple) :: args
  type(dict) :: kwargs
  integer(kind=int64) :: element
  ierror = tuple_create(args, 0)
  ierror = dict_create(kwargs)
  ierror = call_py(retval, test_mod, "return_medium", args, kwargs)
  ASSERT(ierror==0)
  ierror = cast(element, retval)
  ASSERT(is_int(retval))
  ASSERT(ierror==0)
  ASSERT(.not. have_exception())
  ASSERT(element==281474976710656_int64)
  call retval%destroy
  call args%destroy
  call kwargs%destroy
end subroutine

subroutine test_int32_bounds
  integer ierror
  type(object) :: retval
  type(tuple) :: bounds
  type(tuple) :: args
  type(dict) :: kwargs
  integer(kind=int32) :: element
  ierror = tuple_create(args, 0)
  ierror = dict_create(kwargs)
  ierror = call_py(retval, test_mod, "return_int32_bounds", args, kwargs)
  ASSERT(ierror==0)
  ierror = cast(bounds, retval)
  ASSERT(ierror==0)
  
  ! -2**31-1 out of bounds
  ierror = bounds%getitem(element, 0)
  ASSERT(ierror == EXCEPTION_ERROR)
  ASSERT(have_exception())
  call err_clear
  
  ! -2**31 in bounds
  ierror = bounds%getitem(element, 1)
  ASSERT(ierror == 0)
  ASSERT(.not. have_exception())
  call err_clear

  ! 2**31 - 1 in bounds
  ierror = bounds%getitem(element, 2)
  ASSERT(ierror == 0)
  ASSERT(.not. have_exception())
  call err_clear

  ! 2**31 out of bounds
  ierror = bounds%getitem(element, 3)
  ASSERT(ierror /= 0)
  ASSERT(have_exception())
  call err_clear

  call bounds%destroy
  call retval%destroy
  call args%destroy
  call kwargs%destroy
end subroutine

subroutine test_int_expected
  integer ierror
  type(tuple) :: args
  ierror = tuple_create(args, 1)
  ierror = args%setitem(0, 555)
  ierror = call_py_noret(test_mod, "int_expected", args)
  ASSERT(ierror==0)
  ASSERT(.not. have_exception())
  call args%destroy
end subroutine

subroutine test_print_py
  integer ierror
  type(str) :: message
  ierror = str_create(message, "Testing the print function.")
  ASSERT(ierror==0)
  ierror = print_py(message)
  ASSERT(ierror==0)
  call message%destroy
end subroutine

subroutine test_print_py_kwargs
  integer ierror
  type(str) :: message
  type(dict) :: kwargs
  ierror = str_create(message, "Testing the print function with kwargs")
  ASSERT(ierror==0)
  ierror = dict_create(kwargs)
  ASSERT(ierror==0)
  ierror = kwargs%setitem("end", "!" // C_NEW_LINE)
  ASSERT(ierror==0)
  ierror = print_py(message, kwargs)
  ASSERT(ierror==0)
  call kwargs%destroy
  call message%destroy
end subroutine

subroutine test_get_zero_length_str
  integer ierror
  character(kind=C_CHAR), dimension(:), pointer :: ptr
  type(object) :: retval
  ierror = call_py(retval, test_mod, "get_zero_length_str")
  ASSERT(ierror==0)
  ASSERT(is_str(retval))
  ierror = cast(ptr, retval)
  ASSERT(ierror==0)
  ASSERT(size(ptr)==0)
  call retval%destroy
end subroutine

subroutine test_get_zero_length_str2
  integer ierror
  character(kind=C_CHAR, len=:), allocatable :: a_str
  type(object) :: retval
  ierror = call_py(retval, test_mod, "get_zero_length_str")
  ASSERT(ierror==0)
  ASSERT(is_str(retval))
  ierror = cast(a_str, retval)
  ASSERT(ierror==0)
  ASSERT(len(a_str)==0)
  ASSERT(allocated(a_str))
  call retval%destroy
end subroutine

subroutine test_exception_before_return
  integer ierror
  type(object) :: retval
  ierror = call_py(retval, test_mod, "exception_before_return")
  ASSERT(ierror/=0)
  ASSERT(have_exception())
  ASSERT(is_null(retval))
  call err_clear
  call retval%destroy
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

subroutine test_minus_ones64
! Testing values of -1, because C-api sometimes uses this value as error
! indicator
  integer ierror
  type(list) :: a_list
  integer(kind=int64) :: f_int
  complex(kind=real64) :: f_complex
  real(kind=real64) :: f_real
  integer(kind=int64), parameter :: minus_one = -1_int64
  complex(kind=real64), parameter :: c_minus_one = (-1.0_real64, 0.0_real64)
  real(kind=real64), parameter :: r_minus_one = -1.0_real64
  ierror = list_create(a_list)
  ASSERT(ierror==0)
  ierror = a_list%append(minus_one)
  ierror = a_list%append(r_minus_one)
  ierror = a_list%append(c_minus_one)
  ASSERT(.not. have_exception())
  ierror = a_list%getitem(f_int, 0)
  ASSERT(ierror==0)
  ASSERT(f_int==minus_one)
  ierror = a_list%getitem(f_real, 1)
  ASSERT(ierror==0)
  ASSERT(f_real==r_minus_one)
  ierror = a_list%getitem(f_complex, 2)
  ASSERT(ierror==0)
  ASSERT(f_complex==c_minus_one)
  call a_list%destroy
end subroutine

subroutine test_minus_ones32
! Testing values of -1, because C-api sometimes uses this value as error
! indicator
  integer ierror
  type(list) :: a_list
  integer(kind=int32) :: f_int
  complex(kind=real32) :: f_complex
  real(kind=real32) :: f_real
  integer(kind=int32), parameter :: minus_one = -1_int32
  complex(kind=real32), parameter :: c_minus_one = (-1.0_real32, 0.0_real32)
  real(kind=real32), parameter :: r_minus_one = -1.0_real32
  ierror = list_create(a_list)
  ASSERT(ierror==0)
  ierror = a_list%append(minus_one)
  ierror = a_list%append(r_minus_one)
  ierror = a_list%append(c_minus_one)
  ASSERT(.not. have_exception())
  ierror = a_list%getitem(f_int, 0)
  ASSERT(ierror==0)
  ASSERT(f_int==minus_one)
  ierror = a_list%getitem(f_real, 1)
  ASSERT(ierror==0)
  ASSERT(f_real==r_minus_one)
  ierror = a_list%getitem(f_complex, 2)
  ASSERT(ierror==0)
  ASSERT(f_complex==c_minus_one)
  call a_list%destroy
end subroutine

subroutine test_return_unicode
  integer ierror
  character(kind=C_CHAR), dimension(:), pointer :: uni
  type(object) :: retval
  logical :: exc_correct
  character(kind=C_CHAR), dimension(12) :: solution
  
  solution(1) = char(229, kind=C_CHAR) 
  solution(2) = char(159, kind=C_CHAR)
  solution(3) = char(131, kind=C_CHAR)
  solution(4) = char(229, kind=C_CHAR)
  solution(5) = char(136, kind=C_CHAR)
  solution(6) = char(169, kind=C_CHAR)
  solution(7) = char(228, kind=C_CHAR)
  solution(8) = char(186, kind=C_CHAR)
  solution(9) = char(154, kind=C_CHAR)
  solution(10) = char(230, kind=C_CHAR)
  solution(11) = char(150, kind=C_CHAR)
  solution(12) = char(175, kind=C_CHAR)

  ierror = call_py(retval, test_mod, "return_unicode")
  ASSERT(ierror==0)
  ASSERT(is_unicode(retval))
  ierror = cast(uni, retval)
  if (ierror /= 0) then
    ! Python 2: casting from unicode not supported (yet)
    exc_correct = exception_matches(TypeError)
    ASSERT(exc_correct)
    call err_clear
    call retval%destroy
    return
  endif
  
  ASSERT(size(uni)==12)
  ASSERT(all(uni==solution))
  
  call retval%destroy
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

  ierror = import_py(test_mod, "test_basics")
  if (ierror /= 0) then
    write(*,*) "Could not import test module 'test_basics'. Cannot test..."
    STOP
  endif
end subroutine

subroutine tearDownClass()
  call test_mod%destroy
  call forpy_finalize()
  
  call print_test_count
end subroutine

end module
