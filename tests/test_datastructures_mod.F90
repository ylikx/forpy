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

module test_datastructures_mod
use unittest_mod
use forpy_mod
use iso_c_binding
use forpy_tests_common_mod, only: setUp_forpy_test, tearDown_forpy_test, gettotalrefcount
implicit none

CONTAINS

#include "unittest_mod.inc"

subroutine test1
  ASSERT(.false.)
end subroutine

subroutine test2
  ASSERT(.true.)
end subroutine

subroutine test_list_create
  integer ierror
  type(list) :: li
  ierror = list_create(li)
  ASSERT(ierror==0)
  call li%destroy
end subroutine

subroutine test_list_append_getitem
  integer ierror, element
  type(list) :: li
  ierror = list_create(li)
  ASSERT(ierror==0)
  ierror = li%append(42)
  ASSERT(ierror==0)
  ierror = li%getitem(element, 0)
  ASSERT(ierror==0)
  ASSERTEQUAL(element, 42)
  call li%destroy
end subroutine

subroutine test_dict_setitem_getitem
  integer ierror, element
  type(dict) :: di
  ierror = dict_create(di)
  ASSERT(ierror==0)
  ierror = di%setitem("hello", 42)
  ASSERT(ierror==0)
  ierror = di%getitem(element, "hello")
  ASSERT(ierror==0)  
  ASSERT(element==42)
  call di%destroy
end subroutine

subroutine test_tuple_setitem_getitem
  integer ierror, element
  type(tuple) :: tu
  ierror = tuple_create(tu, 1)
  ASSERT(ierror==0)
  ierror = tu%setitem(0, 42)
  ASSERT(ierror==0)
  ierror = tu%getitem(element, 0)
  ASSERT(ierror==0)
  ASSERT(element==42)
  call tu%destroy
end subroutine

subroutine test_list_in_tuple
! construct the tuple ([]) and read its element
  integer ierror
  type(tuple) :: tu
  type(list) :: li, li2
  type(object) :: obj
  ierror = tuple_create(tu, 1)
  ASSERT(ierror==0)
  ierror = list_create(li)
  ASSERT(ierror==0)
  ierror = tu%setitem(0, li)
  ASSERT(ierror==0)
  ierror = tu%getitem(obj, 0)
  ASSERT(ierror==0)
  ASSERT(is_list(obj))
  ierror = cast(li2, obj)
  ASSERT(ierror==0)
  call tu%destroy
  call li%destroy
  call li2%destroy
  call obj%destroy
end subroutine

subroutine test_list_type
  integer ierror
  type(list) :: li
  ierror = list_create(li)
  ASSERT(ierror==0)
  ASSERT(is_list(li))
  ASSERT(.not. is_dict(li))
  ASSERT(.not. is_tuple(li))
  ASSERT(.not. is_none(li))
  ASSERT(.not. is_int(li))
  ASSERT(.not. is_bool(li))
  ASSERT(.not. is_float(li))
  ASSERT(.not. is_complex(li))
  ASSERT(.not. is_bytes(li))
  ASSERT(.not. is_str(li))
  ASSERT(.not. is_unicode(li))
  call li%destroy
end subroutine

subroutine test_dict_type
  integer ierror
  type(dict) :: di
  ierror = dict_create(di)
  ASSERT(ierror==0)
  ASSERT(.not. is_list(di))
  ASSERT(is_dict(di))
  ASSERT(.not. is_tuple(di))
  ASSERT(.not. is_none(di))
  ASSERT(.not. is_int(di))
  ASSERT(.not. is_bool(di))
  ASSERT(.not. is_float(di))
  ASSERT(.not. is_complex(di))
  ASSERT(.not. is_bytes(di))
  ASSERT(.not. is_str(di))
  ASSERT(.not. is_unicode(di))
  call di%destroy
end subroutine

subroutine test_tuple_type
  integer ierror
  type(tuple) :: t
  ierror = tuple_create(t, 0)
  ASSERT(ierror==0)
  ASSERT(.not. is_list(t))
  ASSERT(.not. is_dict(t))
  ASSERT(is_tuple(t))
  ASSERT(.not. is_none(t))
  ASSERT(.not. is_int(t))
  ASSERT(.not. is_bool(t))
  ASSERT(.not. is_float(t))
  ASSERT(.not. is_complex(t))
  ASSERT(.not. is_bytes(t))
  ASSERT(.not. is_str(t))
  ASSERT(.not. is_unicode(t))
  call t%destroy
end subroutine

subroutine test_bytes_type
  integer ierror
  type(bytes) :: t
  ierror = bytes_create(t, "abc")
  ASSERT(ierror==0)
  ASSERT(.not. is_list(t))
  ASSERT(.not. is_dict(t))
  ASSERT(.not. is_tuple(t))
  ASSERT(.not. is_none(t))
  ASSERT(.not. is_int(t))
  ASSERT(.not. is_bool(t))
  ASSERT(.not. is_float(t))
  ASSERT(.not. is_complex(t))
  ASSERT(is_bytes(t))
  ASSERT(.not. is_unicode(t))
  call t%destroy
end subroutine

subroutine test_str_type
  integer ierror
  type(str) :: t
  ierror = str_create(t, "abc")
  ASSERT(ierror==0)
  ASSERT(.not. is_list(t))
  ASSERT(.not. is_dict(t))
  ASSERT(.not. is_tuple(t))
  ASSERT(.not. is_none(t))
  ASSERT(.not. is_int(t))
  ASSERT(.not. is_bool(t))
  ASSERT(.not. is_float(t))
  ASSERT(.not. is_complex(t))
  ASSERT(is_str(t))
  call t%destroy
end subroutine

subroutine test_unicode_type
  integer ierror
  type(unicode) :: t
  ierror = unicode_create(t, "abc")
  ASSERT(ierror==0)
  ASSERT(.not. is_list(t))
  ASSERT(.not. is_dict(t))
  ASSERT(.not. is_tuple(t))
  ASSERT(.not. is_none(t))
  ASSERT(.not. is_int(t))
  ASSERT(.not. is_bool(t))
  ASSERT(.not. is_float(t))
  ASSERT(.not. is_complex(t))
  ASSERT(.not. is_bytes(t))
  ASSERT(is_unicode(t))
  call t%destroy
end subroutine

subroutine test_tuple_to_list
  integer ierror, element
  type(tuple) :: t
  type(list) :: li
  ierror = tuple_create(t, 1)
  ierror = t%setitem(0, 42)
  ierror = list_create(li, t)
  ierror = li%getitem(element, 0)
  ASSERT(element == 42)
  call t%destroy
  call li%destroy
end subroutine

subroutine test_list_copy
  integer ierror, element
  type(list) :: li, li_copy
  ierror = list_create(li)
  ierror = li%append(23)
  ierror = li%copy(li_copy)
  ierror = li_copy%getitem(element, 0)
  ASSERT(element==23)
  call li%destroy
  call li_copy%destroy
end subroutine

subroutine test_dict_copy
  integer ierror, element
  type(dict) :: di, di_copy
  ierror = dict_create(di)
  ierror = di%setitem("hi", 23)
  ierror = di%copy(di_copy)
  ierror = di_copy%getitem(element, "hi")
  ASSERT(element==23)
  call di%destroy
  call di_copy%destroy
end subroutine

subroutine test_str_in_tuple
  integer ierror
  type(tuple) :: t
  character(kind=C_CHAR, len=:), allocatable :: buffer
  character(kind=C_CHAR, len=8), parameter :: TESTSTR = "abcdefgh"
  ierror = tuple_create(t, 1)
  ASSERT(ierror==0)
  ierror = t%setitem(0, TESTSTR)
  ASSERT(ierror==0)
  ierror = t%getitem(buffer, 0)
  ASSERT(ierror==0)
  ASSERT(buffer==TESTSTR)
  call t%destroy
end subroutine

subroutine test_str_create_object()
  integer :: ierror
  type(NoneType) :: the_none
  type(str) :: a_str
  character(kind=C_CHAR,len=:), allocatable :: res

  ierror = NoneType_create(the_none)
  ASSERT(ierror==0)
  ierror = str_create(a_str, the_none)
  ASSERT(ierror==0)
  ierror = cast(res, a_str)
  ASSERT(ierror==0)
  ASSERT(res=='None')
  call the_none%destroy
  call a_str%destroy
end subroutine

subroutine setUp()
  call setUp_forpy_test
end subroutine

subroutine tearDown()
  call tearDown_forpy_test
end subroutine

subroutine setUpClass()
  integer ierror
  ierror = forpy_initialize(use_numpy=.false.)
  
  if (ierror /= 0) then
    write (*,*) "Initialisation of forpy failed!!! Most tests will fail! Errorcode: ", ierror
  endif
end subroutine

subroutine tearDownClass()
  call forpy_finalize()
  call print_test_count
end subroutine

end module
