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

!> This module contains common code that all forpy test suites use 
!> to help with testing.
module forpy_tests_common_mod
use unittest_mod, only: fail_test, get_fail_flag
use forpy_mod, only: PY_SSIZE_T_KIND, have_exception, err_print
use iso_c_binding
implicit none

public :: save_total_refcount, check_total_refcount, gettotalrefcount, &
          setUp_forpy_test, tearDown_forpy_test

PRIVATE

integer(kind=PY_SSIZE_T_KIND), save :: saved_total_refcount = 0

interface
  !PyObject *PySys_GetObject(const char *name)
  function PySys_GetObject(a_name) bind(c, name="PySys_GetObject") result(r)
    import c_ptr, C_CHAR
    character(kind=C_CHAR), dimension(*) :: a_name
    type(c_ptr) :: r
  end function
  
  function PyObject_CallObject(callable_object, args) bind(c, name="PyObject_CallObject") result(r)
    import c_ptr
    type(c_ptr), value :: callable_object, args
    type(c_ptr) :: r
  end function

  !Py_ssize_t PyNumber_AsSsize_t(PyObject *o, PyObject *exc)
  function PyNumber_AsSsize_t(o, exc) bind(c, name="PyNumber_AsSsize_t") result(r)
    import c_ptr, PY_SSIZE_T_KIND
    type(c_ptr), value :: o
    type(c_ptr), value :: exc
    integer(kind=PY_SSIZE_T_KIND) :: r
  end function

  !void Py_DecRef(PyObject *o)
  subroutine Py_DecRef(o) bind(c, name="Py_DecRef")
    import c_ptr
    type(c_ptr), value :: o
  end subroutine

end interface

CONTAINS

!> Get total reference count. Only works with debug builds of cpython.
!> Implemented using Python C-API calls, since it would not be good
!> to implement it using the functions that we want to test.
!> Returns -1 on error, no exceptions set
function gettotalrefcount() result(cnt)
  integer(kind=C_LONG) :: cnt
  
  type(c_ptr) :: gettotalrefcount_method
  type(c_ptr) :: cnt_py
  
  cnt = -1
  gettotalrefcount_method = PySys_GetObject("gettotalrefcount" // C_NULL_CHAR) ! borrowed ref
  if (.not. c_associated(gettotalrefcount_method)) then
    return
  endif
  
  cnt_py = PyObject_CallObject(gettotalrefcount_method, C_NULL_PTR)
  if (.not. c_associated(gettotalrefcount_method)) then
    return
  endif
  
  cnt = PyNumber_AsSsize_t(cnt_py, C_NULL_PTR)
  call Py_DecRef(cnt_py)
  
end function

subroutine save_total_refcount()
  saved_total_refcount = gettotalrefcount()
end subroutine

subroutine check_total_refcount()
  integer(kind=PY_SSIZE_T_KIND) :: current_total_refcount
  
  current_total_refcount = gettotalrefcount()
  
  if (current_total_refcount /= saved_total_refcount) then
    !call fail_test
    write(*,*)
    write(*,fmt="(' Following test: Refcount before/after not the same: ',I7,'/',I7,' diff=',I7)") &
      saved_total_refcount, current_total_refcount, (current_total_refcount - saved_total_refcount)
    write(*,*) "This does not necessarily mean that there is an error."
    write(*,*) "The total refcount might change due to internal caching, deleted objects, garbage collector behaviour..."
    write(*,*) "Try running the test several times to see if the problem persists."
  endif
end subroutine

!> To be called before every forpy test.
subroutine setUp_forpy_test()
#ifdef Py_DEBUG
  call save_total_refcount
#endif
end subroutine

!> To be called after every forpy test.
!> Checks if there is an uncleared exception.
!> Only for Python debug builds: Checks if total reference has changed. 
subroutine tearDown_forpy_test()
  !check if there is an uncleared exception - if yes, fail the test and clear
  if (have_exception()) then
    call fail_test
    write(*,*) "The test did not clear the following exception:"
    call err_print
    return
  endif

#ifdef Py_DEBUG
  ! test refcounts only when test has passed so far
  if (get_fail_flag() == 0) then
    call check_total_refcount
  endif
#endif
end subroutine

end module
