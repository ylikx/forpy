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

module unittest_mod
use, intrinsic :: iso_fortran_env, only: int32
implicit none

integer, private, save :: global_fail_flag = 0
integer, private, save :: global_tests_run = 0
integer, private, save :: global_tests_failed = 0

character(len=:), allocatable, save :: global_testname

interface pFail
  module procedure pFail_assert_fail
  module procedure pFail_assertequal_fail_int32
end interface

CONTAINS

subroutine fail_test()
  global_fail_flag = 1
end subroutine

subroutine reset_fail_flag()
  global_fail_flag = 0
end subroutine

function get_fail_flag() result(flag)
  integer flag
  flag = global_fail_flag
end function

subroutine update_test_count()
  if (global_fail_flag == 1) then
    global_tests_failed = global_tests_failed + 1
  endif
  global_tests_run = global_tests_run + 1
end subroutine

subroutine print_test_count()
  write(*,*) "---------------------------------------------------------"
  write(*,*) global_tests_run, " tests run."
  write(*,*) global_tests_failed, " tests failed."
end subroutine

! set the name of the current test
subroutine setN(tname)
  character(len=*), intent(in) :: tname
  global_testname = tname
end subroutine

subroutine pFail_assert_fail(assertion, filename, line)
  character(len=*), intent(in) :: assertion
  character(len=*), intent(in) :: filename
  integer, intent(in) :: line
  
  call fail_test
  write(*,fmt="(A,A,A,A,A,I6)") "Assertion (", assertion, ") failed @", filename, ":", line
end subroutine

subroutine pFail_assertequal_fail_int32(value1, value2, filename, line)
  integer(kind=int32), intent(in) :: value1, value2
  character(len=*), intent(in) :: filename
  integer, intent(in) :: line
  
  call fail_test
  write(*,fmt="(A,I9,A,I9,A,A,A,I6)") "Assertion (", value1, " ==", value2, ") failed @", filename, ":", line
end subroutine

end module
