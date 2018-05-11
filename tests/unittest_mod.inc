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

! include this in the "contains" - section of each test module

! Note: When using the ASSERT macro, put strings into single quotes!

#ifdef __GFORTRAN__
#define ASSERT(X) if(.not.(X)) then;call pFail("X",__FILE__,__LINE__);return;endif 
#else
#define ASSERT(X) if(.not.(X)) then;call pFail(#X,__FILE__,__LINE__);return;endif 
#endif

#define ASSERTEQUAL(X,Y) if((X)/=(Y)) then;call pFail(X,Y,__FILE__,__LINE__);return;endif

subroutine preT()
  call setUp()
  call reset_fail_flag()
end subroutine

subroutine postT()
  call tearDown()
  
  call update_test_count()

  if (get_fail_flag() == 0) then
    write(*,*) "[OK  ] ", global_testname
  else
    write(*,*) "[FAIL] ", global_testname
  endif
end subroutine

