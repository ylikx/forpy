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

#ifdef __GFORTRAN__
#define TEST(X) call setN("X");call preT;call X;call postT
#else
#define TEST(X) call setN(#X);call preT;call X;call postT
#endif

program test_datastructures
use test_datastructures_mod
implicit none

call setUpClass

!example for failing test
!TEST(test1)
TEST(test2)
TEST(test_list_create)
TEST(test_list_append_getitem)
TEST(test_dict_setitem_getitem)
TEST(test_tuple_setitem_getitem)
TEST(test_list_type)
TEST(test_dict_type)
TEST(test_tuple_type)
TEST(test_bytes_type)
TEST(test_str_type)
TEST(test_unicode_type)
TEST(test_list_in_tuple)
TEST(test_tuple_to_list)
TEST(test_str_in_tuple)
TEST(test_list_copy)
TEST(test_dict_copy)
TEST(test_str_create_object)

call tearDownClass

end program
