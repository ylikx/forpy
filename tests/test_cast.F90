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

program test_cast
use test_cast_mod
implicit none

call setUpClass

TEST(test_cast_tuple_to_list)
TEST(test_cast_float_to_int)
TEST(test_cast_float_to_logical)
TEST(test_cast_int_to_float)
TEST(test_cast_bool_to_int)
TEST(test_cast_int_to_logical)
TEST(test_cast_nonstrict_real)
TEST(test_cast_nonstrict_complex)
TEST(test_cast_nonstrict_int)
TEST(test_cast_nonstrict_list)
TEST(test_cast_nonstrict_numeric)

TEST(test_cast_nonstrict_list_to_list)
TEST(test_cast_nonstrict_tuple_to_list)
TEST(test_cast_nonstrict_int_to_list)

TEST(test_cast_nonstrict_list_to_tuple)
TEST(test_cast_nonstrict_tuple_to_tuple)
TEST(test_cast_nonstrict_int_to_tuple)

TEST(test_cast_nonstrict_list_to_chars)
TEST(test_cast_nonstrict_tuple_to_chars)
TEST(test_cast_nonstrict_int_to_chars)
TEST(test_cast_nonstrict_bytes_to_chars)
TEST(test_cast_nonstrict_none_to_str)
TEST(test_cast_bytes_to_chars)
TEST(test_cast_chars_to_object)

TEST(test_cast_to_str)
TEST(test_cast_to_bytes)
TEST(test_cast_to_unicode)
call tearDownClass

end program
