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

program test_basics
use test_basics_mod
implicit none

call setUpClass

TEST(test_simple_call)
TEST(test_check_args01)
TEST(test_check_args02)
TEST(test_check_args03)
TEST(test_check_args04)
TEST(test_does_not_exist)
TEST(test_raise_exc)
TEST(test_raise_exc2)
TEST(test_check_arg)
TEST(test_check_kwarg)
TEST(test_instantiate)
TEST(test_cast_tuple_to_list)
TEST(test_cast_float_to_int)
TEST(test_cast_float_to_logical)
TEST(test_cast_int_to_float)
TEST(test_cast_bool_to_int)
TEST(test_cast_int_to_logical)
TEST(test_getitem_char_1d_bad)
TEST(test_bad_utf8)
TEST(test_is_int)
TEST(test_return_small)
TEST(test_return_large)
TEST(test_int_overflow)
TEST(test_int64)
TEST(test_int32_bounds)
TEST(test_int_expected)
TEST(test_print_py)
TEST(test_print_py_kwargs)
TEST(test_get_zero_length_str)
TEST(test_get_zero_length_str2)
TEST(test_exception_before_return)
TEST(test_cast_real_nostrict)
TEST(test_cast_complex_nostrict)
TEST(test_cast_int_nostrict)
TEST(test_cast_list_nostrict)
TEST(test_minus_ones64)
TEST(test_minus_ones32)
TEST(test_return_unicode)

call tearDownClass

end program
