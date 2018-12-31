#!/usr/bin/env python
# -*- coding: UTF-8 -*-

# Copyright (C) 2017-2018  Elias Rabel
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by 
# the Free Software Foundation, either version 3 of the License, or 
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from __future__ import print_function

import numpy as np

def ndarray_expected(arr):
    if not isinstance(arr, np.ndarray):
        raise TypeError

def check_ndarray_1d(arr):
    solution = np.arange(1,25,dtype='int')
    if not np.all(arr == solution):
        raise ValueError

def check_ndarray_2d(arr):
    solution = np.arange(1,25,dtype='float64').reshape(4,6)
    if not np.all(arr == solution):
        print("Got: ")
        print(arr)
        print("Solution: ")
        print(solution)
        raise ValueError

def check_ndarray_3d(arr):
    solution = np.arange(1,25,dtype='float32').reshape(2,3,4)
    if not np.all(arr == solution):
        print("Got: ")
        print(arr)
        print("Solution: ")
        print(solution)
        raise ValueError
        
def get_ndarray_2d():
    arr = np.arange(1,25,dtype='float64').reshape((4,6), order='F')
    return arr
 
def get_ndarray_2d_c_order():
    arr = np.arange(1,25,dtype='float64').reshape((4,6), order='C')
    #print(arr)
    return arr

def get_ndarray_2d_not_contiguous():
    arr = np.arange(1,25,dtype='float64').reshape((4,6), order='F')
    arr = arr[0::2,0::2]
    #print(arr)
    return arr
    
def check_transpose_2d(array_to_check):
    solution = get_ndarray_2d().transpose()
    if not np.all(array_to_check == solution):
        raise ValueError
        
def c_order_expected(x):
    if not x.flags.c_contiguous:
        raise TypeError

def get_test_array(dimension, dtype_string):
    shape_4d = (7, 5, 3, 2)
    shape = shape_4d[0:dimension]
    tmp = np.array(range(1, np.prod(shape)+1), "int64")
    #tmp[::2] *= -1
    test_array = np.array(tmp,  dtype_string)
    test_array = test_array.reshape(shape, order='F')

    if dtype_string in ("complex64", "complex128"):
        #without imaginary part it would be boring
        test_array += -3j * test_array

    return test_array

def check_test_array(array_to_check, dimension, dtype_string):
    if np.dtype(dtype_string) != array_to_check.dtype:
        raise TypeError("dtypes do not match")

    test_array = get_test_array(dimension, dtype_string)
    
    if test_array.shape != array_to_check.shape:
        raise TypeError("shapes do not match")
    
    if not np.array_equal(array_to_check, test_array):
        #print(array_to_check)
        #print(test_array)
        raise ValueError("values do not match")
	
