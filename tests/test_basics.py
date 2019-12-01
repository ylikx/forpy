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

import sys

test_attribute = 123
attribute_to_delete = 321

def do_nothing():
    pass

def check_args_kwargs(*args, **kwargs):
    """Tests if there are args or kwargs
       returns: 0 - no args, no kwargs
                1 -    args, no kwargs
                2 - no args, kwargs
                3      args and kwargs present"""
    res = 0
    if len(args) > 0:
        res += 1
    if len(kwargs) > 0:
        res += 2
    
    return res
    
def raise_exc():
    raise StopIteration
    
def check_arg(v):
    return (v == 42)

def check_kwarg(hello=99):
    return (hello == 42)


class MyClass(object):
    def __init__(self):
        self.x = 42

def return_small():
    return 5

def return_medium():
    return 2**48
    
def return_large():
    return 2**100

def return_int32_bounds():
    return (-2**31 - 1, -2**31, 2**31-1, 2**31)
    
def int_expected(n):
    if not isinstance(n, int):
        raise TypeError

def get_zero_length_str():
    return ""

def exception_before_return():
    raise RuntimeError
    return 3.14
    
def return_unicode():
    return u"埃利亚斯"

def return_bytes():
    return b"saile"

def check_sys_argv():
    if sys.argv != ['']:
        raise RuntimeError("sys.argv != [''], argv = {0}".format(sys.argv))

