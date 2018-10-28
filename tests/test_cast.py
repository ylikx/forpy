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
    
class ConvertibleNumber:
	"""Type that can be converted to different (numeric) types"""
	def __complex__(self):
		return -12.3+4.56j
	def __float__(self):
		return -12.3
	def __int__(self):
		return -12
