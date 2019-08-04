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

module forpy_mod
!! author: Elias Rabel
!!
!! Forpy: A library for Fortran-Python interoperability.
!! 
!! Forpy allows you to use Python features within Fortran ("Python in Fortran")
!! For example: Python modules, datastructures such as list, dict, tuple
!! Furthermore you can write Python (extension) modules in Fortran ("Fortran in Python")

!  Contact:
!  Let me know, if you find this library useful:
!  Mail: ylikx.0 at gmail dot com
!  Web:  https://github.com/ylikx

!  This project uses the fypp preprocessor (https://github.com/aradi/fypp)
!  to generate the code
!  Do not edit forpy_mod.F90, edit forpy_mod.fypp
! 

#ifdef PYTHON2_32
#define PYTHON2
#endif

use, intrinsic :: iso_c_binding, only: C_CHAR, C_NULL_CHAR, C_INT, C_LONG, C_LONG_LONG, C_NEW_LINE, c_ptr, C_DOUBLE, &
                                       C_DOUBLE_COMPLEX, c_associated, C_NULL_PTR, c_loc, c_f_pointer, &
                                       c_funptr, c_funloc, C_NULL_FUNPTR, C_INTPTR_T

use, intrinsic :: iso_fortran_env, only: int64, int32, real32, real64

implicit none 

public :: object, type_py, list, dict, tuple, bytes, str, unicode, module_py, &
NoneType, ndarray, Sequence, MutableSequence, ImmutableSequence, Mapping, &
tuple_create, list_create, dict_create, bytes_create, str_create, &
unicode_create, NoneType_create, ndarray_create, ndarray_create_nocopy, & 
ndarray_create_empty, ndarray_create_zeros, ndarray_create_ones, &
import_py, call_py, call_py_noret, assign_py, cast, cast_nonstrict, &
PythonMethodTable, PythonModule, forpy_initialize, forpy_initialize_ext, &
forpy_finalize, is_long, is_list, is_tuple, is_bytes, is_dict, &
is_float, is_complex, is_bool, is_unicode, is_int, is_str, is_none, &
is_null, is_ndarray, exception_matches, err_clear, err_print, have_exception, &
raise_exception, print_py, get_sys_path, run_string, unsafe_cast_from_c_ptr

! ERROR CODES
integer(kind=C_INT), public, parameter :: NO_NUMPY_ERROR = 2_C_INT
integer(kind=C_INT), public, parameter :: EXCEPTION_ERROR = -1_C_INT

! Flags used for Python extension development
integer(kind=C_INT), public, parameter :: METH_VARARGS = 1_C_INT
integer(kind=C_INT), public, parameter :: METH_KEYWORDS = 2_C_INT
integer(kind=C_INT), public, parameter :: METH_NOARGS = 4_C_INT
integer(kind=C_INT), public, parameter :: METH_O = 8_C_INT

integer, public, parameter :: PY_SSIZE_T_KIND = C_INTPTR_T

PRIVATE

! These global variables shall be set in
! forpy_initialize only and never changed afterwards!
type(c_ptr), private, save :: global_numpy_mod = C_NULL_PTR
type(c_ptr), private, save :: global_numpy_asarray_method = C_NULL_PTR
! the location of the singleton Python Py_NoneStruct method
! initialised in forpy_initialize - if not called, working with None
! is impossible
type(c_ptr), private, save :: global_Py_NoneStruct_ptr = C_NULL_PTR

! Similar for the 2 singleton bools
type(c_ptr), private, save :: global_Py_TrueStruct_ptr = C_NULL_PTR
type(c_ptr), private, save :: global_Py_FalseStruct_ptr = C_NULL_PTR

type(c_ptr), private, save :: global_numpy_ndarray_typeobj = C_NULL_PTR

!pointers to type-objects of fundamental datatypes
!initialised in forpy_initialize
type(c_ptr), private, save :: global_pyfloat_type_ptr = C_NULL_PTR
type(c_ptr), private, save :: global_pycomplex_type_ptr = C_NULL_PTR
type(c_ptr), private, save :: global_pybool_type_ptr = C_NULL_PTR
type(c_ptr), private, save :: global_pyunicode_type_ptr = C_NULL_PTR

type, bind(c) :: Py_buffer
  type(c_ptr) :: buf
  type(c_ptr) :: obj
  integer(kind=PY_SSIZE_T_KIND) :: len
  integer(kind=PY_SSIZE_T_KIND) :: itemsize
  integer(kind=C_INT) :: readonly
  integer(kind=C_INT) :: ndim
  type(c_ptr) :: format
  type(c_ptr) :: shape
  type(c_ptr) :: strides
  type(c_ptr) :: suboffsets
#ifdef PYTHON2
  integer(kind=PY_SSIZE_T_KIND) :: smalltable(2)
#endif
  type(c_ptr) :: internal
end type

type, bind(c) :: PyObject
#ifdef Py_DEBUG
    type(c_ptr) :: ob_next
    type(c_ptr) :: ob_prev
#endif
    integer(kind=PY_SSIZE_T_KIND) :: ob_refcnt
    type(c_ptr) :: ob_type
end type

type, bind(c) :: PyTypeObject
#ifdef Py_DEBUG
    type(c_ptr) :: ob_next
    type(c_ptr) :: ob_prev
#endif
    integer(kind=PY_SSIZE_T_KIND) :: ob_refcnt
    type(c_ptr) :: ob_type
    integer(kind=PY_SSIZE_T_KIND) :: ob_size

    type(c_ptr) :: tp_name ! For printing, in format "<module>.<name>"
    integer(kind=PY_SSIZE_T_KIND) :: tp_basicsize, tp_itemsize ! For allocation

    !Methods to implement standard operations

    type(c_ptr) :: tp_dealloc
    type(c_ptr) :: tp_print
    type(c_ptr) :: tp_getattr
    type(c_ptr) :: tp_setattr
    type(c_ptr) :: tp_compare
    type(c_ptr) :: tp_repr

    !Method suites for standard classes

    type(c_ptr) :: tp_as_number
    type(c_ptr) :: tp_as_sequence
    type(c_ptr) :: tp_as_mapping

    ! More standard operations (here for binary compatibility)

    type(c_ptr) :: tp_hash
    type(c_ptr) :: tp_call
    type(c_ptr) :: tp_str
    type(c_ptr) :: tp_getattro
    type(c_ptr) :: tp_setattro

    ! Functions to access object as input/output buffer
    type(c_ptr) :: tp_as_buffer

    !Flags to define presence of optional/expanded features
    integer(kind=C_LONG) :: tp_flags  ! Python2: long, Python3: unsigned long

    type(c_ptr) :: tp_doc ! Documentation string

    !call function for all accessible objects
    type(c_ptr) :: tp_traverse

    ! delete references to contained objects
    type(c_ptr) :: tp_clear

    ! Assigned meaning in release 2.1
    ! rich comparisons
    type(c_ptr) :: tp_richcompare

    ! weak reference enabler
    integer(kind=PY_SSIZE_T_KIND) :: tp_weaklistoffset

    !Added in release 2.2
    !Iterators
    type(c_ptr) :: tp_iter
    type(c_ptr) :: tp_iternext

    ! Attribute descriptor and subclassing stuff
    type(c_ptr) :: tp_methods
    type(c_ptr) :: tp_members
    type(c_ptr) :: tp_getset
    type(c_ptr) :: tp_base
    type(c_ptr) :: tp_dict
    type(c_ptr) :: tp_descr_get
    type(c_ptr) :: tp_descr_set
    integer(kind=PY_SSIZE_T_KIND) :: tp_dictoffset
    type(c_ptr) :: tp_init
    type(c_ptr) :: tp_alloc
    type(c_ptr) :: tp_new
    type(c_ptr) :: tp_free ! Low-level free-memory routine
    type(c_ptr) :: tp_is_gc ! For PyObject_IS_GC
    type(c_ptr) :: tp_bases
    type(c_ptr) :: tp_mro ! method resolution order
    type(c_ptr) :: tp_cache
    type(c_ptr) :: tp_subclasses
    type(c_ptr) :: tp_weaklist
    type(c_ptr) :: tp_del

    ! Type attribute cache version tag. Added in version 2.6
    integer(kind=C_INT) :: tp_version_tag

    ! additionally Python3 has this field:
    ! destructor tp_finalize;
    ! we should be fine without it, since all we actually need is the offset
    ! of tp_flags and we are not using arrays of PyTypeObjects

end type

type, bind(c) :: Py_complex
  real(kind=C_DOUBLE) :: real_part
  real(kind=C_DOUBLE) :: imag_part
end type

type, bind(c) :: PyMethodDef
  type(c_ptr) :: ml_name
  type(c_funptr) :: ml_meth
  integer(kind=C_INT) :: ml_flags
  type(c_ptr) :: ml_doc
end type

type, bind(c) :: PyModuleDef_Base
  integer(kind=PY_SSIZE_T_KIND) :: ob_refcnt !PyObject_HEAD  (init to 1)
  type(c_ptr) :: ob_type ! from PyObject_HEAD (init to NULL)
    
  type(c_ptr) :: m_init
  integer(kind=PY_SSIZE_T_KIND) :: m_index
  type(c_ptr) :: m_copy
end type

type, bind(c) :: PyModuleDef
  type(PyModuleDef_Base) :: m_base
  type(c_ptr) :: m_name
  type(c_ptr) :: m_doc
  integer(kind=PY_SSIZE_T_KIND) :: m_size
  type(c_ptr) :: m_methods
  type(c_ptr) :: m_slots
  type(c_funptr) :: m_traverse
  type(c_funptr) :: m_clear
  type(c_funptr) :: m_free
end type

interface
  subroutine Py_Initialize() bind(c, name="Py_Initialize")
  end subroutine

  subroutine Py_Finalize() bind(c, name="Py_Finalize")
  end subroutine

  function PyImport_ImportModule(a_name) bind(c, name="PyImport_ImportModule") result(m)
    import c_ptr, C_CHAR
    character(kind=C_CHAR), dimension(*) :: a_name
    type(c_ptr) :: m
  end function

  function PyRun_SimpleString(command) bind(c, name="PyRun_SimpleString") result(r)
    import C_INT, C_CHAR
    character(kind=C_CHAR), dimension(*) :: command
    integer(kind=C_INT) :: r
  end function

  function PyList_New(len) bind(c, name="PyList_New") result(r)
    import c_ptr, PY_SSIZE_T_KIND
    integer(kind=PY_SSIZE_T_KIND), value :: len
    type(c_ptr) :: r
  end function

  function PyDict_New() bind(c, name="PyDict_New") result(r)
    import c_ptr
    type(c_ptr) :: r
  end function

  !int PyList_Append(PyObject *list, PyObject *item)
  function PyList_Append(list, item) bind(c, name="PyList_Append") result(r)
    import c_ptr, C_INT
    type(c_ptr), value :: list
    type(c_ptr), value :: item
    integer(kind=C_INT) :: r
  end function
  
  function PyList_Sort(list) bind(c, name="PyList_Sort") result(r)
    import c_ptr, C_INT
    type(c_ptr), value :: list
    integer(kind=C_INT) :: r
  end function
  
  function PyList_Reverse(list) bind(c, name="PyList_Reverse") result(r)
    import c_ptr, C_INT
    type(c_ptr), value :: list
    integer(kind=C_INT) :: r
  end function
  
  !int PyList_Insert(PyObject *list, Py_ssize_t index, PyObject *item)
  function PyList_Insert(list, index, item) bind(c, name="PyList_Insert") result(r)
    import c_ptr, C_INT, PY_SSIZE_T_KIND
    type(c_ptr), value :: list
    integer(kind=PY_SSIZE_T_KIND), value :: index
    type(c_ptr), value :: item
    integer(kind=C_INT) :: r
  end function
  
  !PyObject* PyLong_FromLongLong(long long ival)
  function PyLong_FromLongLong(ival) bind(c, name="PyLong_FromLongLong") result(r)
    import c_ptr, C_LONG_LONG
    integer(kind=C_LONG_LONG), value :: ival
    type(c_ptr) :: r
  end function
  
  !PY_LONG_LONG PyLong_AsLongLongAndOverflow(PyObject *obj, int *overflow)
  function PyLong_AsLongLongAndOverflow(obj, overflow) bind(c, name="PyLong_AsLongLongAndOverflow") result(r)
    import c_ptr, C_LONG_LONG, C_INT
    type(c_ptr), value :: obj
    integer(kind=C_INT) :: overflow
    integer(kind=C_LONG_LONG) :: r
  end function

#ifdef PYTHON2
  !PyObject* PyInt_FromLong(Py_ssize_t ival)
  function PyInt_FromLong(ival) bind(c, name="PyInt_FromLong") result(r)
    import c_ptr, C_LONG
    integer(kind=C_LONG), value :: ival
    type(c_ptr) :: r
  end function
#endif

  !void Py_DecRef(PyObject *o)
  subroutine Py_DecRef(o) bind(c, name="Py_DecRef")
    import c_ptr
    type(c_ptr), value :: o
  end subroutine

  !void Py_IncRef(PyObject *o)
  subroutine Py_IncRef(o) bind(c, name="Py_IncRef")
    import c_ptr
    type(c_ptr), value :: o
  end subroutine

  !PyObject* PyObject_GetItem(PyObject *o, PyObject *key)
  function PyObject_GetItem(o, key) bind(c, name="PyObject_GetItem") result(r)
    import c_ptr
    type(c_ptr), value :: o
    type(c_ptr), value :: key
    type(c_ptr) :: r
  end function

  !int PyObject_SetItem(PyObject *o, PyObject *key, PyObject *v)
  function PyObject_SetItem(o, key, v) bind(c, name="PyObject_SetItem") result(r)
    import c_ptr, C_INT
    type(c_ptr), value :: o
    type(c_ptr), value :: key
    type(c_ptr), value :: v
    integer(kind=C_INT) :: r
  end function
  
  !Py_ssize_t PyObject_Length(PyObject *o)
  function PyObject_Length(o) bind(c, name="PyObject_Length") result(r)
    import c_ptr, PY_SSIZE_T_KIND
    type(c_ptr), value :: o
    integer(kind=PY_SSIZE_T_KIND) :: r
  end function

  !int PyObject_IsTrue(PyObject *o)
  function PyObject_IsTrue(o) bind(c, name="PyObject_IsTrue") result(r)
    import c_ptr, C_INT
    type(c_ptr), value :: o
    integer(kind=C_INT) :: r
  end function
  
  !PyObject* PyObject_Str(PyObject *o)
  function PyObject_Str(o) bind(c, name="PyObject_Str") result(r)
    import c_ptr
    type(c_ptr), value :: o
    type(c_ptr) :: r
  end function

  !int PySequence_SetItem(PyObject *o, Py_ssize_t i, PyObject *v)
  function PySequence_SetItem(o, i, v) bind(c, name="PySequence_SetItem") result(r)
    import c_ptr, C_INT, PY_SSIZE_T_KIND
    type(c_ptr), value :: o
    integer(kind=PY_SSIZE_T_KIND), value :: i
    type(c_ptr), value :: v
    integer(kind=C_INT) :: r
  end function

  !PyObject* PySequence_GetItem(PyObject *o, Py_ssize_t i)
  function PySequence_GetItem(o, i) bind(c, name="PySequence_GetItem") result(r)
    import c_ptr, C_INT, PY_SSIZE_T_KIND
    type(c_ptr), value :: o
    integer(kind=PY_SSIZE_T_KIND), value :: i
    type(c_ptr) :: r
  end function

  !int PyTuple_SetItem(PyObject *p, Py_ssize_t pos, PyObject *o)
  function PyTuple_SetItem(p, pos, o) bind(c, name="PyTuple_SetItem") result(r)
    import c_ptr, C_INT, PY_SSIZE_T_KIND
    type(c_ptr), value :: p
    integer(kind=PY_SSIZE_T_KIND), value :: pos
    type(c_ptr), value :: o
    integer(kind=C_INT) :: r
  end function

  !PyObject* PyTuple_New(Py_ssize_t len)
  function PyTuple_New(len) bind(c, name="PyTuple_New") result(r)
    import c_ptr, PY_SSIZE_T_KIND
    integer(kind=PY_SSIZE_T_KIND), value :: len
    type(c_ptr) :: r
  end function

  !long long PyLong_AsLongLong(PyObject *io)
  function PyLong_AsLongLong(io) bind(c, name="PyLong_AsLongLong") result(r)
    import c_ptr, C_LONG_LONG
    type(c_ptr), value :: io
    integer(kind=C_LONG_LONG) :: r
  end function

  !PyObject* PyFloat_FromDouble(double v)
  function PyFloat_FromDouble(v) bind(c, name="PyFloat_FromDouble") result(r)
    import c_ptr, C_DOUBLE
    real(kind=C_DOUBLE), value :: v
    type(c_ptr) :: r
  end function
  
  !double PyFloat_AsDouble(PyObject *pyfloat)
  function PyFloat_AsDouble(pyfloat) bind(c, name="PyFloat_AsDouble") result(r)
    import c_ptr, C_DOUBLE
    type(c_ptr), value :: pyfloat
    real(kind=C_DOUBLE) :: r
  end function

  function PyComplex_FromDoubles(re, im) bind(c, name="PyComplex_FromDoubles") result(r)
    import c_ptr, C_DOUBLE
    real(kind=C_DOUBLE), value :: re, im
    type(c_ptr) :: r
  end function

  function PyComplex_AsCComplex(obj) bind(c, name="PyComplex_AsCComplex") result(r)
    import c_ptr, Py_complex
    type(c_ptr), value :: obj
    type(Py_complex) :: r
  end function

  function PyErr_Occurred() bind(c, name="PyErr_Occurred") result(r)
    import c_ptr
    type(c_ptr) :: r
  end function

  !void PyErr_Print()
  subroutine PyErr_Print() bind(c, name="PyErr_Print")
  end subroutine

  !void PyErr_Clear()
  subroutine PyErr_Clear() bind(c, name="PyErr_Clear")
  end subroutine

#ifdef PYTHON2
  function PyBytes_FromStringAndSize(v, len) bind(c, name="PyString_FromStringAndSize") result(r)
#else
  function PyBytes_FromStringAndSize(v, len) bind(c, name="PyBytes_FromStringAndSize") result(r)
#endif
    import c_ptr, PY_SSIZE_T_KIND, C_CHAR
    character(kind=C_CHAR), dimension(*), intent(in) :: v
    integer(kind=PY_SSIZE_T_KIND), value :: len
    type(c_ptr) :: r
  end function

#ifdef PYTHON2
  function PyBytes_FromString(v) bind(c, name="PyString_FromString") result(r)
#else
  function PyBytes_FromString(v) bind(c, name="PyBytes_FromString") result(r)
#endif
    import c_ptr, C_CHAR
    character(kind=C_CHAR), dimension(*), intent(in) :: v
    type(c_ptr) :: r
  end function
  
  !char* PyBytes_AsString(PyObject *o)
#ifdef PYTHON2
  function PyBytes_AsString(o) bind(c, name="PyString_AsString") result(r)
#else
  function PyBytes_AsString(o) bind(c, name="PyBytes_AsString") result(r)
#endif
    import c_ptr
    type(c_ptr), value :: o
    type(c_ptr) :: r
  end function

  ! PyObject* PyObject_GetAttr(PyObject *o, PyObject *attr_name)
  function PyObject_GetAttr(o, attr_name) bind(c, name="PyObject_GetAttr") result(r)
    import c_ptr
    type(c_ptr), value :: o, attr_name
    type(c_ptr) :: r
  end function
  
  function PyObject_SetAttr(o, attr_name, v) bind(c, name="PyObject_SetAttr") result(r)
    import c_ptr, C_INT
    type(c_ptr), value :: o, attr_name, v
    integer(kind=C_INT) :: r
  end function
  
  !int PyObject_DelItem(PyObject *o, PyObject *key)
  function PyObject_DelItem(o, key) bind(c, name="PyObject_DelItem") result(r)
    import c_ptr, C_INT
    type(c_ptr), value :: o, key
    integer(kind=C_INT) :: r
  end function
  
  !int PySequence_DelItem(PyObject *o, Py_ssize_t i)
  function PySequence_DelItem(o, i) bind(c, name="PySequence_DelItem") result(r)
    import c_ptr, C_INT, PY_SSIZE_T_KIND
    type(c_ptr), value :: o
    integer(kind=PY_SSIZE_T_KIND), value :: i
    integer(kind=C_INT) :: r
  end function  

  !PyObject* PyObject_Call(PyObject *callable_object, PyObject *args, PyObject *kw)
  function PyObject_Call(callable_object, args, kw) bind(c, name="PyObject_Call") result(r)
    import c_ptr
    type(c_ptr), value :: callable_object, args, kw
    type(c_ptr) :: r
  end function

  !PyObject *PyMemoryView_FromBuffer(Py_buffer *view)
  function PyMemoryView_FromBuffer(view) bind(c, name="PyMemoryView_FromBuffer") result(r)
    import Py_buffer, c_ptr
    type(Py_buffer) :: view
    type(c_ptr) :: r
  end function

  !PyObject *PyMemoryView_FromObject(PyObject *obj)
  function PyMemoryView_FromObject(obj) bind(c, name="PyMemoryView_FromObject") result(r)
    import c_ptr
    type(c_ptr), value :: obj
    type(c_ptr) :: r
  end function

  !int PyObject_GetBuffer(PyObject *obj, Py_buffer *view, int flags)
  function PyObject_GetBuffer(obj, view, flags) bind(c, name="PyObject_GetBuffer") result(r)
    import Py_buffer, c_ptr, C_INT
    type(c_ptr), value :: obj
    type(Py_buffer) :: view
    integer(kind=C_INT), value :: flags
    integer(kind=C_INT) :: r
  end function

  !int PyBuffer_IsContiguous(Py_buffer *view, char fortran)
  function PyBuffer_IsContiguous(view, fortran) bind(c, name="PyBuffer_IsContiguous") result(r)
    import Py_buffer, C_INT, C_CHAR
    type(Py_buffer) :: view
    character(kind=C_CHAR), value :: fortran
    integer(kind=C_INT) :: r
  end function

  !void PyBuffer_Release(Py_buffer *view)
  subroutine PyBuffer_Release(view) bind(c, name="PyBuffer_Release")
    import Py_buffer
    type(Py_buffer) :: view
  end subroutine

  !int PyObject_IsInstance(PyObject *inst, PyObject *cls)
  function PyObject_IsInstance(inst, cls) bind(c, name="PyObject_IsInstance") result(r)
    import C_INT, c_ptr
    type(c_ptr), value :: inst
    type(c_ptr), value :: cls
    integer(kind=C_INT) :: r
  end function

  !int PyType_IsSubtype(PyTypeObject *a, PyTypeObject *b)
  function PyType_IsSubtype(a, b) bind(c, name="PyType_IsSubtype") result(r)
    import C_INT, c_ptr
    type(c_ptr), value :: a
    type(c_ptr), value :: b
    integer(kind=C_INT) :: r
  end function

  !PyObject* PyBool_FromLong(long v)
  function PyBool_FromLong(v) bind(c, name="PyBool_FromLong") result(r)
    import C_LONG, c_ptr
    integer(kind=C_LONG), value :: v
    type(c_ptr) :: r
  end function

  !PyObject* PyUnicode_DecodeUTF8(const char *s, Py_ssize_t size, const char *errors)
#ifndef PYTHON2
  function PyUnicode_DecodeUTF8(s, size, errors) bind(c, name="PyUnicode_DecodeUTF8") result(r)
#endif

#ifdef PYTHON2
#ifdef PYTHON_NARROW
  function PyUnicode_DecodeUTF8(s, size, errors) bind(c, name="PyUnicodeUCS2_DecodeUTF8") result(r)  
#else
  function PyUnicode_DecodeUTF8(s, size, errors) bind(c, name="PyUnicodeUCS4_DecodeUTF8") result(r)
#endif
#endif
    import c_ptr, PY_SSIZE_T_KIND, C_CHAR
    character(kind=C_CHAR), dimension(*) :: s
    integer(kind=PY_SSIZE_T_KIND), value :: size
    character(kind=C_CHAR), dimension(*) :: errors
    type(c_ptr) :: r
  end function
  
#ifndef PYTHON2
  ! Since Python 3.3 in C-API
  !char* PyUnicode_AsUTF8AndSize(PyObject *unicode, Py_ssize_t *size)
  function PyUnicode_AsUTF8AndSize(unicode, size) bind(c, name="PyUnicode_AsUTF8AndSize") result(r)
    import c_ptr, PY_SSIZE_T_KIND
    type(c_ptr), value :: unicode
    integer(kind=PY_SSIZE_T_KIND) :: size
    type(c_ptr) :: r
  end function  
#endif

  function PyEval_GetBuiltins() bind(c, name="PyEval_GetBuiltins") result(r)
    import c_ptr
    type(c_ptr) :: r
  end function
  
  !PyObject* PyDict_GetItemString(PyObject *p, const char *key)
  function PyDict_GetItemString(p, key) bind(c, name="PyDict_GetItemString") result(r)
    import c_ptr, C_CHAR
    type(c_ptr), value :: p
    character(kind=C_CHAR), dimension(*) :: key
    type(c_ptr) :: r
  end function

!void PyErr_SetString(PyObject *type, const char *message)
subroutine PyErr_SetString(a_type, message) bind(c, name="PyErr_SetString")
  import c_ptr, C_CHAR
  type(c_ptr), value :: a_type
  character(kind=C_CHAR), dimension(*) :: message
end subroutine

!int PyErr_GivenExceptionMatches(PyObject *given, PyObject *exc)
function PyErr_GivenExceptionMatches(given, exc) bind(c, name="PyErr_GivenExceptionMatches") result(r)
  import c_ptr, C_INT
  type(c_ptr), value :: given, exc
  integer(kind=C_INT) :: r
end function

function PySequence_Tuple(o) bind(c, name="PySequence_Tuple") result(r)
  import c_ptr
  type(c_ptr), value :: o
  type(c_ptr) :: r
end function

function PySequence_List(o) bind(c, name="PySequence_List") result(r)
  import c_ptr
  type(c_ptr), value :: o
  type(c_ptr) :: r
end function

subroutine PyDict_Clear(p) bind(c, name="PyDict_Clear")
  import c_ptr
  type(c_ptr), value :: p
end subroutine

function PyDict_Copy(p) bind(c, name="PyDict_Copy") result(r)
  import c_ptr
  type(c_ptr), value :: p
  type(c_ptr) :: r
end function

!PyObject* PyDict_Items(PyObject *p)
function PyDict_Items(p) bind(c, name="PyDict_Items") result(r)
  import c_ptr
  type(c_ptr), value :: p
  type(c_ptr) :: r
end function

!PyObject* PyDict_Keys(PyObject *p)
function PyDict_Keys(p) bind(c, name="PyDict_Keys") result(r)
  import c_ptr
  type(c_ptr), value :: p
  type(c_ptr) :: r
end function

!PyObject* PyDict_Values(PyObject *p)
function PyDict_Values(p) bind(c, name="PyDict_Values") result(r)
  import c_ptr
  type(c_ptr), value :: p
  type(c_ptr) :: r
end function

!void PyBuffer_FillContiguousStrides(int ndim, Py_ssize_t *shape, Py_ssize_t *strides, Py_ssize_t itemsize, char order)
subroutine PyBuffer_FillContiguousStrides(ndim, shape, strides, itemsize, order) bind(c, name="PyBuffer_FillContiguousStrides")
  import c_ptr, C_INT, C_CHAR, PY_SSIZE_T_KIND
  integer(kind=C_INT), value :: ndim
  type(c_ptr), value :: shape
  type(c_ptr), value :: strides
  integer(kind=PY_SSIZE_T_KIND), value :: itemsize
  character(kind=C_CHAR), value :: order
end subroutine

function PySequence_Contains(o, a_value) bind(c, name="PySequence_Contains") result(r)
  import c_ptr, C_INT
  type(c_ptr), value :: o
  type(c_ptr), value :: a_value  
  integer(kind=C_INT) :: r
end function

function PySequence_Index(o, a_value) bind(c, name="PySequence_Index") result(r)
  import c_ptr, PY_SSIZE_T_KIND
  type(c_ptr), value :: o
  type(c_ptr), value :: a_value  
  integer(kind=PY_SSIZE_T_KIND) :: r
end function

function PySequence_Count(o, a_value) bind(c, name="PySequence_Count") result(r)
  import c_ptr, PY_SSIZE_T_KIND
  type(c_ptr), value :: o
  type(c_ptr), value :: a_value  
  integer(kind=PY_SSIZE_T_KIND) :: r
end function

function PyMapping_HasKey(o, a_value) bind(c, name="PyMapping_HasKey") result(r)
  import c_ptr, C_INT
  type(c_ptr), value :: o
  type(c_ptr), value :: a_value  
  integer(kind=C_INT) :: r
end function

function PySequence_Concat(o1, o2) bind(c, name="PySequence_Concat") result(r)
  import c_ptr
  type(c_ptr), value :: o1, o2
  type(c_ptr) :: r 
end function

!PyObject *PySys_GetObject(const char *name)
function PySys_GetObject(a_name) bind(c, name="PySys_GetObject") result(r)
  import c_ptr, C_CHAR
  character(kind=C_CHAR), dimension(*) :: a_name
  type(c_ptr) :: r
end function

#ifndef PYTHON2
#ifndef Py_DEBUG
function PyModule_Create2(def, module_api_version) bind(c, name="PyModule_Create2") result(r)
#else
function PyModule_Create2(def, module_api_version) bind(c, name="PyModule_Create2TraceRefs") result(r)
#endif
  import c_ptr, C_INT
  type(c_ptr), value :: def
  integer(kind=C_INT), value :: module_api_version
  type(c_ptr) :: r
end function
#endif

#ifdef PYTHON2
!Python 2 only
!PyObject* Py_InitModule4(char *name, PyMethodDef *methods, char *doc, PyObject *self, int apiver)
#ifndef PYTHON2_32
#ifndef Py_DEBUG
function Py_InitModule4(a_name, methods, doc, self, apiver) bind(c, name="Py_InitModule4_64") result(r)
#else
function Py_InitModule4(a_name, methods, doc, self, apiver) bind(c, name="Py_InitModule4TraceRefs_64") result(r)
#endif
#else
#ifndef Py_DEBUG
function Py_InitModule4(a_name, methods, doc, self, apiver) bind(c, name="Py_InitModule4") result(r)
#else
function Py_InitModule4(a_name, methods, doc, self, apiver) bind(c, name="Py_InitModule4TraceRefs") result(r)
#endif
#endif
  import c_ptr, C_CHAR, C_INT
  character(kind=C_CHAR), dimension(*) :: a_name
  type(c_ptr), value :: methods
  character(kind=C_CHAR), dimension(*) :: doc
  type(c_ptr), value :: self
  integer(kind=C_INT), value :: apiver
  type(c_ptr) :: r
end function
#endif

!int PyModule_AddObject(PyObject *module, const char *name, PyObject *value)
function PyModule_AddObject(a_module, a_name, a_value) bind(c, name="PyModule_AddObject") result(r)
  import c_ptr, C_CHAR, C_INT
  type(c_ptr), value :: a_module
  character(kind=C_CHAR), dimension(*) :: a_name
  type(c_ptr), value :: a_value
  integer(kind=C_INT) :: r
end function

#ifdef PYTHON2
! Old-style Python2-only buffer protocol API function
!PyObject* PyBuffer_FromReadWriteMemory(void *ptr, Py_ssize_t size)
function PyBuffer_FromReadWriteMemory(ptr, the_size) bind(c, name="PyBuffer_FromReadWriteMemory") result(r)
  import c_ptr, PY_SSIZE_T_KIND
  type(c_ptr), value :: ptr
  integer(kind=PY_SSIZE_T_KIND), value :: the_size
  type(c_ptr) :: r
end function
#endif

function strcmp(s1, s2) bind(c) result(r)
  import c_ptr, C_INT
  type(c_ptr), value :: s1, s2
  integer(kind=C_INT) :: r
end function

end interface

interface box_value
#ifdef PYTHON2
  module procedure box_value_int32
  module procedure box_value_int64
#else
  module procedure box_value_int32_as_long
  module procedure box_value_int64_as_long
#endif
  module procedure box_value_real32
  module procedure box_value_real64
  module procedure box_value_complex_real32
  module procedure box_value_complex_real64
  module procedure box_value_logical
  module procedure box_value_chars
  module procedure box_value_char_1d
end interface

interface unbox_value
  module procedure unbox_value_int32
  module procedure unbox_value_int64
  module procedure unbox_value_real32
  module procedure unbox_value_real64
  module procedure unbox_value_complex_real32
  module procedure unbox_value_complex_real64
  module procedure unbox_value_logical
  module procedure unbox_value_chars
  module procedure unbox_value_char_1d
end interface

interface tuple_from_array
  module procedure tuple_from_array_int32
  module procedure tuple_from_array_int64
end interface

!--------- High-level API to Python's datastructures -------------------






!> Type to represent an arbitrary Python object
type object
  private
  type(c_ptr) :: py_object = C_NULL_PTR

  contains
    !> Call to allow for freeing of resources of this object.
    procedure, public :: destroy => object_destroy
    !> Get value of an attribute of this object
    procedure, public :: getattribute => object_getattribute  ! TODO: make generic?
    !> Set value of an attribute of this object
    procedure, public :: setattr => object_setattr
    !> Delete an attribute of this object
    procedure, public :: delattr => object_delattr
    !> Get c_ptr representation of this object. For development of Python extension modules
    procedure, public :: get_c_ptr => object_get_c_ptr
end type

!> Type that represents a "class object". In Python class objects have the type 'type' which we name type_py here.
type, extends(object) :: type_py

end type

!> Abstract type that represents sequence objects. Elements of a sequence can be accessed by an index.
type, abstract, extends(object) :: Sequence
  contains
      procedure, private :: sequence_len_int32
      !> Get the length of the object (number of elements).
      generic, public :: len => sequence_len_int32
      procedure, private :: sequence_count_int32
      generic, public :: count => sequence_count_int32
      
      !index - does not support optional start and stop indices as the Python function does
      procedure, private :: sequence_index_int32
      !> Get the first index of a value.
      generic, public :: index => sequence_index_int32
    
      procedure, private :: sequence_getitem_int32_object
      !> Get item at a certain index
      generic, public :: getitem => sequence_getitem_int32_object
        procedure, private :: sequence_getitem_int32_int32
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int32_int32
        procedure, private :: sequence_getitem_int32_int64
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int32_int64
        procedure, private :: sequence_getitem_int32_real32
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int32_real32
        procedure, private :: sequence_getitem_int32_real64
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int32_real64
        procedure, private :: sequence_getitem_int32_complex_real32
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int32_complex_real32
        procedure, private :: sequence_getitem_int32_complex_real64
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int32_complex_real64
        procedure, private :: sequence_getitem_int32_logical
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int32_logical
        procedure, private :: sequence_getitem_int32_char_1d
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int32_char_1d
        procedure, private :: sequence_getitem_int32_chars
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int32_chars

      procedure, private :: sequence_len_int64
      !> Get the length of the object (number of elements).
      generic, public :: len => sequence_len_int64
      procedure, private :: sequence_count_int64
      generic, public :: count => sequence_count_int64
      
      !index - does not support optional start and stop indices as the Python function does
      procedure, private :: sequence_index_int64
      !> Get the first index of a value.
      generic, public :: index => sequence_index_int64
    
      procedure, private :: sequence_getitem_int64_object
      !> Get item at a certain index
      generic, public :: getitem => sequence_getitem_int64_object
        procedure, private :: sequence_getitem_int64_int32
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int64_int32
        procedure, private :: sequence_getitem_int64_int64
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int64_int64
        procedure, private :: sequence_getitem_int64_real32
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int64_real32
        procedure, private :: sequence_getitem_int64_real64
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int64_real64
        procedure, private :: sequence_getitem_int64_complex_real32
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int64_complex_real32
        procedure, private :: sequence_getitem_int64_complex_real64
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int64_complex_real64
        procedure, private :: sequence_getitem_int64_logical
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int64_logical
        procedure, private :: sequence_getitem_int64_char_1d
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int64_char_1d
        procedure, private :: sequence_getitem_int64_chars
        !> Get item at a certain index
        generic, public :: getitem => sequence_getitem_int64_chars

    
    !> Checks if a given item is contained in the sequence.
    procedure, public :: contains => sequence_contains
end type

!> Abstract type that represents a sequence, whose items can be changed.
type, abstract, extends(Sequence) :: MutableSequence
  contains
    procedure, private :: mutablesequence_setitem_int32_object
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int32_object
    procedure, private :: mutablesequence_setitem_int32_int32
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int32_int32
    procedure, private :: mutablesequence_setitem_int32_int64
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int32_int64
    procedure, private :: mutablesequence_setitem_int32_real32
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int32_real32
    procedure, private :: mutablesequence_setitem_int32_real64
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int32_real64
    procedure, private :: mutablesequence_setitem_int32_complex_real32
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int32_complex_real32
    procedure, private :: mutablesequence_setitem_int32_complex_real64
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int32_complex_real64
    procedure, private :: mutablesequence_setitem_int32_logical
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int32_logical
    procedure, private :: mutablesequence_setitem_int32_char_1d
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int32_char_1d
    procedure, private :: mutablesequence_setitem_int32_chars
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int32_chars
    procedure, private :: mutablesequence_setitem_int64_object
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int64_object
    procedure, private :: mutablesequence_setitem_int64_int32
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int64_int32
    procedure, private :: mutablesequence_setitem_int64_int64
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int64_int64
    procedure, private :: mutablesequence_setitem_int64_real32
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int64_real32
    procedure, private :: mutablesequence_setitem_int64_real64
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int64_real64
    procedure, private :: mutablesequence_setitem_int64_complex_real32
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int64_complex_real32
    procedure, private :: mutablesequence_setitem_int64_complex_real64
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int64_complex_real64
    procedure, private :: mutablesequence_setitem_int64_logical
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int64_logical
    procedure, private :: mutablesequence_setitem_int64_char_1d
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int64_char_1d
    procedure, private :: mutablesequence_setitem_int64_chars
    !> Set an item at a given index
    generic, public :: setitem => mutablesequence_setitem_int64_chars
end type

!> Abstract type that represents a sequence, whose items can not be changed.
type, abstract, extends(Sequence) :: ImmutableSequence

end type

!> Type that corresponds to a Python list.
type, extends(MutableSequence) :: list
  contains
    procedure, private :: list_append_object
    !> Append an item at the end of a list
    generic, public :: append => list_append_object
    
    !> Creates a copy of a list
    procedure, public :: copy => list_copy
    !> Sorts the list.
    procedure, public :: sort => list_sort
    !> Reverses a list.
    procedure, public :: reverse => list_reverse
    !> Concatenates another list at the end of a list.
    procedure, public :: add => list_add

    procedure, private :: list_insert_int32
    !> Inserts item at given index.
    generic, public :: insert => list_insert_int32
    procedure, private :: list_delitem_int32
    !> Deletes item at given index from list.
    generic, public :: delitem => list_delitem_int32
    procedure, private :: list_insert_int64
    !> Inserts item at given index.
    generic, public :: insert => list_insert_int64
    procedure, private :: list_delitem_int64
    !> Deletes item at given index from list.
    generic, public :: delitem => list_delitem_int64
    
    procedure, private :: list_append_int32
    !> Append an item at the end of a list
    generic, public :: append => list_append_int32
    procedure, private :: list_append_int64
    !> Append an item at the end of a list
    generic, public :: append => list_append_int64
    procedure, private :: list_append_real32
    !> Append an item at the end of a list
    generic, public :: append => list_append_real32
    procedure, private :: list_append_real64
    !> Append an item at the end of a list
    generic, public :: append => list_append_real64
    procedure, private :: list_append_complex_real32
    !> Append an item at the end of a list
    generic, public :: append => list_append_complex_real32
    procedure, private :: list_append_complex_real64
    !> Append an item at the end of a list
    generic, public :: append => list_append_complex_real64
    procedure, private :: list_append_logical
    !> Append an item at the end of a list
    generic, public :: append => list_append_logical
    procedure, private :: list_append_char_1d
    !> Append an item at the end of a list
    generic, public :: append => list_append_char_1d
    procedure, private :: list_append_chars
    !> Append an item at the end of a list
    generic, public :: append => list_append_chars

end type

!> Abstract type that represents a datastructure that maps keys to values.
type, abstract, extends(object) :: Mapping
  contains
     procedure, private :: mapping_getitem_object_object
     !> Get value at a given key. KeyError if key does not exist
     generic, public :: getitem => mapping_getitem_object_object   
     procedure, private :: mapping_setitem_object_object
     !> Inserts value at given key. Sets value if key already exists.
     generic, public :: setitem => mapping_setitem_object_object
     procedure, private :: mapping_delitem_object
     !> Delete key-value pair with given key.
     generic, public :: delitem => mapping_delitem_object
     
       procedure, private :: mapping_getitem_int32_object
       !> Get value at a given key. KeyError if key does not exist
       generic, public :: getitem => mapping_getitem_int32_object   
       procedure, private :: mapping_setitem_int32_object
       !> Inserts value at given key. Sets value if key already exists.
       generic, public :: setitem => mapping_setitem_int32_object
       !> Delete key-value pair with given key.
       procedure, private :: mapping_delitem_int32
       generic, public :: delitem => mapping_delitem_int32
         procedure, private :: mapping_getitem_int32_int32
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int32_int32    
         procedure, private :: mapping_setitem_int32_int32
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int32_int32
         procedure, private :: mapping_getitem_int32_int64
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int32_int64    
         procedure, private :: mapping_setitem_int32_int64
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int32_int64
         procedure, private :: mapping_getitem_int32_real32
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int32_real32    
         procedure, private :: mapping_setitem_int32_real32
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int32_real32
         procedure, private :: mapping_getitem_int32_real64
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int32_real64    
         procedure, private :: mapping_setitem_int32_real64
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int32_real64
         procedure, private :: mapping_getitem_int32_complex_real32
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int32_complex_real32    
         procedure, private :: mapping_setitem_int32_complex_real32
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int32_complex_real32
         procedure, private :: mapping_getitem_int32_complex_real64
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int32_complex_real64    
         procedure, private :: mapping_setitem_int32_complex_real64
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int32_complex_real64
         procedure, private :: mapping_getitem_int32_logical
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int32_logical    
         procedure, private :: mapping_setitem_int32_logical
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int32_logical
         procedure, private :: mapping_getitem_int32_char_1d
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int32_char_1d    
         procedure, private :: mapping_setitem_int32_char_1d
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int32_char_1d
         procedure, private :: mapping_getitem_int32_chars
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int32_chars    
         procedure, private :: mapping_setitem_int32_chars
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int32_chars
       procedure, private :: mapping_getitem_int64_object
       !> Get value at a given key. KeyError if key does not exist
       generic, public :: getitem => mapping_getitem_int64_object   
       procedure, private :: mapping_setitem_int64_object
       !> Inserts value at given key. Sets value if key already exists.
       generic, public :: setitem => mapping_setitem_int64_object
       !> Delete key-value pair with given key.
       procedure, private :: mapping_delitem_int64
       generic, public :: delitem => mapping_delitem_int64
         procedure, private :: mapping_getitem_int64_int32
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int64_int32    
         procedure, private :: mapping_setitem_int64_int32
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int64_int32
         procedure, private :: mapping_getitem_int64_int64
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int64_int64    
         procedure, private :: mapping_setitem_int64_int64
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int64_int64
         procedure, private :: mapping_getitem_int64_real32
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int64_real32    
         procedure, private :: mapping_setitem_int64_real32
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int64_real32
         procedure, private :: mapping_getitem_int64_real64
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int64_real64    
         procedure, private :: mapping_setitem_int64_real64
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int64_real64
         procedure, private :: mapping_getitem_int64_complex_real32
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int64_complex_real32    
         procedure, private :: mapping_setitem_int64_complex_real32
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int64_complex_real32
         procedure, private :: mapping_getitem_int64_complex_real64
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int64_complex_real64    
         procedure, private :: mapping_setitem_int64_complex_real64
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int64_complex_real64
         procedure, private :: mapping_getitem_int64_logical
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int64_logical    
         procedure, private :: mapping_setitem_int64_logical
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int64_logical
         procedure, private :: mapping_getitem_int64_char_1d
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int64_char_1d    
         procedure, private :: mapping_setitem_int64_char_1d
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int64_char_1d
         procedure, private :: mapping_getitem_int64_chars
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_int64_chars    
         procedure, private :: mapping_setitem_int64_chars
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_int64_chars
       procedure, private :: mapping_getitem_chars_object
       !> Get value at a given key. KeyError if key does not exist
       generic, public :: getitem => mapping_getitem_chars_object   
       procedure, private :: mapping_setitem_chars_object
       !> Inserts value at given key. Sets value if key already exists.
       generic, public :: setitem => mapping_setitem_chars_object
       !> Delete key-value pair with given key.
       procedure, private :: mapping_delitem_chars
       generic, public :: delitem => mapping_delitem_chars
         procedure, private :: mapping_getitem_chars_int32
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_chars_int32    
         procedure, private :: mapping_setitem_chars_int32
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_chars_int32
         procedure, private :: mapping_getitem_chars_int64
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_chars_int64    
         procedure, private :: mapping_setitem_chars_int64
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_chars_int64
         procedure, private :: mapping_getitem_chars_real32
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_chars_real32    
         procedure, private :: mapping_setitem_chars_real32
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_chars_real32
         procedure, private :: mapping_getitem_chars_real64
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_chars_real64    
         procedure, private :: mapping_setitem_chars_real64
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_chars_real64
         procedure, private :: mapping_getitem_chars_complex_real32
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_chars_complex_real32    
         procedure, private :: mapping_setitem_chars_complex_real32
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_chars_complex_real32
         procedure, private :: mapping_getitem_chars_complex_real64
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_chars_complex_real64    
         procedure, private :: mapping_setitem_chars_complex_real64
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_chars_complex_real64
         procedure, private :: mapping_getitem_chars_logical
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_chars_logical    
         procedure, private :: mapping_setitem_chars_logical
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_chars_logical
         procedure, private :: mapping_getitem_chars_char_1d
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_chars_char_1d    
         procedure, private :: mapping_setitem_chars_char_1d
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_chars_char_1d
         procedure, private :: mapping_getitem_chars_chars
         !> Get value at a given key. KeyError if key does not exist
         generic, public :: getitem => mapping_getitem_chars_chars    
         procedure, private :: mapping_setitem_chars_chars
         !> Inserts value at given key. Sets value if key already exists.
         generic, public :: setitem => mapping_setitem_chars_chars

      procedure, private :: mapping_len_int32
      !> Get number of key-value pairs.
      generic, public :: len => mapping_len_int32
      procedure, private :: mapping_len_int64
      !> Get number of key-value pairs.
      generic, public :: len => mapping_len_int64

    !> Checks if key is contained in datastructure.
    procedure, public :: mapping_contains
    
end type

type, extends(Mapping) :: dict
  contains
      !> Removes all key-value pairs from dictionary.
      procedure, public :: clear => dict_clear
      !> Creates a copy of dict
      procedure, public :: copy => dict_copy
      !> Creates a list of a dict's keys 
      procedure, public :: keys => dict_keys
      !> Creates a list of a dict's key-value pairs.
      procedure, public :: items => dict_items
      !> Creates a list of a dict's values
      procedure, public :: values => dict_values

      procedure, private :: dict_get_object_object 
      !> Get value at a given key. If key does not exist, return a default value.
      generic, public :: get => dict_get_object_object    
        procedure, private :: dict_get_int32_object 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int32_object
        procedure, private :: dict_get_int32_int32 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int32_int32        
        procedure, private :: dict_get_int32_int64 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int32_int64        
        procedure, private :: dict_get_int32_real32 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int32_real32        
        procedure, private :: dict_get_int32_real64 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int32_real64        
        procedure, private :: dict_get_int32_complex_real32 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int32_complex_real32        
        procedure, private :: dict_get_int32_complex_real64 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int32_complex_real64        
        procedure, private :: dict_get_int32_logical 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int32_logical        
        procedure, private :: dict_get_int32_char_1d 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int32_char_1d        
        procedure, private :: dict_get_int32_chars 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int32_chars        
        procedure, private :: dict_get_int64_object 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int64_object
        procedure, private :: dict_get_int64_int32 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int64_int32        
        procedure, private :: dict_get_int64_int64 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int64_int64        
        procedure, private :: dict_get_int64_real32 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int64_real32        
        procedure, private :: dict_get_int64_real64 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int64_real64        
        procedure, private :: dict_get_int64_complex_real32 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int64_complex_real32        
        procedure, private :: dict_get_int64_complex_real64 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int64_complex_real64        
        procedure, private :: dict_get_int64_logical 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int64_logical        
        procedure, private :: dict_get_int64_char_1d 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int64_char_1d        
        procedure, private :: dict_get_int64_chars 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_int64_chars        
        procedure, private :: dict_get_chars_object 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_chars_object
        procedure, private :: dict_get_chars_int32 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_chars_int32        
        procedure, private :: dict_get_chars_int64 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_chars_int64        
        procedure, private :: dict_get_chars_real32 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_chars_real32        
        procedure, private :: dict_get_chars_real64 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_chars_real64        
        procedure, private :: dict_get_chars_complex_real32 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_chars_complex_real32        
        procedure, private :: dict_get_chars_complex_real64 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_chars_complex_real64        
        procedure, private :: dict_get_chars_logical 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_chars_logical        
        procedure, private :: dict_get_chars_char_1d 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_chars_char_1d        
        procedure, private :: dict_get_chars_chars 
        !> Get value at a given key. If key does not exist, return a default value.
        generic, public :: get => dict_get_chars_chars        

      !> Get value at a given key. If key does not exist, set value at key to default value and return default.
      procedure, public :: setdefault => dict_setdefault_object_object       
end type

!> Type that corresponds to a Python tuple. Create with tuple_create.
type, extends(ImmutableSequence) :: tuple
  contains
    procedure, private :: tuple_setitem_int32_object
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int32_object
    procedure, private :: tuple_setitem_int32_int32
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int32_int32
    procedure, private :: tuple_setitem_int32_int64
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int32_int64
    procedure, private :: tuple_setitem_int32_real32
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int32_real32
    procedure, private :: tuple_setitem_int32_real64
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int32_real64
    procedure, private :: tuple_setitem_int32_complex_real32
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int32_complex_real32
    procedure, private :: tuple_setitem_int32_complex_real64
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int32_complex_real64
    procedure, private :: tuple_setitem_int32_logical
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int32_logical
    procedure, private :: tuple_setitem_int32_char_1d
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int32_char_1d
    procedure, private :: tuple_setitem_int32_chars
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int32_chars
    procedure, private :: tuple_setitem_int64_object
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int64_object
    procedure, private :: tuple_setitem_int64_int32
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int64_int32
    procedure, private :: tuple_setitem_int64_int64
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int64_int64
    procedure, private :: tuple_setitem_int64_real32
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int64_real32
    procedure, private :: tuple_setitem_int64_real64
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int64_real64
    procedure, private :: tuple_setitem_int64_complex_real32
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int64_complex_real32
    procedure, private :: tuple_setitem_int64_complex_real64
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int64_complex_real64
    procedure, private :: tuple_setitem_int64_logical
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int64_logical
    procedure, private :: tuple_setitem_int64_char_1d
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int64_char_1d
    procedure, private :: tuple_setitem_int64_chars
    !> Sets item at given index. One must set all items before use of tuple.
    generic, public :: setitem => tuple_setitem_int64_chars

    !> Concatenates tuples.
    procedure, public :: add => tuple_add
end type

!> Creates a tuple with a given number of items.
interface tuple_create
  module procedure tuple_create_int32
  module procedure tuple_create_int64
  module procedure tuple_create_object
end interface

!> Creates a list. Create as empty list or from other object.
interface list_create
  module procedure list_create_empty
  module procedure list_create_object
end interface

!> Type corresponding to Python 'bytes'.
type, extends(ImmutableSequence) :: bytes

end type

!> Creates a bytes object from Fortran character string or character array.
interface bytes_create
  module procedure bytes_create_chars
  module procedure bytes_create_char_1d  
end interface

!> Type corresponding to Python 'str' - Python's string type.
type, extends(ImmutableSequence) :: str

end type

!> Creates a str object from Fortran character string or character array.
interface str_create
  module procedure str_create_chars
  module procedure str_create_char_1d  
end interface

!> Type corresponding to Python 2 'unicode' or Python 3 'str'.
type, extends(ImmutableSequence) :: unicode

end type

!> Creates a unicode string from Fortran character string or character array.
interface unicode_create
  module procedure unicode_create_chars
  module procedure unicode_create_char_1d  
end interface

!> Type representing a Python module 
type, extends(object) :: module_py
end type

!> Type representing Python's 'None'. Create with NoneType_create.
type, extends(object) :: NoneType
end type

!> Interface to call a Python objects (methods or other callables)
!> Arguments (optional) have to be passed as tuple.
!> Keyword arguments (optional) have to be passed as dict.
interface call_py
  module procedure call_py_attribute
  module procedure call_py_object
  module procedure call_py_object_nokwargs
  module procedure call_py_object_noargs
  module procedure call_py_object_only_kwargs
end interface

!> Interface to call a Python objects (methods or other callables), ignoring the return value.
!> Arguments (optional) have to be passed as tuple.
!> Keyword arguments (optional) have to be passed as dict.
interface call_py_noret
  module procedure call_py_noret_attribute
  module procedure call_py_noret_object
end interface

!> Creates a new reference to an object. Python equivalent: lhs = rhs
!> Do not forget to destroy lhs after use.
interface assign_py
  module procedure assign_py_object
  module procedure assign_py_list
  module procedure assign_py_tuple
  module procedure assign_py_dict
  module procedure assign_py_ndarray
  module procedure assign_py_type_py
  module procedure assign_py_module_py
  module procedure assign_py_NoneType
end interface

! or extends MutableSequence? probably better as object since indexing is fancier than for a sequence?
!> Type that represents a Numpy array
type, extends(object) :: ndarray
  contains
    procedure, private :: get_data_int32_1d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_int32_1d
    procedure, private :: get_data_int64_1d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_int64_1d
    procedure, private :: get_data_real32_1d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_real32_1d
    procedure, private :: get_data_real64_1d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_real64_1d
    procedure, private :: get_data_complex_real32_1d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_complex_real32_1d
    procedure, private :: get_data_complex_real64_1d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_complex_real64_1d
    procedure, private :: get_data_int32_2d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_int32_2d
    procedure, private :: get_data_int64_2d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_int64_2d
    procedure, private :: get_data_real32_2d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_real32_2d
    procedure, private :: get_data_real64_2d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_real64_2d
    procedure, private :: get_data_complex_real32_2d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_complex_real32_2d
    procedure, private :: get_data_complex_real64_2d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_complex_real64_2d
    procedure, private :: get_data_int32_3d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_int32_3d
    procedure, private :: get_data_int64_3d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_int64_3d
    procedure, private :: get_data_real32_3d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_real32_3d
    procedure, private :: get_data_real64_3d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_real64_3d
    procedure, private :: get_data_complex_real32_3d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_complex_real32_3d
    procedure, private :: get_data_complex_real64_3d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_complex_real64_3d
    procedure, private :: get_data_int32_4d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_int32_4d
    procedure, private :: get_data_int64_4d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_int64_4d
    procedure, private :: get_data_real32_4d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_real32_4d
    procedure, private :: get_data_real64_4d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_real64_4d
    procedure, private :: get_data_complex_real32_4d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_complex_real32_4d
    procedure, private :: get_data_complex_real64_4d
    !> Retrieve a Fortran pointer to the array data.
    generic, public :: get_data => get_data_complex_real64_4d
  !> Transpose the array.
  procedure, public :: transpose => ndarray_transpose
  !> Create a copy (with its own data) of the ndarray
  procedure, public :: copy => ndarray_copy
  !> Checks if the array has Fortran or C storage order (contiguous array)
  procedure, public :: is_ordered => ndarray_is_ordered
  !> Get numpy.dtype type identifier of the array. Python equivalent: self.dtype.name
  procedure, public :: get_dtype_name => ndarray_get_dtype_name
  
  procedure, private :: ndarray_ndim_int32
  procedure, private :: ndarray_ndim_int64
  !> Get dimension of array
  generic, public :: ndim => ndarray_ndim_int32
  !> Get dimension of array
  generic, public :: ndim => ndarray_ndim_int64
end type

!> Create a ndarray from a Fortran array. The ndarray will be a copy
!> of the Fortran array.
interface ndarray_create
  module procedure ndarray_create_int32_1d
  module procedure ndarray_create_int64_1d
  module procedure ndarray_create_real32_1d
  module procedure ndarray_create_real64_1d
  module procedure ndarray_create_complex_real32_1d
  module procedure ndarray_create_complex_real64_1d
  module procedure ndarray_create_int32_2d
  module procedure ndarray_create_int64_2d
  module procedure ndarray_create_real32_2d
  module procedure ndarray_create_real64_2d
  module procedure ndarray_create_complex_real32_2d
  module procedure ndarray_create_complex_real64_2d
  module procedure ndarray_create_int32_3d
  module procedure ndarray_create_int64_3d
  module procedure ndarray_create_real32_3d
  module procedure ndarray_create_real64_3d
  module procedure ndarray_create_complex_real32_3d
  module procedure ndarray_create_complex_real64_3d
  module procedure ndarray_create_int32_4d
  module procedure ndarray_create_int64_4d
  module procedure ndarray_create_real32_4d
  module procedure ndarray_create_real64_4d
  module procedure ndarray_create_complex_real32_4d
  module procedure ndarray_create_complex_real64_4d
end interface

!> Create a ndarray wrapper for a Fortran array. NO copy is made, changes 
!> to the Fortran array affect the ndarray and vice versa.
!>
!> Only pass contiguous Fortran arrays to this function. This is not checked!
!>
!> The lifetime of the Fortran array must be at least as long as the
!> ndarray is in use: beware of deallocation and compiler generated
!> temporary arrays.
!>
!> Since the Fortran array is used as underlying buffer for the ndarray,
!> it can be indirectly modified by changing the ndarray. To avoid bugs
!> related to certain compiler optimizations, declare the Fortran array
!> with the 'asynchronous' attribute.
interface ndarray_create_nocopy
  module procedure ndarray_create_nocopy_int32_1d
  module procedure ndarray_create_nocopy_int64_1d
  module procedure ndarray_create_nocopy_real32_1d
  module procedure ndarray_create_nocopy_real64_1d
  module procedure ndarray_create_nocopy_complex_real32_1d
  module procedure ndarray_create_nocopy_complex_real64_1d
  module procedure ndarray_create_nocopy_int32_2d
  module procedure ndarray_create_nocopy_int64_2d
  module procedure ndarray_create_nocopy_real32_2d
  module procedure ndarray_create_nocopy_real64_2d
  module procedure ndarray_create_nocopy_complex_real32_2d
  module procedure ndarray_create_nocopy_complex_real64_2d
  module procedure ndarray_create_nocopy_int32_3d
  module procedure ndarray_create_nocopy_int64_3d
  module procedure ndarray_create_nocopy_real32_3d
  module procedure ndarray_create_nocopy_real64_3d
  module procedure ndarray_create_nocopy_complex_real32_3d
  module procedure ndarray_create_nocopy_complex_real64_3d
  module procedure ndarray_create_nocopy_int32_4d
  module procedure ndarray_create_nocopy_int64_4d
  module procedure ndarray_create_nocopy_real32_4d
  module procedure ndarray_create_nocopy_real64_4d
  module procedure ndarray_create_nocopy_complex_real32_4d
  module procedure ndarray_create_nocopy_complex_real64_4d
end interface

!> Creates an empty ndarray of given shape. Array contains uninitialised values.
interface ndarray_create_empty
  module procedure ndarray_create_empty_aint32
  module procedure ndarray_create_empty_aint64
  module procedure ndarray_create_empty_int32
  module procedure ndarray_create_empty_int64
end interface

!> Creates a ndarray of zeroes.
interface ndarray_create_zeros
  module procedure ndarray_create_zeros_aint32
  module procedure ndarray_create_zeros_aint64
  module procedure ndarray_create_zeros_int32
  module procedure ndarray_create_zeros_int64
end interface

!> Creates a ndarray of ones.
interface ndarray_create_ones
  module procedure ndarray_create_ones_aint32
  module procedure ndarray_create_ones_aint64
  module procedure ndarray_create_ones_int32
  module procedure ndarray_create_ones_int64
end interface

!> Casts/Transforms between Fortran and Python datatypes
!>
!> Result is 1st argument to cast, object/scalar to cast 2nd argument
!> Use to cast/transform a Python [[object]] into a Fortran value
!> Use to cast/transform Fortran values into a Python [[object]]
!> and to cast an unspecific Python [[object]] into more specific objects, such
!> as [[list]], [[tuple]], [[dict]]
!> Fortran values can be scalars and character strings or character arrays
!> Fortran character strings are decoded as UTF-8
!> Python strings are encoded as UTF-8
!>
!> For casting to numerical Fortran scalars, there is an optional 3rd argument "strict"
!> for cast: if strict=.false. it will try to convert numerical values to the requested
!> datatype (default: strict=.true.). This is the same as using [[cast_nonstrict]].
interface cast
  module procedure cast_to_list
  module procedure cast_to_dict
  module procedure cast_to_tuple
  module procedure cast_to_NoneType
  module procedure cast_to_ndarray
  module procedure cast_to_object
  
  module procedure cast_to_char_1d
  module procedure cast_to_chars
  
  module procedure cast_to_int32
  module procedure cast_to_int32_flex
  module procedure cast_from_int32  
  module procedure cast_to_int64
  module procedure cast_to_int64_flex
  module procedure cast_from_int64  
  module procedure cast_to_real32
  module procedure cast_to_real32_flex
  module procedure cast_from_real32  
  module procedure cast_to_real64
  module procedure cast_to_real64_flex
  module procedure cast_from_real64  
  module procedure cast_to_complex_real32
  module procedure cast_to_complex_real32_flex
  module procedure cast_from_complex_real32  
  module procedure cast_to_complex_real64
  module procedure cast_to_complex_real64_flex
  module procedure cast_from_complex_real64  
  module procedure cast_to_logical
  module procedure cast_to_logical_flex
  module procedure cast_from_logical  
end interface

!> Non-strict casts/transforms between Fortran and Python datatypes
!>
!> Result is 1st argument to cast, 2nd argument object/scalar to cast
!>
!> In contrast to [[cast]], cast_nonstrict tries to convert to the type specified
!> by the 1st argument even when there is no exact correspondence of types.
!> Non-strict cast might lead to loss of information (e. g. when casting
!> a float to an integer) or might need additional memory and time for 
!> making a copy (e. g. casting a list to a tuple) 
!>
!> Use to cast/transform a Python [[object]] into a Fortran value
!> and to cast an unspecific Python [[object]] into more specific objects, such
!> as [[list]] or [[tuple]], converting between types when necessary.
!> Fortran values can be scalars or character strings.
!> 
!> Can be used to get the string representation of a Python object
!> as a Fortran character string.
!> Python strings are encoded as UTF-8
interface cast_nonstrict
  module procedure cast_nonstrict_to_list
  module procedure cast_nonstrict_to_tuple
  
  ! no cast_nonstrict_to_char_1d, because one can 
  ! not always return a pointer to a character buffer
  
  module procedure cast_nonstrict_to_chars
  
  module procedure cast_nonstrict_to_int32 
  module procedure cast_nonstrict_to_int64 
  module procedure cast_nonstrict_to_real32 
  module procedure cast_nonstrict_to_real64 
  module procedure cast_nonstrict_to_complex_real32 
  module procedure cast_nonstrict_to_complex_real64 
  module procedure cast_nonstrict_to_logical 
end interface

! Class objects that correspond to Python standard exceptions
type(type_py), public, save :: ArithmeticError
type(type_py), public, save :: AssertionError
type(type_py), public, save :: AttributeError
type(type_py), public, save :: BaseException
type(type_py), public, save :: BufferError
type(type_py), public, save :: BytesWarning
type(type_py), public, save :: DeprecationWarning
type(type_py), public, save :: EOFError
type(type_py), public, save :: EnvironmentError
type(type_py), public, save :: Exception
type(type_py), public, save :: FloatingPointError
type(type_py), public, save :: FutureWarning
type(type_py), public, save :: GeneratorExit
type(type_py), public, save :: IOError
type(type_py), public, save :: ImportError
type(type_py), public, save :: ImportWarning
type(type_py), public, save :: IndentationError
type(type_py), public, save :: IndexError
type(type_py), public, save :: KeyError
type(type_py), public, save :: KeyboardInterrupt
type(type_py), public, save :: LookupError
type(type_py), public, save :: MemoryError
type(type_py), public, save :: NameError
type(type_py), public, save :: NotImplementedError
type(type_py), public, save :: OSError
type(type_py), public, save :: OverflowError
type(type_py), public, save :: PendingDeprecationWarning
type(type_py), public, save :: ReferenceError
type(type_py), public, save :: RuntimeError
type(type_py), public, save :: RuntimeWarning
type(type_py), public, save :: StandardError
type(type_py), public, save :: StopIteration
type(type_py), public, save :: SyntaxError
type(type_py), public, save :: SyntaxWarning
type(type_py), public, save :: SystemError
type(type_py), public, save :: SystemExit
type(type_py), public, save :: TabError
type(type_py), public, save :: TypeError
type(type_py), public, save :: UnboundLocalError
type(type_py), public, save :: UnicodeDecodeError
type(type_py), public, save :: UnicodeEncodeError
type(type_py), public, save :: UnicodeError
type(type_py), public, save :: UnicodeTranslateError
type(type_py), public, save :: UnicodeWarning
type(type_py), public, save :: UserWarning
type(type_py), public, save :: ValueError
type(type_py), public, save :: Warning
type(type_py), public, save :: ZeroDivisionError

! Types needed for writing a Python extension
! For extensions one needs exactly one of PythonMethodTable and PythonModule at Fortran module level

!> just a helper type, not public
type PythonMethodTableStrings
  character(kind=C_CHAR, len=:), pointer :: doc_string
  character(kind=C_CHAR, len=:), pointer :: method_name
end type

!> Only used for writing Python extension modules. Datastructure to hold table of methods of your Python extension module.
!> Put exactly one instance at Fortran module level.
!>
!> Python 3: initialise and configure in PyInit_*module name* function with bind(c, name="PyInit_*module_name*") 
!>           attribute and type(c_ptr) return value.
!> Python 2: initialise in init*module name* subroutine with bind(c) attribute
!>
!> Pass the configured PythonMethodTable to PythonModule%init
type PythonMethodTable
private
  type(PyMethodDef), dimension(:), pointer :: methods
  type(PythonMethodTableStrings), dimension(:), allocatable :: strings
  integer :: num_methods
  integer :: method_count
contains
  !> Initialises the method table. Call in PyInit_*module name* (Py3) / init*module name* (Py2)
  procedure, public :: init => PythonMethodTable_init
  !> Adds a method to your Python module
  procedure, public :: add_method => PythonMethodTable_add_method
  !> Used only internally. Gets type(c_ptr) to method table.
  procedure, public :: get_method_table => PythonMethodTable_get
end type

!> Only used for writing Python extension modules. Datastructure to hold information about 
!> your Python extension module. Put exactly one instance at Fortran module level.
!>
!> Python 3: initialise and configure in PyInit_<module name> function with bind(c, name="PyInit_<module_name>") 
!>           attribute and type(c_ptr) return value.
!>
!> Python 2: initialise in init<module name> subroutine with bind(c) attribute
type PythonModule
private
  type(PyModuleDef), pointer :: module_def
  character(kind=C_CHAR, len=:), pointer :: doc_string
  character(kind=C_CHAR, len=:), pointer :: module_name
  type(c_ptr) :: module_ptr
contains
  !> Initialises the PythonModule with a PythonMethod table.
  !>
  !> Python 3: the return value must be returned by PyInit_*module name*
  !>
  !> Python 2: call in init*module name*, ignore return value
  procedure, public :: init => PythonModule_init
  !> Adds a Python object to the module that can be accessed by 
  !> my_module.the_name_of_object_added
  !> Useful to add constants to a Python module
  procedure, public :: add_object => PythonModule_add_object
end type

CONTAINS

!> Initialisation of forpy module. Must be called before using forpy.
function forpy_initialize(use_numpy) result(ierror)
  !> Set to .false., if you do not need the array features of forpy powered by numpy. (Default: .true.)
  logical, optional, intent(in) :: use_numpy
  integer(kind=C_INT) :: ierror
  
  logical :: numpy_flag
  if (present(use_numpy)) then
    numpy_flag = use_numpy
  else
    numpy_flag = .true.
  endif

  ierror = forpy_initialize_helper(.false., numpy_flag)
end function

!> Deprecated: use forpy_initialize instead
function forpy_initialize_ext(use_numpy) result(ierror)
  !> Set to .false., if you do not need the array features of forpy powered by numpy. (Default: .true.)
  logical, optional, intent(in) :: use_numpy
  integer(kind=C_INT) :: ierror
  
  logical :: numpy_flag
  if (present(use_numpy)) then
    numpy_flag = use_numpy
  else
    numpy_flag = .true.
  endif

  ierror = forpy_initialize_helper(.true., numpy_flag)
end function

function forpy_initialize_helper(is_extension, use_numpy) result(ierror)
  logical, intent(in) :: is_extension
  logical, intent(in) :: use_numpy
  integer(kind=C_INT) :: ierror
  
  ! might remove this in the future, since it is required that
  ! calling Py_Initialize multiple times is safe
  if (.not. is_extension) then
    call Py_Initialize()
  endif
  
  ! Initialise Python's None object
  ierror = forpy_initialize_none()
  if (ierror /= 0) then
    return
  endif

  ierror = forpy_initialize_float()
  if (ierror /= 0) then
    return
  endif

  ierror = forpy_initialize_complex()
  if (ierror /= 0) then
    return
  endif

  ierror = forpy_initialize_bool()
  if (ierror /= 0) then
    return
  endif
  
  ierror = forpy_initialize_unicode()
  if (ierror /= 0) then
    return
  endif

  ierror = forpy_initialize_exceptions()
  if (ierror /= 0) then
    return
  endif

  if (use_numpy) then
    ierror = forpy_initialize_numpy()
  endif
end function

function forpy_initialize_numpy() result(ierror)
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: asarray_str
  type(c_ptr) :: ndarray_str

  ! Initialisation of Numpy
  global_numpy_mod = PyImport_ImportModule(C_CHAR_"numpy" // C_NULL_CHAR)
  if (.not. c_associated(global_numpy_mod)) then
    ierror = NO_NUMPY_ERROR
    call err_clear
    return
  else
  
    ! TODO: exception checks?
    ierror = box_value(asarray_str, "asarray")
    
    if (.not. c_associated(asarray_str)) then
      ierror = NO_NUMPY_ERROR
      call err_clear
      return
    else
      global_numpy_asarray_method = PyObject_GetAttr(global_numpy_mod, asarray_str)
      call Py_Decref(asarray_str)

      if (.not. c_associated(global_numpy_asarray_method)) then
        ierror = NO_NUMPY_ERROR ! Something's wrong with numpy... but might use Python without numpy
        call err_clear
      endif
    endif
  endif

  ! Get the numpy.ndarray type-object
  if (ierror == 0) then
    ierror = box_value(ndarray_str, "ndarray")

    if (ierror /= 0) then
      return
    endif

    global_numpy_ndarray_typeobj = PyObject_GetAttr(global_numpy_mod, ndarray_str)
    call Py_Decref(ndarray_str)

    if (.not. c_associated(global_numpy_asarray_method)) then
      ierror = NO_NUMPY_ERROR ! No Numpy...
      call err_clear
    endif

  endif

end function

! sets up Python's None
function forpy_initialize_none() result(ierror)
  integer(kind=C_INT) :: ierror

  type(list) :: li
  type(tuple) :: args
  type(dict) :: kwargs
  type(object) :: none_obj

  ! A bit hacky: use return value of the following python expression
  ! to get pointer to the singleton None-object (_Py_NoneStruct):
  ! The expression is: [].sort()

  ierror = list_create(li)
  if (ierror /= 0) then
    return
  endif

  ierror = tuple_create(args, 0)
  if (ierror /= 0) then
    call li%destroy()
    return
  endif

  ierror = dict_create(kwargs)
  if (ierror /= 0) then
    call li%destroy()
    call args%destroy()
    return
  endif

  ierror = call_py_attribute(none_obj, li, "sort", args, kwargs)

  if (ierror == 0 .and. c_associated(none_obj%py_object)) then
    global_Py_NoneStruct_ptr = none_obj%py_object
    call none_obj%destroy()
  endif

  call li%destroy()
  call args%destroy()
  call kwargs%destroy()

  ierror = 0_C_INT
end function

! Get pointer to "float"-type object
function forpy_initialize_float() result(ierror)
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: a_float
  type(PyObject), pointer :: ptr

  a_float = PyFloat_FromDouble(1.0_C_DOUBLE)

  if (.not. c_associated(a_float)) then
    ierror = EXCEPTION_ERROR
    return
  endif

  call c_f_pointer(a_float, ptr)
  global_pyfloat_type_ptr = ptr%ob_type
  call Py_DecRef(a_float)
  ierror = 0_C_INT
end function

! Get pointer to "complex"-type object
! Note: floats are not complex in Python
function forpy_initialize_complex() result(ierror)
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: a_complex
  type(PyObject), pointer :: ptr

  a_complex = PyComplex_FromDoubles(1.0_C_DOUBLE, 1.0_C_DOUBLE)

  if (.not. c_associated(a_complex)) then
    ierror = EXCEPTION_ERROR
    return
  endif

  call c_f_pointer(a_complex, ptr)
  global_pycomplex_type_ptr = ptr%ob_type
  call Py_DecRef(a_complex)
  ierror = 0_C_INT
end function

! Get pointer to "unicode"-type object
function forpy_initialize_unicode() result(ierror)
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: a_unicode
  type(PyObject), pointer :: ptr
  
  character(kind=C_CHAR), dimension(1) :: a
  character(kind=C_CHAR), dimension(7) :: b 
  
  a = [C_NULL_CHAR]
  b = ['s','t','r','i','c','t', C_NULL_CHAR]

  ! fix for PGI compiler: pgi does not like if a and b are 
  ! string literals in this function call
  a_unicode = PyUnicode_DecodeUTF8(a, 0_PY_SSIZE_T_KIND, b)

  if (.not. c_associated(a_unicode)) then
    ierror = EXCEPTION_ERROR
    return
  endif

  call c_f_pointer(a_unicode, ptr)
  global_pyunicode_type_ptr = ptr%ob_type
  call Py_DecRef(a_unicode)
  ierror = 0_C_INT
end function

! Get pointer to "bool"-type object and the singletons Py_True and Py_False
function forpy_initialize_bool() result(ierror)
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: a_bool
  type(PyObject), pointer :: ptr

  a_bool = PyBool_FromLong(0_C_LONG)

  if (.not. c_associated(a_bool)) then
    ierror = EXCEPTION_ERROR
    return
  endif

  call c_f_pointer(a_bool, ptr)
  global_pybool_type_ptr = ptr%ob_type
  global_Py_FalseStruct_ptr = a_bool
  call Py_DecRef(a_bool)

  a_bool = PyBool_FromLong(1_C_LONG)

  if (.not. c_associated(a_bool)) then
    ierror = EXCEPTION_ERROR
    return
  endif

  global_Py_TrueStruct_ptr = a_bool
  call Py_DecRef(a_bool)

  ierror = 0_C_INT
end function

! initialise Python standard exceptions
function forpy_initialize_exceptions() result(ierror)
  integer(kind=C_INT) :: ierror
  
  type(c_ptr) :: tmp
  type(c_ptr) :: builtin_dict
  
  ierror = 0_C_INT

  builtin_dict = PyEval_GetBuiltins()
  if (.not. c_associated(builtin_dict)) then
    ierror = 1_C_INT
    return
  endif
  
  tmp = PyDict_GetItemString(builtin_dict, "ArithmeticError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  ArithmeticError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "AssertionError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  AssertionError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "AttributeError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  AttributeError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "BaseException" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  BaseException%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "BufferError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  BufferError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "BytesWarning" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  BytesWarning%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "DeprecationWarning" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  DeprecationWarning%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "EOFError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  EOFError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "EnvironmentError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  EnvironmentError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "Exception" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  Exception%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "FloatingPointError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  FloatingPointError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "FutureWarning" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  FutureWarning%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "GeneratorExit" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  GeneratorExit%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "IOError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  IOError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "ImportError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  ImportError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "ImportWarning" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  ImportWarning%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "IndentationError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  IndentationError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "IndexError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  IndexError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "KeyError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  KeyError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "KeyboardInterrupt" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  KeyboardInterrupt%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "LookupError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  LookupError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "MemoryError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  MemoryError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "NameError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  NameError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "NotImplementedError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  NotImplementedError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "OSError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  OSError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "OverflowError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  OverflowError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "PendingDeprecationWarning" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  PendingDeprecationWarning%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "ReferenceError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  ReferenceError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "RuntimeError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  RuntimeError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "RuntimeWarning" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  RuntimeWarning%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "StandardError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  StandardError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "StopIteration" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  StopIteration%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "SyntaxError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  SyntaxError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "SyntaxWarning" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  SyntaxWarning%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "SystemError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  SystemError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "SystemExit" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  SystemExit%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "TabError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  TabError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "TypeError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  TypeError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "UnboundLocalError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  UnboundLocalError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "UnicodeDecodeError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  UnicodeDecodeError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "UnicodeEncodeError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  UnicodeEncodeError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "UnicodeError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  UnicodeError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "UnicodeTranslateError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  UnicodeTranslateError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "UnicodeWarning" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  UnicodeWarning%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "UserWarning" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  UserWarning%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "ValueError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  ValueError%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "Warning" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  Warning%py_object = tmp
  tmp = PyDict_GetItemString(builtin_dict, "ZeroDivisionError" // C_NULL_CHAR) 
  if (.not. c_associated(tmp)) then; call PyErr_Clear(); endif
  ZeroDivisionError%py_object = tmp
end function

!> Frees resources used by Python interpreter. Call when finished using forpy.
subroutine forpy_finalize()

  call Py_Decref(global_numpy_asarray_method)
  call Py_Decref(global_numpy_mod)
  global_numpy_asarray_method = C_NULL_PTR
  global_numpy_mod = C_NULL_PTR

  call Py_Finalize()
end subroutine


!-------------------------------------------------------------------------------------
! Functions to check type
!> Checks if object is a Python long.
logical function is_long(obj)
  !> The object to check.
  class(object), intent(in) :: obj

  is_long = (check_tp_flags(obj%py_object, ishft(1_C_LONG, 24)) /= 0)
end function

!> Checks if object is a Python list.
logical function is_list(obj)
  !> The object to check.
  class(object), intent(in) :: obj

  is_list = (check_tp_flags(obj%py_object, ishft(1_C_LONG, 25)) /= 0)
end function

!> Checks if object is a Python tuple.
logical function is_tuple(obj)
  !> The object to check.
  class(object), intent(in) :: obj

  is_tuple = (check_tp_flags(obj%py_object, ishft(1_C_LONG, 26)) /= 0)
end function

!> Checks if object is a Python bytes.
logical function is_bytes(obj)
  !> The object to check.
  class(object), intent(in) :: obj

  is_bytes = (check_tp_flags(obj%py_object, ishft(1_C_LONG, 27)) /= 0)
end function

!> Checks if object is a Python dict.
logical function is_dict(obj)
  !> The object to check.
  class(object), intent(in) :: obj

  is_dict = (check_tp_flags(obj%py_object, ishft(1_C_LONG, 29)) /= 0)
end function


!> Checks if object is a Python float.
logical function is_float(obj)
  !> The object to check.
  class(object), intent(in) :: obj
  type(PyObject), pointer :: obj_ptr

  if (.not. (c_associated(obj%py_object) .and. c_associated(global_pyfloat_type_ptr))) then
    is_float = .false.
    return
  endif

  call c_f_pointer(obj%py_object, obj_ptr)
  is_float = c_associated(global_pyfloat_type_ptr, obj_ptr%ob_type)
  if (.not. is_float) then
    is_float = (PyType_IsSubtype(obj_ptr%ob_type, global_pyfloat_type_ptr) /= 0) ! Check if subtype
  endif
end function

!> Checks if object is a Python complex.
logical function is_complex(obj)
  !> The object to check.
  class(object), intent(in) :: obj
  type(PyObject), pointer :: obj_ptr

  if (.not. (c_associated(obj%py_object) .and. c_associated(global_pycomplex_type_ptr))) then
    is_complex = .false.
    return
  endif

  call c_f_pointer(obj%py_object, obj_ptr)
  is_complex = c_associated(global_pycomplex_type_ptr, obj_ptr%ob_type)
  if (.not. is_complex) then
    is_complex = (PyType_IsSubtype(obj_ptr%ob_type, global_pycomplex_type_ptr) /= 0) ! Check if subtype
  endif
end function

!> Checks if object is a Python bool.
logical function is_bool(obj)
  !> The object to check.
  class(object), intent(in) :: obj
  type(PyObject), pointer :: obj_ptr

  if (.not. (c_associated(obj%py_object) .and. c_associated(global_pybool_type_ptr))) then
    is_bool = .false.
    return
  endif

  call c_f_pointer(obj%py_object, obj_ptr)
  is_bool = c_associated(global_pybool_type_ptr, obj_ptr%ob_type)
  if (.not. is_bool) then
    is_bool = (PyType_IsSubtype(obj_ptr%ob_type, global_pybool_type_ptr) /= 0) ! Check if subtype
  endif
end function

!> Checks if object is a Python unicode.
logical function is_unicode(obj)
  !> The object to check.
  class(object), intent(in) :: obj
  type(PyObject), pointer :: obj_ptr

  if (.not. (c_associated(obj%py_object) .and. c_associated(global_pyunicode_type_ptr))) then
    is_unicode = .false.
    return
  endif

  call c_f_pointer(obj%py_object, obj_ptr)
  is_unicode = c_associated(global_pyunicode_type_ptr, obj_ptr%ob_type)
  if (.not. is_unicode) then
    is_unicode = (PyType_IsSubtype(obj_ptr%ob_type, global_pyunicode_type_ptr) /= 0) ! Check if subtype
  endif
end function


#ifdef PYTHON2
logical function is_short_int(obj)
  class(object), intent(in) :: obj

  is_short_int = (check_tp_flags(obj%py_object, ishft(1_C_LONG, 23)) /= 0)
end function

!> Checks if object is of integer Python type ('int' OR 'long')
logical function is_int(obj)
  !> The object to check.
  class(object), intent(in) :: obj

  is_int = is_short_int(obj)
  if (.not. is_int) then
    is_int = is_long(obj)
  endif
end function
#endif

#ifndef PYTHON2
!> Checks if object is a Python int
logical function is_int(obj)
  !> The object to check.
  class(object), intent(in) :: obj

  is_int = is_long(obj)
end function
#endif

!> Checks if object is a Python str
logical function is_str(obj)
  !> The object to check.
  class(object), intent(in) :: obj
#ifdef PYTHON2
  is_str = is_bytes(obj)
#else
  is_str = is_unicode(obj)
#endif
end function

!> Checks if object is a Python None.
logical function is_none(obj)
  !> The object to check.
  class(object), intent(in) :: obj

  is_none = c_associated(obj%py_object) .and. c_associated(global_Py_NoneStruct_ptr, obj%py_object)
end function

!> Returns true if object is a null-pointer internally. Object is not properly initialised then.
logical function is_null(obj)
  !> The object to check.
  class(object), intent(in) :: obj

  is_null = (.not. c_associated(obj%py_object))
end function

!> Checks if object is a Numpy array.
logical function is_ndarray(obj)
  !> The object to check.
  class(object), intent(in) :: obj

  integer(kind=C_INT) :: res
  type(c_ptr) :: err_obj

  if (.not. c_associated(global_numpy_ndarray_typeobj)) then
    is_ndarray = .false.
    return
  endif

  res = PyObject_IsInstance(obj%py_object, global_numpy_ndarray_typeobj)

  ! if res==-1 then exception is set by PyObject_IsInstance
  ! clear the exception and return false
  if (res == -1_C_INT) then
    is_ndarray = .false.
      err_obj = PyErr_Occurred()
      if (c_associated(err_obj)) then
        call PyErr_Clear()
      endif
    return
  endif

  is_ndarray = (res == 1)
end function

!> Helper for type checks
function check_tp_flags(py_obj, mask) result(res)
  type(c_ptr), intent(in) :: py_obj
  integer(kind=C_LONG), intent(in) :: mask
  integer(kind=C_LONG) :: res

  type(PyObject), pointer :: obj_ptr
  type(PyTypeObject), pointer :: type_ptr

  call c_f_pointer(py_obj, obj_ptr)
  call c_f_pointer(obj_ptr%ob_type, type_ptr)

  res = iand(type_ptr%tp_flags, mask)
end function

!> Creates an empty list. Python equivalent: r = []
function list_create_empty(r) result(ierror)
  !> the freshly created empty list
  type(list), intent(out) :: r
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror

  r%py_object = PyList_New(0_PY_SSIZE_T_KIND)
  ierror = 0_C_INT

  if (.not. c_associated(r%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

!> Creates a list from given object if possible. Python equivalent: r = list(obj)
function list_create_object(r, obj) result(ierror)
  !> the freshly created list
  type(list), intent(out) :: r
  !> object to create list from
  class(object), intent(in) :: obj
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  ierror = 0_C_INT
  r%py_object = PySequence_List(obj%py_object)
  
  if (.not. c_associated(r%py_object)) then 
    ierror = EXCEPTION_ERROR
  endif
end function

!> Creates a tuple with a given number of elements
function tuple_create_int32(r, len) result(ierror)
  !> the freshly created tuple
  type(tuple), intent(out) :: r
  !> Number of items in tuple
  integer(kind=int32), intent(in) :: len
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror

  integer(kind=PY_SSIZE_T_KIND) :: tmp

  tmp = int(len, PY_SSIZE_T_KIND)
  r%py_object = PyTuple_New(tmp)
  ierror = 0_C_INT

  if (.not. c_associated(r%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

!> Creates a tuple with a given number of elements
function tuple_create_int64(r, len) result(ierror)
  !> the freshly created tuple
  type(tuple), intent(out) :: r
  !> Number of items in tuple
  integer(kind=int64), intent(in) :: len
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror

  integer(kind=PY_SSIZE_T_KIND) :: tmp

  tmp = int(len, PY_SSIZE_T_KIND)
  r%py_object = PyTuple_New(tmp)
  ierror = 0_C_INT

  if (.not. c_associated(r%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function


function tuple_create_object(r, obj) result(ierror)
  !> the created tuple
  type(tuple), intent(out) :: r
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  ierror = 0_C_INT
  r%py_object = PySequence_Tuple(obj%py_object)
  
  if (.not. c_associated(r%py_object)) then 
    ierror = EXCEPTION_ERROR
  endif

end function

!> Creates a Python None.
function NoneType_create(r) result(ierror)
  !> The created None
  type(NoneType), intent(out) :: r
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror

  r%py_object = global_Py_NoneStruct_ptr
  call Py_IncRef(global_Py_NoneStruct_ptr)
  ierror = 0_C_INT  

end function

!> Creates an empty Python dictionary. Python: r = {}
function dict_create(r) result(ierror)
  !> The created empty dict
  type(dict), intent(out) :: r
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  r%py_object = PyDict_New()
  ierror = 0_C_INT 

  if (.not. c_associated(r%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

subroutine dict_clear(self)
  class(dict), intent(inout) :: self
  call PyDict_Clear(self%py_object)
end subroutine

function dict_copy(self, dest) result(ierror)
  class(dict), intent(in) :: self
  type(dict), intent(out) :: dest
  integer(kind=C_INT) :: ierror
  
  ierror = 0_C_INT
  dest%py_object = PyDict_Copy(self%py_object)
  if (.not. c_associated(dest%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function dict_keys(self, keys) result(ierror)
  class(dict), intent(in) :: self
  type(list), intent(out) :: keys
  integer(kind=C_INT) :: ierror
  
  ierror = 0_C_INT
  keys%py_object = PyDict_Keys(self%py_object)
  if (.not. c_associated(keys%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function dict_items(self, items) result(ierror)
  class(dict), intent(in) :: self
  type(list), intent(out) :: items
  integer(kind=C_INT) :: ierror
  
  ierror = 0_C_INT
  items%py_object = PyDict_Items(self%py_object)
  if (.not. c_associated(items%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function dict_values(self, values) result(ierror)
  class(dict), intent(in) :: self
  type(list), intent(out) :: values
  integer(kind=C_INT) :: ierror
  
  ierror = 0_C_INT
  values%py_object = PyDict_Values(self%py_object)
  if (.not. c_associated(values%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function list_append_object(self, item) result(ierror)
  class(list), intent(inout) :: self
  class(object), intent(in) :: item
  integer(kind=C_INT) :: ierror

  ierror = PyList_Append(self%py_object, item%py_object)
end function

function list_append_int32(self, item) result(ierror)
  class(list), intent(inout) :: self
  integer(kind=int32), intent(in) :: item
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)

  if (ierror == 0_C_INT) then
    ierror = PyList_Append(self%py_object, item_py)
    call Py_DecRef(item_py)
  endif
end function

function list_append_int64(self, item) result(ierror)
  class(list), intent(inout) :: self
  integer(kind=int64), intent(in) :: item
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)

  if (ierror == 0_C_INT) then
    ierror = PyList_Append(self%py_object, item_py)
    call Py_DecRef(item_py)
  endif
end function

function list_append_real32(self, item) result(ierror)
  class(list), intent(inout) :: self
  real(kind=real32), intent(in) :: item
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)

  if (ierror == 0_C_INT) then
    ierror = PyList_Append(self%py_object, item_py)
    call Py_DecRef(item_py)
  endif
end function

function list_append_real64(self, item) result(ierror)
  class(list), intent(inout) :: self
  real(kind=real64), intent(in) :: item
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)

  if (ierror == 0_C_INT) then
    ierror = PyList_Append(self%py_object, item_py)
    call Py_DecRef(item_py)
  endif
end function

function list_append_complex_real32(self, item) result(ierror)
  class(list), intent(inout) :: self
  complex(kind=real32), intent(in) :: item
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)

  if (ierror == 0_C_INT) then
    ierror = PyList_Append(self%py_object, item_py)
    call Py_DecRef(item_py)
  endif
end function

function list_append_complex_real64(self, item) result(ierror)
  class(list), intent(inout) :: self
  complex(kind=real64), intent(in) :: item
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)

  if (ierror == 0_C_INT) then
    ierror = PyList_Append(self%py_object, item_py)
    call Py_DecRef(item_py)
  endif
end function

function list_append_logical(self, item) result(ierror)
  class(list), intent(inout) :: self
  logical, intent(in) :: item
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)

  if (ierror == 0_C_INT) then
    ierror = PyList_Append(self%py_object, item_py)
    call Py_DecRef(item_py)
  endif
end function

function list_append_char_1d(self, item) result(ierror)
  class(list), intent(inout) :: self
  character(kind=C_CHAR), dimension(:), intent(in) :: item
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)

  if (ierror == 0_C_INT) then
    ierror = PyList_Append(self%py_object, item_py)
    call Py_DecRef(item_py)
  endif
end function

function list_append_chars(self, item) result(ierror)
  class(list), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: item
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)

  if (ierror == 0_C_INT) then
    ierror = PyList_Append(self%py_object, item_py)
    call Py_DecRef(item_py)
  endif
end function


function list_copy(self, dest) result(ierror)
  class(list), intent(in) :: self
  type(list), intent(out) :: dest
  integer(kind=C_INT) :: ierror
  
  ierror = list_create(dest, self)
end function

function list_sort(self) result(ierror)
  class(list), intent(inout) :: self
  integer(kind=C_INT) :: ierror
  
  ierror = PyList_Sort(self%py_object)
end function

function list_reverse(self) result(ierror)
  class(list), intent(inout) :: self
  integer(kind=C_INT) :: ierror
  
  ierror = PyList_Reverse(self%py_object)
end function

!Creates new list by concatenating 'list_to_concatenate'
function list_add(self, result_list, list_to_concatenate) result(ierror)
  class(list), intent(inout) :: self
  type(list), intent(out) :: result_list
  class(list), intent(in) :: list_to_concatenate
  integer(kind=C_INT) :: ierror
  
  ierror = 0_C_INT
  result_list%py_object = PySequence_Concat(self%py_object, list_to_concatenate%py_object)
  if (.not. c_associated(result_list%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function list_insert_int32(self, ind, item) result(ierror)
  class(list), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  class(object), intent(in) :: item
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: ind_tmp
  
  ind_tmp = int(ind, PY_SSIZE_T_KIND)
  ierror = PyList_Insert(self%py_object, ind_tmp, item%py_object) 
end function

function list_delitem_int32(self, ind) result(ierror)
  class(list), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: ind_tmp
  
  ind_tmp = int(ind, PY_SSIZE_T_KIND)
  ierror = PySequence_DelItem(self%py_object, ind_tmp) 
end function

function list_insert_int64(self, ind, item) result(ierror)
  class(list), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  class(object), intent(in) :: item
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: ind_tmp
  
  ind_tmp = int(ind, PY_SSIZE_T_KIND)
  ierror = PyList_Insert(self%py_object, ind_tmp, item%py_object) 
end function

function list_delitem_int64(self, ind) result(ierror)
  class(list), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: ind_tmp
  
  ind_tmp = int(ind, PY_SSIZE_T_KIND)
  ierror = PySequence_DelItem(self%py_object, ind_tmp) 
end function


function sequence_getitem_int32_object(self, item, ind) result(ierror)
  class(Sequence), intent(inout) :: self
  type(object), intent(out) :: item
  integer(kind=int32), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item%py_object = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function sequence_getitem_int32_int32(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  integer(kind=int32), intent(out) :: item
  integer(kind=int32), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int32_int64(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  integer(kind=int64), intent(out) :: item
  integer(kind=int32), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int32_real32(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  real(kind=real32), intent(out) :: item
  integer(kind=int32), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int32_real64(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  real(kind=real64), intent(out) :: item
  integer(kind=int32), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int32_complex_real32(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  complex(kind=real32), intent(out) :: item
  integer(kind=int32), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int32_complex_real64(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  complex(kind=real64), intent(out) :: item
  integer(kind=int32), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int32_logical(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  logical, intent(out) :: item
  integer(kind=int32), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int32_char_1d(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  character(kind=C_CHAR), dimension(:), pointer, intent(out) :: item
  integer(kind=int32), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int32_chars(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: item
  integer(kind=int32), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int64_object(self, item, ind) result(ierror)
  class(Sequence), intent(inout) :: self
  type(object), intent(out) :: item
  integer(kind=int64), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item%py_object = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function sequence_getitem_int64_int32(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  integer(kind=int32), intent(out) :: item
  integer(kind=int64), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int64_int64(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  integer(kind=int64), intent(out) :: item
  integer(kind=int64), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int64_real32(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  real(kind=real32), intent(out) :: item
  integer(kind=int64), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int64_real64(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  real(kind=real64), intent(out) :: item
  integer(kind=int64), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int64_complex_real32(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  complex(kind=real32), intent(out) :: item
  integer(kind=int64), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int64_complex_real64(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  complex(kind=real64), intent(out) :: item
  integer(kind=int64), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int64_logical(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  logical, intent(out) :: item
  integer(kind=int64), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int64_char_1d(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  character(kind=C_CHAR), dimension(:), pointer, intent(out) :: item
  integer(kind=int64), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function

function sequence_getitem_int64_chars(self, item, ind) result(ierror)
  class(Sequence), intent(in) :: self
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: item
  integer(kind=int64), intent(in) :: ind

  integer(kind=C_INT) :: ierror
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  type(c_ptr) :: item_py

  ierror = 0_C_INT
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)
  item_py = PySequence_GetItem(self%py_object, ind_py_ssize_t)

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif
end function


function Sequence_len_int32(self, length) result(ierror)
  class(Sequence), intent(in) :: self
  integer(kind=int32), intent(out) :: length
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: length_tmp
  
  ierror = 0_C_INT
  length_tmp = PyObject_Length(self%py_object)
  length = int(length_tmp, int32)
  
  ! TODO: overflow check
  if (length_tmp == -1_PY_SSIZE_T_KIND) then
    ierror = EXCEPTION_ERROR
  endif
  
end function

function Sequence_len_int64(self, length) result(ierror)
  class(Sequence), intent(in) :: self
  integer(kind=int64), intent(out) :: length
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: length_tmp
  
  ierror = 0_C_INT
  length_tmp = PyObject_Length(self%py_object)
  length = int(length_tmp, int64)
  
  ! TODO: overflow check
  if (length_tmp == -1_PY_SSIZE_T_KIND) then
    ierror = EXCEPTION_ERROR
  endif
  
end function

function Mapping_len_int32(self, length) result(ierror)
  class(Mapping), intent(in) :: self
  integer(kind=int32), intent(out) :: length
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: length_tmp
  
  ierror = 0_C_INT
  length_tmp = PyObject_Length(self%py_object)
  length = int(length_tmp, int32)
  
  ! TODO: overflow check
  if (length_tmp == -1_PY_SSIZE_T_KIND) then
    ierror = EXCEPTION_ERROR
  endif
  
end function

function Mapping_len_int64(self, length) result(ierror)
  class(Mapping), intent(in) :: self
  integer(kind=int64), intent(out) :: length
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: length_tmp
  
  ierror = 0_C_INT
  length_tmp = PyObject_Length(self%py_object)
  length = int(length_tmp, int64)
  
  ! TODO: overflow check
  if (length_tmp == -1_PY_SSIZE_T_KIND) then
    ierror = EXCEPTION_ERROR
  endif
  
end function


function sequence_index_int32(self, ind, item) result(ierror)
  class(Sequence), intent(in) :: self
  integer(kind=int32), intent(out) :: ind  
  class(object), intent(in) :: item
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: ind_tmp
  
  ierror = 0_C_INT
  ind_tmp = PySequence_Index(self%py_object, item%py_object)
  ind = int(ind_tmp, int32)
  
  ! TODO: overflow check
  if (ind_tmp == -1_PY_SSIZE_T_KIND) then
    ierror = EXCEPTION_ERROR
  endif
  
end function

function sequence_count_int32(self, the_count, item) result(ierror)
  class(Sequence), intent(in) :: self
  integer(kind=int32), intent(out) :: the_count  
  class(object), intent(in) :: item
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: the_count_tmp
  
  ierror = 0_C_INT
  the_count_tmp = PySequence_Count(self%py_object, item%py_object)
  the_count = int(the_count_tmp, int32)
  
  ! TODO: overflow check
  if (the_count_tmp == -1_PY_SSIZE_T_KIND) then
    ierror = EXCEPTION_ERROR
  endif
  
end function

function sequence_index_int64(self, ind, item) result(ierror)
  class(Sequence), intent(in) :: self
  integer(kind=int64), intent(out) :: ind  
  class(object), intent(in) :: item
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: ind_tmp
  
  ierror = 0_C_INT
  ind_tmp = PySequence_Index(self%py_object, item%py_object)
  ind = int(ind_tmp, int64)
  
  ! TODO: overflow check
  if (ind_tmp == -1_PY_SSIZE_T_KIND) then
    ierror = EXCEPTION_ERROR
  endif
  
end function

function sequence_count_int64(self, the_count, item) result(ierror)
  class(Sequence), intent(in) :: self
  integer(kind=int64), intent(out) :: the_count  
  class(object), intent(in) :: item
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: the_count_tmp
  
  ierror = 0_C_INT
  the_count_tmp = PySequence_Count(self%py_object, item%py_object)
  the_count = int(the_count_tmp, int64)
  
  ! TODO: overflow check
  if (the_count_tmp == -1_PY_SSIZE_T_KIND) then
    ierror = EXCEPTION_ERROR
  endif
  
end function


function sequence_contains(self, contain_flag, item) result(ierror)
  class(sequence), intent(in) :: self
  logical, intent(out) :: contain_flag
  class(object), intent(in) :: item
  integer(kind=C_INT) :: ierror
  
  ! returns -1 on error, 0 if item not contained, 1 if contained
  ierror = PySequence_Contains(self%py_object, item%py_object)
  contain_flag = (ierror == 1_C_INT)
  
  if (contain_flag) then
    ierror = 0_C_INT
  endif
  
end function

function mutablesequence_setitem_int32_object(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  class(object), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item%py_object)

end function

function mutablesequence_setitem_int32_int32(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  integer(kind=int32), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int32_int64(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  integer(kind=int64), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int32_real32(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  real(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int32_real64(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  real(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int32_complex_real32(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  complex(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int32_complex_real64(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  complex(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int32_logical(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  logical, intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int32_char_1d(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  character(kind=C_CHAR), dimension(:), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int32_chars(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  character(kind=C_CHAR, len=*), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int64_object(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  class(object), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item%py_object)

end function

function mutablesequence_setitem_int64_int32(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  integer(kind=int32), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int64_int64(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  integer(kind=int64), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int64_real32(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  real(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int64_real64(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  real(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int64_complex_real32(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  complex(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int64_complex_real64(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  complex(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int64_logical(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  logical, intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int64_char_1d(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  character(kind=C_CHAR), dimension(:), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function

function mutablesequence_setitem_int64_chars(self, ind, item) result(ierror)
  class(MutableSequence), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  character(kind=C_CHAR, len=*), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  type(c_ptr) :: item_py

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  if (ierror == 0) then
    ierror = PySequence_SetItem(self%py_object, ind_py_ssize_t, item_py)
    call Py_DecRef(item_py)
  endif
end function



! See also: http://stackoverflow.com/questions/6111843/limitations-of-pytuple-setitem
! Tuple ref-count must be 1 - otherwise cannot set items
function tuple_setitem_int32_int32(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  integer(kind=int32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int32_int64(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  integer(kind=int64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int32_real32(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  real(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int32_real64(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  real(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int32_complex_real32(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  complex(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int32_complex_real64(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  complex(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int32_logical(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  logical, intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int32_char_1d(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  character(kind=C_CHAR), dimension(:), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int32_chars(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  character(kind=C_CHAR, len=*), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int64_int32(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  integer(kind=int32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int64_int64(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  integer(kind=int64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int64_real32(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  real(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int64_real64(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  real(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int64_complex_real32(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  complex(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int64_complex_real64(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  complex(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int64_logical(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  logical, intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int64_char_1d(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  character(kind=C_CHAR), dimension(:), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function

function tuple_setitem_int64_chars(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  character(kind=C_CHAR, len=*), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py
  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t

  ierror = box_value(item_py, item)
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  if (ierror == 0) then
    ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item_py)
    ! note: no need for Py_DecRef since reference is stolen
  endif
end function


function tuple_setitem_int32_object(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int32), intent(in) :: ind
  class(object), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  ! therefore must increase ref-count of item

  call Py_IncRef(item%py_object)
  ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item%py_object)
  
end function

function tuple_setitem_int64_object(self, ind, item) result(ierror)
  class(tuple), intent(inout) :: self
  integer(kind=int64), intent(in) :: ind
  class(object), intent(in) :: item
  integer(kind=C_INT):: ierror

  integer(kind=PY_SSIZE_T_KIND) :: ind_py_ssize_t
  
  ind_py_ssize_t = int(ind, PY_SSIZE_T_KIND)

  !tuple: must use PyTuple_SetItem
  !PyTuple_SetItem steals a reference - in contrast to PyObject_SetItem
  ! therefore must increase ref-count of item

  call Py_IncRef(item%py_object)
  ierror = PyTuple_SetItem(self%py_object, ind_py_ssize_t, item%py_object)
  
end function


function tuple_add(self, result_tuple, tuple_to_concatenate) result(ierror)
  class(tuple), intent(inout) :: self
  type(tuple), intent(out) :: result_tuple
  class(tuple), intent(inout) :: tuple_to_concatenate
  integer(kind=C_INT) :: ierror
  
  ierror = 0_C_INT
  result_tuple%py_object = PySequence_Concat(self%py_object, tuple_to_concatenate%py_object)
  if (.not. c_associated(result_tuple%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function object_getattribute(self, attr, attr_name) result(ierror)
  class(object), intent(in) :: self
  type(object), intent(out) :: attr
  character(kind=C_CHAR, len=*), intent(in) :: attr_name
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: attr_name_str

  ierror = box_value(attr_name_str, attr_name)

  if (ierror == 0_C_INT) then
    attr%py_object = PyObject_GetAttr(self%py_object, attr_name_str)
  else
    return
  endif

  call Py_Decref(attr_name_str)

  if (.not. c_associated(attr%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function object_setattr(self, attr_name, attr_value) result(ierror)
  class(object), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: attr_name
  class(object), intent(in) :: attr_value
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: attr_name_str

  ierror = box_value(attr_name_str, attr_name)

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetAttr(self%py_object, attr_name_str, attr_value%py_object)
    call Py_Decref(attr_name_str)
  endif
end function

function object_delattr(self, attr_name) result(ierror)
  class(object), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: attr_name
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: attr_name_str
  ierror = box_value(attr_name_str, attr_name)

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetAttr(self%py_object, attr_name_str, C_NULL_PTR)
    call Py_Decref(attr_name_str)
  endif
end function

!> Get C pointer to an object, needed for developing Python extensions.
function object_get_c_ptr(self) result(r)
  class(object), intent(in) :: self
  type(c_ptr) :: r
  r = self%py_object
end function

subroutine object_destroy(self)
  class(object), intent(inout) :: self
  call Py_DecRef(self%py_object)
end subroutine

function mapping_getitem_object_object(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  type(object), intent(out) :: item
  class(object), intent(in) :: key

  integer(kind=C_INT) :: ierror

  item%py_object = PyObject_GetItem(self%py_object, key%py_object)

  if (.not. c_associated(item%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function mapping_getitem_int32_object(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  type(object), intent(out) :: item
  integer(kind=int32), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item%py_object = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function mapping_getitem_int32_int32(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(out) :: item
  integer(kind=int32), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int32_int64(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(out) :: item
  integer(kind=int32), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int32_real32(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  real(kind=real32), intent(out) :: item
  integer(kind=int32), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int32_real64(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  real(kind=real64), intent(out) :: item
  integer(kind=int32), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int32_complex_real32(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  complex(kind=real32), intent(out) :: item
  integer(kind=int32), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int32_complex_real64(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  complex(kind=real64), intent(out) :: item
  integer(kind=int32), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int32_logical(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  logical, intent(out) :: item
  integer(kind=int32), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int32_char_1d(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR), dimension(:), pointer, intent(out) :: item
  integer(kind=int32), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int32_chars(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: item
  integer(kind=int32), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int64_object(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  type(object), intent(out) :: item
  integer(kind=int64), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item%py_object = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function mapping_getitem_int64_int32(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(out) :: item
  integer(kind=int64), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int64_int64(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(out) :: item
  integer(kind=int64), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int64_real32(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  real(kind=real32), intent(out) :: item
  integer(kind=int64), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int64_real64(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  real(kind=real64), intent(out) :: item
  integer(kind=int64), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int64_complex_real32(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  complex(kind=real32), intent(out) :: item
  integer(kind=int64), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int64_complex_real64(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  complex(kind=real64), intent(out) :: item
  integer(kind=int64), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int64_logical(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  logical, intent(out) :: item
  integer(kind=int64), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int64_char_1d(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR), dimension(:), pointer, intent(out) :: item
  integer(kind=int64), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_int64_chars(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: item
  integer(kind=int64), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_chars_object(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  type(object), intent(out) :: item
  character(kind=C_CHAR, len=*), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item%py_object = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

function mapping_getitem_chars_int32(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(out) :: item
  character(kind=C_CHAR, len=*), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_chars_int64(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(out) :: item
  character(kind=C_CHAR, len=*), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_chars_real32(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  real(kind=real32), intent(out) :: item
  character(kind=C_CHAR, len=*), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_chars_real64(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  real(kind=real64), intent(out) :: item
  character(kind=C_CHAR, len=*), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_chars_complex_real32(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  complex(kind=real32), intent(out) :: item
  character(kind=C_CHAR, len=*), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_chars_complex_real64(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  complex(kind=real64), intent(out) :: item
  character(kind=C_CHAR, len=*), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_chars_logical(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  logical, intent(out) :: item
  character(kind=C_CHAR, len=*), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_chars_char_1d(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR), dimension(:), pointer, intent(out) :: item
  character(kind=C_CHAR, len=*), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function

function mapping_getitem_chars_chars(self, item, key) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: item
  character(kind=C_CHAR, len=*), intent(in) :: key

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  item_py = PyObject_GetItem(self%py_object, ind_py)
  call Py_DecRef(ind_py) !don't need index anymore

  if (.not. c_associated(item_py)) then
    ierror = EXCEPTION_ERROR
  else
    ierror = unbox_value(item, item_py)
    call Py_DecRef(item_py)
  endif

end function


function mapping_setitem_object_object(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  class(object), intent(in) :: key
  class(object), intent(in) :: item
  integer(kind=C_INT):: ierror

  ierror = PyObject_SetItem(self%py_object, key%py_object, item%py_object)
  
end function

function mapping_delitem_object(self, key) result(ierror)
  class(Mapping), intent(inout) :: self
  class(object), intent(in) :: key
  integer(kind=C_INT):: ierror

  ierror = PyObject_DelItem(self%py_object, key%py_object)
  
end function

function mapping_setitem_int32_object(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(in) :: key
  class(object), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  ierror = PyObject_SetItem(self%py_object, ind_py, item%py_object)
  call Py_DecRef(ind_py)

end function

function mapping_delitem_int32(self, key) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(in) :: key
  integer(kind=C_INT):: ierror

  type(c_ptr) :: ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = PyObject_DelItem(self%py_object, ind_py)
  call Py_DecRef(ind_py)

end function

function mapping_setitem_int32_int32(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(in) :: key
  integer(kind=int32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int32_int64(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(in) :: key
  integer(kind=int64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int32_real32(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(in) :: key
  real(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int32_real64(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(in) :: key
  real(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int32_complex_real32(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(in) :: key
  complex(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int32_complex_real64(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(in) :: key
  complex(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int32_logical(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(in) :: key
  logical, intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int32_char_1d(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(in) :: key
  character(kind=C_CHAR), dimension(:), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int32_chars(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int32), intent(in) :: key
  character(kind=C_CHAR, len=*), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int64_object(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(in) :: key
  class(object), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  ierror = PyObject_SetItem(self%py_object, ind_py, item%py_object)
  call Py_DecRef(ind_py)

end function

function mapping_delitem_int64(self, key) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(in) :: key
  integer(kind=C_INT):: ierror

  type(c_ptr) :: ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = PyObject_DelItem(self%py_object, ind_py)
  call Py_DecRef(ind_py)

end function

function mapping_setitem_int64_int32(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(in) :: key
  integer(kind=int32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int64_int64(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(in) :: key
  integer(kind=int64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int64_real32(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(in) :: key
  real(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int64_real64(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(in) :: key
  real(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int64_complex_real32(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(in) :: key
  complex(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int64_complex_real64(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(in) :: key
  complex(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int64_logical(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(in) :: key
  logical, intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int64_char_1d(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(in) :: key
  character(kind=C_CHAR), dimension(:), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_int64_chars(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  integer(kind=int64), intent(in) :: key
  character(kind=C_CHAR, len=*), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_chars_object(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: key
  class(object), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0) then
    return
  endif

  ierror = PyObject_SetItem(self%py_object, ind_py, item%py_object)
  call Py_DecRef(ind_py)

end function

function mapping_delitem_chars(self, key) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: key
  integer(kind=C_INT):: ierror

  type(c_ptr) :: ind_py

  ierror = box_value(ind_py, key)

  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = PyObject_DelItem(self%py_object, ind_py)
  call Py_DecRef(ind_py)

end function

function mapping_setitem_chars_int32(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: key
  integer(kind=int32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_chars_int64(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: key
  integer(kind=int64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_chars_real32(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: key
  real(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_chars_real64(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: key
  real(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_chars_complex_real32(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: key
  complex(kind=real32), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_chars_complex_real64(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: key
  complex(kind=real64), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_chars_logical(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: key
  logical, intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_chars_char_1d(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: key
  character(kind=C_CHAR), dimension(:), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function

function mapping_setitem_chars_chars(self, key, item) result(ierror)
  class(Mapping), intent(inout) :: self
  character(kind=C_CHAR, len=*), intent(in) :: key
  character(kind=C_CHAR, len=*), intent(in) :: item
  integer(kind=C_INT):: ierror

  type(c_ptr) :: item_py, ind_py

  ierror = box_value(ind_py, key)

  if (ierror == 0_C_INT) then
    ierror = box_value(item_py, item)

    if (ierror /= 0) then
      call Py_DecRef(ind_py)
      return
    endif
  endif

  if (ierror == 0_C_INT) then
    ierror = PyObject_SetItem(self%py_object, ind_py, item_py)
    call Py_DecRef(item_py)
    call Py_DecRef(ind_py)
  endif
  
end function


function mapping_contains(self, contain_flag, item) result(ierror)
  class(Mapping), intent(in) :: self
  logical, intent(out) :: contain_flag
  class(object), intent(in) :: item
  integer(kind=C_INT) :: ierror
  
  integer(kind=C_INT) :: cont
  ierror = 0_C_INT
  ! returns 0 if item not contained, 1 if contained
  ! in contrast to PySequence_Contains this never fails
  ! we want to have the same call signature as with sequence objects
  ! therefore we have ierror=0 as return value
  cont = PyMapping_HasKey(self%py_object, item%py_object)
  contain_flag = (cont == 1_C_INT)
  
end function

function dict_get_object_object(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  type(object), intent(out) :: item  
  class(object), intent(in) :: key
  class(object), intent(in) :: default_value

  integer(kind=C_INT) :: ierror

  ierror = dict_get_helper(self, item%py_object, key%py_object, default_value%py_object, .false.)
  
end function

function dict_get_int32_object(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  type(object), intent(out) :: item  
  integer(kind=int32), intent(in) :: key
  class(object), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror == 0_C_INT) then
    ierror = dict_get_helper(self, item%py_object, key_ptr, default_value%py_object, .false.)
    call Py_Decref(key_ptr)
  endif
end function

function dict_get_int32_int32(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  integer(kind=int32), intent(out) :: item  
  integer(kind=int32), intent(in) :: key
  integer(kind=int32), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int32_int64(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  integer(kind=int64), intent(out) :: item  
  integer(kind=int32), intent(in) :: key
  integer(kind=int64), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int32_real32(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  real(kind=real32), intent(out) :: item  
  integer(kind=int32), intent(in) :: key
  real(kind=real32), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int32_real64(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  real(kind=real64), intent(out) :: item  
  integer(kind=int32), intent(in) :: key
  real(kind=real64), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int32_complex_real32(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  complex(kind=real32), intent(out) :: item  
  integer(kind=int32), intent(in) :: key
  complex(kind=real32), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int32_complex_real64(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  complex(kind=real64), intent(out) :: item  
  integer(kind=int32), intent(in) :: key
  complex(kind=real64), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int32_logical(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  logical, intent(out) :: item  
  integer(kind=int32), intent(in) :: key
  logical, intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int32_char_1d(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  character(kind=C_CHAR), dimension(:), pointer, intent(out) :: item  
  integer(kind=int32), intent(in) :: key
  character(kind=C_CHAR), dimension(:), pointer, intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int32_chars(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: item  
  integer(kind=int32), intent(in) :: key
  character(kind=C_CHAR, len=:), allocatable, intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int64_object(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  type(object), intent(out) :: item  
  integer(kind=int64), intent(in) :: key
  class(object), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror == 0_C_INT) then
    ierror = dict_get_helper(self, item%py_object, key_ptr, default_value%py_object, .false.)
    call Py_Decref(key_ptr)
  endif
end function

function dict_get_int64_int32(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  integer(kind=int32), intent(out) :: item  
  integer(kind=int64), intent(in) :: key
  integer(kind=int32), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int64_int64(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  integer(kind=int64), intent(out) :: item  
  integer(kind=int64), intent(in) :: key
  integer(kind=int64), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int64_real32(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  real(kind=real32), intent(out) :: item  
  integer(kind=int64), intent(in) :: key
  real(kind=real32), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int64_real64(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  real(kind=real64), intent(out) :: item  
  integer(kind=int64), intent(in) :: key
  real(kind=real64), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int64_complex_real32(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  complex(kind=real32), intent(out) :: item  
  integer(kind=int64), intent(in) :: key
  complex(kind=real32), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int64_complex_real64(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  complex(kind=real64), intent(out) :: item  
  integer(kind=int64), intent(in) :: key
  complex(kind=real64), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int64_logical(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  logical, intent(out) :: item  
  integer(kind=int64), intent(in) :: key
  logical, intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int64_char_1d(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  character(kind=C_CHAR), dimension(:), pointer, intent(out) :: item  
  integer(kind=int64), intent(in) :: key
  character(kind=C_CHAR), dimension(:), pointer, intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_int64_chars(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: item  
  integer(kind=int64), intent(in) :: key
  character(kind=C_CHAR, len=:), allocatable, intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_chars_object(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  type(object), intent(out) :: item  
  character(kind=C_CHAR, len=*), intent(in) :: key
  class(object), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror == 0_C_INT) then
    ierror = dict_get_helper(self, item%py_object, key_ptr, default_value%py_object, .false.)
    call Py_Decref(key_ptr)
  endif
end function

function dict_get_chars_int32(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  integer(kind=int32), intent(out) :: item  
  character(kind=C_CHAR, len=*), intent(in) :: key
  integer(kind=int32), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_chars_int64(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  integer(kind=int64), intent(out) :: item  
  character(kind=C_CHAR, len=*), intent(in) :: key
  integer(kind=int64), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_chars_real32(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  real(kind=real32), intent(out) :: item  
  character(kind=C_CHAR, len=*), intent(in) :: key
  real(kind=real32), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_chars_real64(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  real(kind=real64), intent(out) :: item  
  character(kind=C_CHAR, len=*), intent(in) :: key
  real(kind=real64), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_chars_complex_real32(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  complex(kind=real32), intent(out) :: item  
  character(kind=C_CHAR, len=*), intent(in) :: key
  complex(kind=real32), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_chars_complex_real64(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  complex(kind=real64), intent(out) :: item  
  character(kind=C_CHAR, len=*), intent(in) :: key
  complex(kind=real64), intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_chars_logical(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  logical, intent(out) :: item  
  character(kind=C_CHAR, len=*), intent(in) :: key
  logical, intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_chars_char_1d(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  character(kind=C_CHAR), dimension(:), pointer, intent(out) :: item  
  character(kind=C_CHAR, len=*), intent(in) :: key
  character(kind=C_CHAR), dimension(:), pointer, intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function

function dict_get_chars_chars(self, item, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: item  
  character(kind=C_CHAR, len=*), intent(in) :: key
  character(kind=C_CHAR, len=:), allocatable, intent(in) :: default_value

  integer(kind=C_INT) :: ierror
  type(c_ptr) :: item_ptr, key_ptr

  ierror = box_value(key_ptr, key)
  if (ierror /= 0_C_INT) then
    return
  endif

  ierror = dict_get_helper2(self, item_ptr, key_ptr)
  
  if (c_associated(item_ptr) .and. ierror == 0_C_INT) then
    ierror = unbox_value(item, item_ptr)
    call Py_Decref(item_ptr)
  else
    item = default_value
  endif
  
  call Py_Decref(key_ptr)
  
end function


function dict_setdefault_object_object(self, the_value, key, default_value) result(ierror)
  class(dict), intent(inout) :: self
  !> The value retrieved from the dict.
  type(object), intent(out) :: the_value
  !> Key to retrieve value from.
  class(object), intent(in) :: key
  !> Default value that is inserted into dict and returned., if key is not present.
  class(object), intent(in) :: default_value
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror

  ierror = dict_get_helper(self, the_value%py_object, key%py_object, default_value%py_object, .true.)
  
end function

function dict_get_helper(self, item_ptr, key_ptr, default_value_ptr, setdefault) result(ierror)
  class(dict), intent(inout) :: self
  type(c_ptr), intent(out) :: item_ptr  
  type(c_ptr), intent(in) :: key_ptr
  type(c_ptr), intent(in) :: default_value_ptr
  logical, intent(in) :: setdefault
  integer(kind=C_INT) :: ierror

  ierror = 0_C_INT
  item_ptr = PyObject_GetItem(self%py_object, key_ptr)

  if (.not. c_associated(item_ptr)) then
    !always return default value, when lookup fails
    item_ptr = default_value_ptr
    call Py_IncRef(default_value_ptr)
  
    if (exception_matches(KeyError)) then
      call err_clear()
      
      if (setdefault) then
        ierror = PyObject_SetItem(self%py_object, key_ptr, default_value_ptr)  
      endif
    else
      ierror = EXCEPTION_ERROR
      return
    endif
  endif
end function

function dict_get_helper2(self, item_ptr, key_ptr) result(ierror)
  class(dict), intent(inout) :: self
  type(c_ptr), intent(out) :: item_ptr  
  type(c_ptr), intent(in) :: key_ptr
  integer(kind=C_INT) :: ierror

  ierror = 0_C_INT
  item_ptr = PyObject_GetItem(self%py_object, key_ptr)

  if (.not. c_associated(item_ptr)) then
    if (exception_matches(KeyError)) then
      call err_clear()
    else
      ierror = EXCEPTION_ERROR
      return
    endif
  endif
end function

!-----------------------------------------------------------------------------------------------------
! bytes, str and unicode

function bytes_create_chars(r, string) result(ierror)
  type(bytes), intent(out) :: r
  character(kind=C_CHAR, len=*), intent(in) :: string
  integer(kind=C_INT) :: ierror

  ierror = box_value_chars_as_bytestr(r%py_object, string)
end function

function bytes_create_char_1d(r, string) result(ierror)
  type(bytes), intent(out) :: r
  character(kind=C_CHAR), dimension(:), intent(in) :: string
  integer(kind=C_INT) :: ierror

  ierror = box_value_char_1d_as_bytestr(r%py_object, string)
end function

function unicode_create_chars(r, string) result(ierror)
  type(unicode), intent(out) :: r
  character(kind=C_CHAR, len=*), intent(in) :: string
  integer(kind=C_INT) :: ierror

  ierror = box_value_chars_as_unicodestr(r%py_object, string)
end function

function unicode_create_char_1d(r, string) result(ierror)
  type(unicode), intent(out) :: r
  character(kind=C_CHAR), dimension(:), intent(in) :: string
  integer(kind=C_INT) :: ierror

  ierror = box_value_char_1d_as_unicodestr(r%py_object, string)
end function

function str_create_chars(r, string) result(ierror)
  type(str), intent(out) :: r
  character(kind=C_CHAR, len=*), intent(in) :: string
  integer(kind=C_INT) :: ierror

#ifdef PYTHON2
  ierror = box_value_chars_as_bytestr(r%py_object, string)
#else
  ierror = box_value_chars_as_unicodestr(r%py_object, string)
#endif
end function

function str_create_char_1d(r, string) result(ierror)
  type(str), intent(out) :: r
  character(kind=C_CHAR), dimension(:), intent(in) :: string
  integer(kind=C_INT) :: ierror

#ifdef PYTHON2
  ierror = box_value_char_1d_as_bytestr(r%py_object, string)
#else
  ierror = box_value_char_1d_as_unicodestr(r%py_object, string)
#endif
end function

!-----------------------------------------------------------------------------------------------------
!> Import a Python module.
!>
!> Module must be in Python module search path.
!> The search path can be set via the environment variable PYTHONPATH or by
!> manipulating the list of paths retrieved by using [[get_sys_path]].
function import_py(mod_py, mod_name) result(ierror)
  !> The imported module
  type(module_py), intent(out) :: mod_py
  !> Name of the module to import. Name can include dots "."
  character(kind=C_CHAR, len=*), intent(in) :: mod_name
  !> Error code, 0 on success.
  integer(kind=C_INT) :: ierror

  ierror = 0_C_INT
  mod_py%py_object = PyImport_ImportModule(trim(mod_name) // C_NULL_CHAR)

  if (.not. c_associated(mod_py%py_object)) then
    ierror = EXCEPTION_ERROR
  endif
end function

!-----------------------------------------------------------------------------------------------------
!> Calling an object (method, function...) that is a member of another Python object (module,...).
!>
!> Python equivalent: return_value = obj.attr_name(*args, **kwargs)
function call_py_attribute(return_value, obj, attr_name, args, kwargs) result(ierror)
  type(object), intent(out) :: return_value
  class(object), intent(in) :: obj
  character(kind=C_CHAR, len=*), intent(in) :: attr_name
  class(tuple), intent(in), optional :: args
  class(dict), intent(in), optional :: kwargs

  integer(kind=C_INT) :: ierror

  type(object) :: obj_to_call
  ierror = obj%getattribute(obj_to_call, attr_name)

  if (ierror == 0) then
  
    if (present(kwargs) .and. present(args)) then
      ierror = call_py_object(return_value, obj_to_call, args, kwargs)
    elseif (present(args)) then
      ierror = call_py_object_nokwargs(return_value, obj_to_call, args)
    elseif (present(kwargs)) then
      ierror = call_py_object_only_kwargs(return_value, obj_to_call, kwargs)
    else
      ierror = call_py_object_noargs(return_value, obj_to_call)
    endif
    
    call obj_to_call%destroy()
  endif
end function

!> Calling a Python object (method, function...).
!>
!> Python equivalent: return_value = obj_to_call(*args, **kwargs)
function call_py_object(return_value, obj_to_call, args, kwargs) result(ierror)
  type(object), intent(out) :: return_value
  class(object), intent(in) :: obj_to_call
  class(tuple), intent(in):: args
  class(dict), intent(in) :: kwargs

  integer(kind=C_INT) :: ierror

  type(c_ptr) :: kw, args_ptr

  ierror = 0_C_INT

  kw = kwargs%py_object
  args_ptr = args%py_object

  return_value%py_object = PyObject_Call(obj_to_call%py_object, args_ptr, kw)

  if (.not. c_associated(return_value%py_object)) then
    ierror = EXCEPTION_ERROR
  endif

end function

!> Python equivalent: return_value = obj_to_call(*args)
function call_py_object_nokwargs(return_value, obj_to_call, args) result(ierror)
  type(object), intent(out) :: return_value
  class(object), intent(in) :: obj_to_call
  class(tuple), intent(in):: args

  integer(kind=C_INT) :: ierror
  type(dict) :: kwargs

  ierror = dict_create(kwargs)
  if (ierror == 0) then
    ierror = call_py_object(return_value, obj_to_call, args, kwargs)
    call kwargs%destroy
  endif
end function

!> Python equivalent: return_value = obj_to_call()
function call_py_object_noargs(return_value, obj_to_call) result(ierror)
  type(object), intent(out) :: return_value
  class(object), intent(in) :: obj_to_call

  integer(kind=C_INT) :: ierror
  type(tuple) :: args

  ierror = tuple_create(args, 0)
  if (ierror == 0) then
    ierror = call_py_object_nokwargs(return_value, obj_to_call, args)
    call args%destroy
  endif
end function

!> Python equivalent: return_value = obj_to_call(**kwargs)
function call_py_object_only_kwargs(return_value, obj_to_call, kwargs) result(ierror)
  type(object), intent(out) :: return_value
  class(object), intent(in) :: obj_to_call
  class(dict), intent(in) :: kwargs

  integer(kind=C_INT) :: ierror
  type(tuple) :: args

  ierror = tuple_create(args, 0)
  if (ierror == 0) then
    ierror = call_py_object(return_value, obj_to_call, args, kwargs)
    call args%destroy
  endif
end function

!> Python equivalent: obj.attr_name(*args, **kwargs)
function call_py_noret_attribute(obj, attr_name, args, kwargs) result(ierror)
  class(object), intent(in) :: obj
  character(kind=C_CHAR, len=*), intent(in) :: attr_name
  class(tuple), intent(in), optional :: args
  class(dict), intent(in), optional :: kwargs

  integer(kind=C_INT) :: ierror
  type(object) :: return_value
  
  if (present(kwargs) .and. present(args)) then
    ierror = call_py(return_value, obj, attr_name, args, kwargs)
  elseif (present(args)) then
    ierror = call_py(return_value, obj, attr_name, args)
  elseif (present(kwargs)) then
    ierror = call_py(return_value, obj, attr_name, kwargs=kwargs)
  else
    ierror = call_py(return_value, obj, attr_name)
  endif

  call return_value%destroy    

end function

!> Python equivalent: obj.attr_name(*args, **kwargs)
function call_py_noret_object(obj_to_call, args, kwargs) result(ierror)
  class(object), intent(in) :: obj_to_call
  class(tuple), intent(in), optional :: args
  class(dict), intent(in), optional :: kwargs

  integer(kind=C_INT) :: ierror
  type(object) :: return_value
  
  if (present(kwargs) .and. present(args)) then
    ierror = call_py(return_value, obj_to_call, args, kwargs)
  elseif (present(args)) then
    ierror = call_py(return_value, obj_to_call, args)
  elseif (present(kwargs)) then
    ierror = call_py(return_value, obj_to_call, kwargs=kwargs)
  else
    ierror = call_py(return_value, obj_to_call)
  endif

  call return_value%destroy    
end function

!-----------------------------------------------------------------------------------------------------
! Numpy ndarray support

function ndarray_create_nocopy_int32_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  integer(kind=int32), dimension(:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 4_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "i" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "int32")
#endif

end function

function ndarray_create_int32_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  integer(kind=int32), dimension(:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int32), dimension(:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "int32")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_int32_1d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  integer(kind=int32), dimension(:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "i" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_int64_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  integer(kind=int64), dimension(:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "l" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "int64")
#endif

end function

function ndarray_create_int64_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  integer(kind=int64), dimension(:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int64), dimension(:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "int64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_int64_1d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  integer(kind=int64), dimension(:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "l" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_real32_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  real(kind=real32), dimension(:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 4_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "f" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "float32")
#endif

end function

function ndarray_create_real32_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  real(kind=real32), dimension(:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  real(kind=real32), dimension(:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "float32")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_real32_1d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  real(kind=real32), dimension(:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "f" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_real64_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  real(kind=real64), dimension(:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "d" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "float64")
#endif

end function

function ndarray_create_real64_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  real(kind=real64), dimension(:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  real(kind=real64), dimension(:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "float64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_real64_1d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  real(kind=real64), dimension(:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "d" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_complex_real32_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  complex(kind=real32), dimension(:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "Zf" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "complex64")
#endif

end function

function ndarray_create_complex_real32_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  complex(kind=real32), dimension(:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  complex(kind=real32), dimension(:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "complex64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_complex_real32_1d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  complex(kind=real32), dimension(:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "Zf" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_complex_real64_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  complex(kind=real64), dimension(:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 16_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "Zd" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "complex128")
#endif

end function

function ndarray_create_complex_real64_1d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  complex(kind=real64), dimension(:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  complex(kind=real64), dimension(:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "complex128")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_complex_real64_1d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  complex(kind=real64), dimension(:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 1
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "Zd" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_int32_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  integer(kind=int32), dimension(:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 4_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "i" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "int32")
#endif

end function

function ndarray_create_int32_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  integer(kind=int32), dimension(:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int32), dimension(:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "int32")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_int32_2d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  integer(kind=int32), dimension(:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "i" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_int64_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  integer(kind=int64), dimension(:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "l" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "int64")
#endif

end function

function ndarray_create_int64_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  integer(kind=int64), dimension(:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int64), dimension(:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "int64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_int64_2d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  integer(kind=int64), dimension(:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "l" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_real32_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  real(kind=real32), dimension(:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 4_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "f" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "float32")
#endif

end function

function ndarray_create_real32_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  real(kind=real32), dimension(:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  real(kind=real32), dimension(:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "float32")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_real32_2d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  real(kind=real32), dimension(:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "f" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_real64_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  real(kind=real64), dimension(:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "d" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "float64")
#endif

end function

function ndarray_create_real64_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  real(kind=real64), dimension(:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  real(kind=real64), dimension(:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "float64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_real64_2d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  real(kind=real64), dimension(:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "d" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_complex_real32_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  complex(kind=real32), dimension(:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "Zf" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "complex64")
#endif

end function

function ndarray_create_complex_real32_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  complex(kind=real32), dimension(:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  complex(kind=real32), dimension(:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "complex64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_complex_real32_2d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  complex(kind=real32), dimension(:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "Zf" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_complex_real64_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  complex(kind=real64), dimension(:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 16_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "Zd" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "complex128")
#endif

end function

function ndarray_create_complex_real64_2d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  complex(kind=real64), dimension(:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  complex(kind=real64), dimension(:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "complex128")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_complex_real64_2d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  complex(kind=real64), dimension(:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 2
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "Zd" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_int32_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  integer(kind=int32), dimension(:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 4_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "i" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "int32")
#endif

end function

function ndarray_create_int32_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  integer(kind=int32), dimension(:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int32), dimension(:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "int32")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_int32_3d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  integer(kind=int32), dimension(:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "i" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_int64_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  integer(kind=int64), dimension(:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "l" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "int64")
#endif

end function

function ndarray_create_int64_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  integer(kind=int64), dimension(:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int64), dimension(:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "int64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_int64_3d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  integer(kind=int64), dimension(:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "l" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_real32_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  real(kind=real32), dimension(:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 4_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "f" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "float32")
#endif

end function

function ndarray_create_real32_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  real(kind=real32), dimension(:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  real(kind=real32), dimension(:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "float32")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_real32_3d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  real(kind=real32), dimension(:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "f" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_real64_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  real(kind=real64), dimension(:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "d" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "float64")
#endif

end function

function ndarray_create_real64_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  real(kind=real64), dimension(:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  real(kind=real64), dimension(:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "float64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_real64_3d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  real(kind=real64), dimension(:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "d" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_complex_real32_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  complex(kind=real32), dimension(:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "Zf" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "complex64")
#endif

end function

function ndarray_create_complex_real32_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  complex(kind=real32), dimension(:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  complex(kind=real32), dimension(:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "complex64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_complex_real32_3d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  complex(kind=real32), dimension(:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "Zf" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_complex_real64_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  complex(kind=real64), dimension(:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 16_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "Zd" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "complex128")
#endif

end function

function ndarray_create_complex_real64_3d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  complex(kind=real64), dimension(:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  complex(kind=real64), dimension(:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "complex128")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_complex_real64_3d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  complex(kind=real64), dimension(:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 3
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "Zd" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_int32_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  integer(kind=int32), dimension(:,:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 4_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "i" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "int32")
#endif

end function

function ndarray_create_int32_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  integer(kind=int32), dimension(:,:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int32), dimension(:,:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "int32")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_int32_4d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  integer(kind=int32), dimension(:,:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "i" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_int64_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  integer(kind=int64), dimension(:,:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "l" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "int64")
#endif

end function

function ndarray_create_int64_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  integer(kind=int64), dimension(:,:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int64), dimension(:,:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "int64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_int64_4d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  integer(kind=int64), dimension(:,:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "l" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_real32_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  real(kind=real32), dimension(:,:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 4_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "f" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "float32")
#endif

end function

function ndarray_create_real32_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  real(kind=real32), dimension(:,:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  real(kind=real32), dimension(:,:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "float32")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_real32_4d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  real(kind=real32), dimension(:,:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "f" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_real64_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  real(kind=real64), dimension(:,:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "d" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "float64")
#endif

end function

function ndarray_create_real64_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  real(kind=real64), dimension(:,:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  real(kind=real64), dimension(:,:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "float64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_real64_4d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  real(kind=real64), dimension(:,:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "d" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_complex_real32_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  complex(kind=real32), dimension(:,:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 8_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "Zf" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "complex64")
#endif

end function

function ndarray_create_complex_real32_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  complex(kind=real32), dimension(:,:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  complex(kind=real32), dimension(:,:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "complex64")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_complex_real32_4d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  complex(kind=real32), dimension(:,:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "Zf" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function

function ndarray_create_nocopy_complex_real64_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> The Fortran array to wrap as ndarray. NO copy is made. Changes to the ndarray affect the Fortran array and
  !> vice versa. MUST be a contiguous array (this is not checked).
  ! Note: can not use the F2008 CONTIGUOUS attribute here, because a 
  ! temporary copy of array could be created with limited lifetime.
  complex(kind=real64), dimension(:,:,:,:), target, intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), parameter :: ITEMSIZE = 16_PY_SSIZE_T_KIND

#ifndef PYTHON2  
  ierror = ndarray_create_nocopy_helper(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "Zd" // C_NULL_CHAR)
#else  
  ierror = ndarray_create_nocopy_helper_py2(res, c_loc(array), shape(array, kind=PY_SSIZE_T_KIND), NDIM, ITEMSIZE, "complex128")
#endif

end function

function ndarray_create_complex_real64_4d(res, array) result(ierror)
  !> The resulting ndarray (in Fortran storage order).
  type(ndarray), intent(out) :: res
  !> Create a new ndarray with a copy of the data given in 'array'
  complex(kind=real64), dimension(:,:,:,:), intent(in) :: array
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  complex(kind=real64), dimension(:,:,:,:), pointer :: ptr
  
  ierror = ndarray_create_empty(res, shape(array, kind=PY_SSIZE_T_KIND), "complex128")
  if (ierror /= 0_C_INT) return
  ierror = res%get_data(ptr)
  if (ierror /= 0_C_INT) then
    call res%destroy
    res%py_object = C_NULL_PTR
    return
  endif
  ptr = array
end function

!> Get pointer to data of numpy array
!> 
!> Raises BufferError, if array is not contiguous (does not have the required Fortran or C storage order)
!> Raises TypeError, if Fortran pointer datatype is not compatible with numpy datatype.
function get_data_complex_real64_4d(self, ptr, order) result(ierror)
  class(ndarray), intent(in) :: self
  !> Pointer to the numerical data of the Fortran array.
  complex(kind=real64), dimension(:,:,:,:), pointer, intent(out) :: ptr
  !> Only retrieve data, when ndarray has certain order.
  !>
  !> If 'F' (default), only retrieve the data when the ndarray has Fortran storage order.
  !> If 'C', only retrieve the data when the ndarray has C storage order.
  !> ATTENTION: The data, the pointer points to, then will be the transpose of the array.
  !> If 'A' accept Fortran or C order. If C, the data will be transposed.
  character(kind=C_CHAR), optional, intent(in) :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(c_ptr) :: raw_ptr
  integer, parameter :: NDIM = 4
  integer(kind=PY_SSIZE_T_KIND), dimension(NDIM) :: shape_info

  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  nullify(ptr)  
  ierror = get_data_helper(self, raw_ptr, shape_info, NDIM, "Zd" // C_NULL_CHAR, the_order)

  if (ierror == 0_C_INT) then
    call c_f_pointer(raw_ptr, ptr, shape=shape_info)
    if (.not. associated(ptr)) then
      ierror = EXCEPTION_ERROR
      call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    endif
  endif
end function


#ifndef PYTHON2
function ndarray_create_nocopy_helper(res, array_c_loc, array_shape, ndim, itemsize, format_c_string) result(ierror)
  type(ndarray), intent(inout) :: res
  type(c_ptr), intent(in) :: array_c_loc
  integer, intent(in) :: ndim
  integer(kind=PY_SSIZE_T_KIND), target, dimension(ndim), intent(in) :: array_shape
  integer(kind=PY_SSIZE_T_KIND) :: itemsize
  character(kind=C_CHAR, len=*), target, intent(in) :: format_c_string  
  integer(kind=C_INT) :: ierror

  type(Py_buffer) :: buffer
  integer(kind=PY_SSIZE_T_KIND), target, dimension(ndim) :: strides
  integer(kind=PY_SSIZE_T_KIND) :: length
  type(c_ptr) :: mem_view
  type(c_ptr) :: args
  integer :: ii

  !TODO: check if numpy was successfully imported

  ierror = 0_C_INT
  res%py_object = C_NULL_PTR
  
  length = 1
  do ii = 1, ndim
    length = length * array_shape(ii)
  enddo

  ! calculate the strides assuming Fortran-order
  call PyBuffer_FillContiguousStrides(int(ndim, kind=C_INT), c_loc(array_shape), c_loc(strides), itemsize, 'F')

  buffer%buf = array_c_loc
  buffer%obj = C_NULL_PTR
  buffer%len = length * ITEMSIZE
  buffer%itemsize = itemsize
  buffer%readonly = 0_C_INT
  buffer%ndim = int(ndim, C_INT)
  buffer%format = c_loc(format_c_string)
  buffer%shape = c_loc(array_shape)
  buffer%strides = c_loc(strides)
  buffer%suboffsets = C_NULL_PTR
#ifdef PYTHON2
  buffer%smalltable = 0_PY_SSIZE_T_KIND
#endif
  buffer%internal = C_NULL_PTR

  mem_view = PyMemoryView_FromBuffer(buffer)
  if (.not. c_associated(mem_view)) then
    ierror = -1_C_INT
    return
  endif

  args = PyTuple_New(1_PY_SSIZE_T_KIND)
  if (c_associated(args)) then
    ierror = PyTuple_SetItem(args, 0_PY_SSIZE_T_KIND, mem_view) ! steals reference to mem_view even if it fails

    if (ierror /= 0_C_INT) then
      call Py_Decref(args)
      return
    endif
    
    res%py_object = PyObject_Call(global_numpy_asarray_method, args, C_NULL_PTR)
    call Py_Decref(args)

    if (.not. c_associated(res%py_object)) then
      ierror = -1_C_INT
    endif
    
  else ! .not. c_associated(args)
    call Py_Decref(mem_view)
    ierror = -1_C_INT
  endif

end function
#endif

#ifdef PYTHON2
! Python 2 array wrapper creation using old-style, py2-only buffer object + np.frombuffer.
! In principle with Py 2.7 it would be possible to use the same code as 
! in the Py 3 case, but the memoryview + np.asarray
! approach is somewhat buggy in Py 2 and one reference is lost 
function ndarray_create_nocopy_helper_py2(res, array_c_loc, array_shape, ndim, itemsize, dtype) result(ierror)
  type(ndarray), intent(inout) :: res
  type(c_ptr), intent(in) :: array_c_loc
  integer, intent(in) :: ndim
  integer(kind=PY_SSIZE_T_KIND), target, dimension(ndim), intent(in) :: array_shape
  integer(kind=PY_SSIZE_T_KIND) :: itemsize
  character(kind=C_CHAR, len=*), target, intent(in) :: dtype 
  integer(kind=C_INT) :: ierror

  integer(kind=PY_SSIZE_T_KIND) :: length
  type(c_ptr) :: buffer_obj
  type(module_py) :: numpy
  type(tuple) :: args
  type(object) :: retval
  type(object) :: buffer
  type(ndarray) :: reshaped_array
  integer :: ii

  ierror = -1_C_INT
  res%py_object = C_NULL_PTR
  
  length = 1
  do ii = 1, ndim
    length = length * array_shape(ii)
  enddo

  buffer_obj = PyBuffer_FromReadWriteMemory(array_c_loc, length*itemsize)
  if (.not. c_associated(buffer_obj)) then
    return
  endif
  
  buffer%py_object = buffer_obj
  
  ierror = tuple_create(args, 2_PY_SSIZE_T_KIND)
  if (ierror /= 0_C_INT) then
    call buffer%destroy
    return
  endif
  
  ierror = args%setitem(0_PY_SSIZE_T_KIND, buffer)
  if (ierror /= 0_C_INT) then
    call args%destroy
    call buffer%destroy
    return
  endif
  
  ierror = args%setitem(1_PY_SSIZE_T_KIND, dtype)
  if (ierror /= 0_C_INT) then
    call args%destroy
    call buffer%destroy
    return
  endif 
  
  numpy%py_object = global_numpy_mod
  ierror = call_py(retval, numpy, "frombuffer", args)
  call args%destroy
  call buffer%destroy
  
  if (ierror == 0_C_INT) then
    res%py_object = retval%py_object
  else
    call retval%destroy
    return
  endif
  
  if (ndim > 1) then
    ierror = ndarray_reshape_helper(reshaped_array, res, array_shape, 'F')
    call res%destroy
    
    if (ierror == 0_C_INT) then
      res%py_object = reshaped_array%py_object
    else
      res%py_object = C_NULL_PTR
    endif
  endif
end function

function ndarray_reshape_helper(reshaped_array, array, new_shape, order) result(ierror)
  type(ndarray), intent(out) :: reshaped_array
  class(ndarray), intent(in) :: array
  integer(kind=PY_SSIZE_T_KIND), dimension(:), intent(in) :: new_shape
  character(kind=C_CHAR), intent(in) :: order
  integer(kind=C_INT) :: ierror
  
  type(tuple) :: args
  type(dict) :: kwargs
  type(tuple) :: new_shape_tuple
  type(object) :: retval
  
  reshaped_array%py_object = C_NULL_PTR
  
  ierror = tuple_from_array(new_shape_tuple, new_shape)
  if (ierror /= 0_C_INT) then
    return
  endif
  
  ierror = tuple_create(args, 1_PY_SSIZE_T_KIND)
  if (ierror /= 0_C_INT) then
    call new_shape_tuple%destroy
    return
  endif
  
  ierror = args%setitem(0_PY_SSIZE_T_KIND, new_shape_tuple)
  if (ierror /= 0_C_INT) then
    call args%destroy
    call new_shape_tuple%destroy
    return
  endif
  
  ierror = dict_create(kwargs)
  if (ierror /= 0_C_INT) then
    call args%destroy
    call new_shape_tuple%destroy
    return
  endif
  
  ierror = kwargs%setitem("order", order)
  if (ierror /= 0_C_INT) then
    call kwargs%destroy
    call args%destroy
    call new_shape_tuple%destroy
    return
  endif   
  
  ierror = call_py(retval, array, "reshape", args, kwargs)
  if (ierror == 0_C_INT) then
    ierror = cast(reshaped_array, retval)
    call retval%destroy
  endif
  
  call kwargs%destroy
  call args%destroy
  call new_shape_tuple%destroy 
end function
#endif

!> Get pointer to data of numpy array
function get_data_helper(self, raw_ptr, shape_info, ndim, format_c_string, order) result(ierror)
  class(ndarray), intent(in) :: self
  type(c_ptr), intent(out) :: raw_ptr
  integer, intent(in) :: ndim  
  integer(kind=PY_SSIZE_T_KIND), dimension(ndim), intent(out) :: shape_info
  character(kind=C_CHAR, len=*), target, intent(in) :: format_c_string
  character(kind=C_CHAR), intent(in) :: order
  
  integer(kind=C_INT) :: ierror, flag
  type(Py_buffer) :: buffer
  integer(kind=PY_SSIZE_T_KIND), dimension(:), pointer :: shape_ptr
  integer :: shape_info_shape(1)
  character(kind=C_CHAR) :: detected_order
  character(kind=C_CHAR,len=60) :: error_message

  ! order can have values 'C', 'F' or 'A'
  if (index('CFA', order) == 0) then
    ierror = EXCEPTION_ERROR
    call raise_exception(ValueError, "ndarray%get_data: order parameter must be 'F', 'C' or 'A'")
    return
  endif

  shape_info_shape(1) = ndim

  ! raises BufferError exception if array is not contiguous, Python 2: ValueError
  ierror = PyObject_GetBuffer(self%py_object, buffer, 156_C_INT) !flags (PyBUF_FORMAT | PyBUF_ANY_CONTIGUOUS) - we need the format info and PyBUF_FORMAT alone gives error
  
  if (ierror /= 0) then
    if (exception_matches(BufferError) .or. exception_matches(ValueError)) then ! make error message more informative
      call err_clear
      call raise_exception(BufferError, "forpy: ndarray with non-contiguous data. Fortran or C-order needed: try to copy array")
    endif
    return
  endif

  detected_order = 'N'
  flag = PyBuffer_IsContiguous(buffer, 'F')
  if (flag == 1_C_INT) then
    detected_order = 'F'
  else
    flag = PyBuffer_IsContiguous(buffer, 'C')
    if (flag == 1_C_INT) then
      detected_order = 'C'
    endif
  endif
  
  if ((detected_order == 'N') .or. (ndim > 1 .and. order /= 'A' .and. order /= detected_order)) then
    ierror = EXCEPTION_ERROR
    if (order=='F') then
      error_message = "forpy: expected Fortran-ordered array"
    elseif (order=='C') then
      error_message = "forpy: expected C-ordered array"
    else
      error_message = "forpy: expected contiguous array"
    endif
    call PyBuffer_Release(buffer)
    call raise_exception(BufferError, error_message)
    return
  endif

  if (buffer%ndim /= ndim) then
    ierror = EXCEPTION_ERROR
    write(error_message,fmt="('forpy: expected array of rank ',I1,', got array of rank ',I1)") ndim, buffer%ndim
    call PyBuffer_Release(buffer)
    call raise_exception(TypeError, error_message)
    return
  endif

  !get shape info
  call c_f_pointer(buffer%shape, shape_ptr, shape=SHAPE_INFO_SHAPE)

  !check if correct format
  if (associated(shape_ptr)) then
    if (get_data_helper_check_dtype(buffer%format, format_c_string) /= 0) then
      ierror = EXCEPTION_ERROR
      call PyBuffer_Release(buffer)
      call raise_exception(TypeError, "forpy: ndarray%get_data - datatype of data pointer is incompatible with ndarray")
      return
    endif
  else
    ierror = EXCEPTION_ERROR
    call PyBuffer_Release(buffer)
    call raise_exception(RuntimeError, "forpy: Could not determine shape of ndarray")
    return
  endif

  raw_ptr = buffer%buf

  call get_shape_info_helper(shape_info, shape_ptr, detected_order)
  call PyBuffer_Release(buffer)

end function

! returns 0 if dtype matches fortran type, non-zero if not
function get_data_helper_check_dtype(buffer_format, format_c_string) result(flag)
  type(c_ptr), intent(in) :: buffer_format
  character(kind=C_CHAR, len=*), target, intent(in) :: format_c_string
  
  character(kind=C_CHAR, len=2), target :: format_code
  integer(kind=C_INT) :: flag
  
  flag = 1_C_INT
      
  ! the Python buffer format codes corresponding to int32 and int64 are systems dependent... 
   
  if (format_c_string == "i" // C_NULL_CHAR) then  ! buffer type compatible with int32 requested
    if (int32 == C_INT) then
      format_code = "i" // C_NULL_CHAR
      flag = strcmp(buffer_format, c_loc(format_code))
    endif
    if (flag /= 0) then
      if (int32 == C_LONG) then
        format_code = "l" // C_NULL_CHAR
        flag = strcmp(buffer_format, c_loc(format_code))
      endif
    endif
    return
  endif
  
  if (format_c_string == "l" // C_NULL_CHAR) then  ! buffer type compatible with int64 requested
    if (int64 == C_LONG) then
      format_code = "l" // C_NULL_CHAR
      flag = strcmp(buffer_format, c_loc(format_code))
    endif
    if (flag /= 0) then
      if (int64 == C_LONG_LONG) then
        format_code = "q" // C_NULL_CHAR
        flag = strcmp(buffer_format, c_loc(format_code))
      endif
    endif
    if (flag /= 0) then
      if (int64 == C_INT) then
        format_code = "i" // C_NULL_CHAR
        flag = strcmp(buffer_format, c_loc(format_code))
      endif
    endif
    return
  endif
  
  ! this handles all the non-integer cases    
  flag = strcmp(buffer_format, c_loc(format_c_string))
end function

subroutine get_shape_info_helper(shape_info, shape_ptr, order)
  integer(kind=PY_SSIZE_T_KIND), dimension(:), intent(out) :: shape_info
  integer(kind=PY_SSIZE_T_KIND), dimension(:), intent(in) :: shape_ptr
  character(kind=C_CHAR), intent(in) :: order
  
  integer ii, length
  
  if (order == 'F') then
    shape_info = shape_ptr
  elseif (order == 'C') then ! C-order: reverse shape information ("transpose")
    length = size(shape_info)
    do ii = 1, length
      shape_info(ii) = shape_ptr(length-ii+1)
    enddo
  endif
end subroutine

!> Return transpose of a ndarray.
function ndarray_transpose(self, transposed_array) result(ierror)
  class(ndarray), intent(in) :: self
  type(ndarray), intent(out) :: transposed_array
  integer(kind=C_INT) :: ierror
  
  type(object) :: retval
  
  ierror = call_py(retval, self, "transpose")
  if (ierror == 0_C_INT) then
    ierror = cast(transposed_array, retval)
    call retval%destroy
  endif
end function

!> Returns copy of a ndarray
!>
!> order (optional) can be 'F', 'C', 'A' or 'K' 
!> (default is 'F' - in numpy it is 'C')
function ndarray_copy(self, array_copy, order) result(ierror)
  class(ndarray), intent(in) :: self
  type(ndarray), intent(out) :: array_copy
  character(kind=C_CHAR), intent(in), optional :: order
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR) :: the_order
  type(object) :: retval
  type(tuple) :: args
  
  if (.not. present(order)) then
    the_order = 'F'
  else
    the_order = order
  endif
  
  ierror = tuple_create(args, 1_PY_SSIZE_T_KIND)
  if (ierror /= 0_C_INT) then
    return
  endif
    
  ierror = args%setitem(0_PY_SSIZE_T_KIND, the_order)
  if (ierror == 0_C_INT) then
    ierror = call_py(retval, self, "copy", args)
  endif

  call args%destroy
  
  if (ierror == 0_C_INT) then
    ierror = cast(array_copy, retval)
    call retval%destroy
  endif
end function

!> Checks if data of ndarray has a specific storage-order.
!>
!> order can be 'F' (Fortran-order), 'C' (C-order), 'A' (Fortran- or C-order)
function ndarray_is_ordered(self, order) result(is_ordered)
  class(ndarray), intent(in) :: self
  character(kind=C_CHAR), intent(in) :: order
  logical :: is_ordered
  
  type(object) :: retval, flags
  integer(kind=C_INT) :: ierror
  logical :: c_ordered, fortran_ordered, check_c, check_f
  
  is_ordered = .false.
  c_ordered = .false.
  fortran_ordered = .false.
  
  if (have_exception()) return
  
  ierror = self%getattribute(flags, "flags")
  if (ierror /= 0_C_INT) then
    call err_clear
    return
  endif
 
  check_f = (order == 'F') .or. (order == 'A') 

  if (check_f) then
    ierror = flags%getattribute(retval, "f_contiguous")
    if (ierror /= 0_C_INT) then
      call flags%destroy
      call err_clear
      return
    endif
    ierror = cast(fortran_ordered, retval)
    call retval%destroy
    if (ierror /= 0_C_INT) then
      call flags%destroy
      call err_clear
      return
    endif
  endif
  
  check_c = (order == 'C') .or. (order == 'A' .and. .not. fortran_ordered)
  
  if (check_c) then
    ierror = flags%getattribute(retval, "c_contiguous")
    if (ierror /= 0_C_INT) then
      call flags%destroy
      call err_clear
      return
    endif
    ierror = cast(c_ordered, retval)
    call retval%destroy
    if (ierror /= 0_C_INT) then
      call flags%destroy
      call err_clear
      return
    endif
  endif
  
  call flags%destroy
  is_ordered = fortran_ordered .or. c_ordered
  
end function

!> Returns type string of ndarray.
!>
!> corresponds to Python's ndarray.dtype.name property
function ndarray_get_dtype_name(self, dtype_name) result(ierror)
  class(ndarray), intent(in) :: self
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: dtype_name
  integer(kind=C_INT) :: ierror
  
  type(object) :: dtype, dname
  
  ierror = self%getattribute(dtype, "dtype")
  if (ierror /= 0_C_INT) then
    return
  endif
  
  ierror = dtype%getattribute(dname, "name")
  if (ierror /= 0_C_INT) then
    call dtype%destroy
    return
  endif
  
  ierror = cast(dtype_name, dname)
  call dtype%destroy
  call dname%destroy
end function

!> Returns dimensionality of ndarray (ndarray.ndim)
function ndarray_ndim_int32(self, ndim) result(ierror)
  class(ndarray), intent(in) :: self
  !> Output: dimensionality of array.
  integer(kind=int32), intent(out) :: ndim
  integer(kind=C_INT) :: ierror
  
  type(object) :: ndim_obj
  
  ierror = self%getattribute(ndim_obj, "ndim")
  if (ierror /= 0_C_INT) then
    return
  endif
  
  ierror = cast(ndim, ndim_obj)
  call ndim_obj%destroy
end function

!> Returns dimensionality of ndarray (ndarray.ndim)
function ndarray_ndim_int64(self, ndim) result(ierror)
  class(ndarray), intent(in) :: self
  !> Output: dimensionality of array.
  integer(kind=int64), intent(out) :: ndim
  integer(kind=C_INT) :: ierror
  
  type(object) :: ndim_obj
  
  ierror = self%getattribute(ndim_obj, "ndim")
  if (ierror /= 0_C_INT) then
    return
  endif
  
  ierror = cast(ndim, ndim_obj)
  call ndim_obj%destroy
end function


!---Routines for creating ndarrays with Python managed storage ---------
!   numpy.empty, numpy.ones, numpy.zeros

function ndarray_create_empty_aint32(array, a_shape, dtype, order) result(ierror)
  !> The resulting ndarray.
  type(ndarray), intent(out) :: array
  !> Shape of ndarray to create.
  integer(kind=int32), dimension(:), intent(in) :: a_shape
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F')
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "empty", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int32(array, "empty", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "empty", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int32(array, "empty", a_shape, "", "F") 
    endif    
  endif
end function

function ndarray_create_empty_aint64(array, a_shape, dtype, order) result(ierror)
  !> The resulting ndarray.
  type(ndarray), intent(out) :: array
  !> Shape of ndarray to create.
  integer(kind=int64), dimension(:), intent(in) :: a_shape
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F')
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "empty", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int64(array, "empty", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "empty", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int64(array, "empty", a_shape, "", "F") 
    endif    
  endif
end function


function ndarray_create_empty_int32(array, length, dtype, order) result(ierror)
  !> The resulting one dimensional ndarray.
  type(ndarray), intent(out) :: array
  !> Number of elements in ndarray
  integer(kind=int32), intent(in) :: length
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F'). In case of 1D array not relevant.
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int32) :: a_shape(1)
  a_shape(1) = length
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "empty", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int32(array, "empty", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "empty", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int32(array, "empty", a_shape, "", "F") 
    endif    
  endif
end function

function ndarray_create_empty_int64(array, length, dtype, order) result(ierror)
  !> The resulting one dimensional ndarray.
  type(ndarray), intent(out) :: array
  !> Number of elements in ndarray
  integer(kind=int64), intent(in) :: length
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F'). In case of 1D array not relevant.
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int64) :: a_shape(1)
  a_shape(1) = length
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "empty", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int64(array, "empty", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "empty", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int64(array, "empty", a_shape, "", "F") 
    endif    
  endif
end function

function ndarray_create_zeros_aint32(array, a_shape, dtype, order) result(ierror)
  !> The resulting ndarray.
  type(ndarray), intent(out) :: array
  !> Shape of ndarray to create.
  integer(kind=int32), dimension(:), intent(in) :: a_shape
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F')
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "zeros", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int32(array, "zeros", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "zeros", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int32(array, "zeros", a_shape, "", "F") 
    endif    
  endif
end function

function ndarray_create_zeros_aint64(array, a_shape, dtype, order) result(ierror)
  !> The resulting ndarray.
  type(ndarray), intent(out) :: array
  !> Shape of ndarray to create.
  integer(kind=int64), dimension(:), intent(in) :: a_shape
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F')
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "zeros", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int64(array, "zeros", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "zeros", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int64(array, "zeros", a_shape, "", "F") 
    endif    
  endif
end function


function ndarray_create_zeros_int32(array, length, dtype, order) result(ierror)
  !> The resulting one dimensional ndarray.
  type(ndarray), intent(out) :: array
  !> Number of elements in ndarray
  integer(kind=int32), intent(in) :: length
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F'). In case of 1D array not relevant.
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int32) :: a_shape(1)
  a_shape(1) = length
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "zeros", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int32(array, "zeros", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "zeros", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int32(array, "zeros", a_shape, "", "F") 
    endif    
  endif
end function

function ndarray_create_zeros_int64(array, length, dtype, order) result(ierror)
  !> The resulting one dimensional ndarray.
  type(ndarray), intent(out) :: array
  !> Number of elements in ndarray
  integer(kind=int64), intent(in) :: length
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F'). In case of 1D array not relevant.
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int64) :: a_shape(1)
  a_shape(1) = length
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "zeros", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int64(array, "zeros", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "zeros", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int64(array, "zeros", a_shape, "", "F") 
    endif    
  endif
end function

function ndarray_create_ones_aint32(array, a_shape, dtype, order) result(ierror)
  !> The resulting ndarray.
  type(ndarray), intent(out) :: array
  !> Shape of ndarray to create.
  integer(kind=int32), dimension(:), intent(in) :: a_shape
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F')
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "ones", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int32(array, "ones", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "ones", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int32(array, "ones", a_shape, "", "F") 
    endif    
  endif
end function

function ndarray_create_ones_aint64(array, a_shape, dtype, order) result(ierror)
  !> The resulting ndarray.
  type(ndarray), intent(out) :: array
  !> Shape of ndarray to create.
  integer(kind=int64), dimension(:), intent(in) :: a_shape
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F')
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "ones", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int64(array, "ones", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "ones", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int64(array, "ones", a_shape, "", "F") 
    endif    
  endif
end function


function ndarray_create_ones_int32(array, length, dtype, order) result(ierror)
  !> The resulting one dimensional ndarray.
  type(ndarray), intent(out) :: array
  !> Number of elements in ndarray
  integer(kind=int32), intent(in) :: length
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F'). In case of 1D array not relevant.
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int32) :: a_shape(1)
  a_shape(1) = length
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "ones", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int32(array, "ones", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int32(array, "ones", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int32(array, "ones", a_shape, "", "F") 
    endif    
  endif
end function

function ndarray_create_ones_int64(array, length, dtype, order) result(ierror)
  !> The resulting one dimensional ndarray.
  type(ndarray), intent(out) :: array
  !> Number of elements in ndarray
  integer(kind=int64), intent(in) :: length
  !> numpy.dtype of ndarray (default: 'float')
  character(kind=C_CHAR, len=*), intent(in), optional :: dtype
  !> Storage order: 'F' (Fortran) or 'C' (default: 'F'). In case of 1D array not relevant.
  character(kind=C_CHAR), intent(in), optional :: order
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer(kind=int64) :: a_shape(1)
  a_shape(1) = length
  
  if (present(dtype)) then
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "ones", a_shape, dtype, order)
    else
      ! Fortran order as default, in contrast to numpy
      ierror = ndarray_create_special_impl_int64(array, "ones", a_shape, dtype, "F") 
    endif
  else
    if (present(order)) then
      ierror = ndarray_create_special_impl_int64(array, "ones", a_shape, "", order)
    else
      ierror = ndarray_create_special_impl_int64(array, "ones", a_shape, "", "F") 
    endif    
  endif
end function


function ndarray_create_special_impl_int32(array, creator_function, a_shape, dtype, order) result(ierror)
  type(ndarray), intent(out) :: array
  character(kind=C_CHAR, len=*), intent(in) :: creator_function
  integer(kind=int32), dimension(:), intent(in) :: a_shape
  character(kind=C_CHAR, len=*), intent(in) :: dtype
  character(kind=C_CHAR), intent(in) :: order
  integer(kind=C_INT) :: ierror
  
  type(dict) :: kwargs
  
  ierror = dict_create(kwargs)
  if (ierror /= 0_C_INT) return
  
  if (len(dtype) > 0) then
    ierror = kwargs%setitem("dtype", dtype)
    if (ierror /= 0_C_INT) then
      call kwargs%destroy
      return
    endif
  endif
  
  ierror = kwargs%setitem("order", order)
 
  if (ierror == 0_C_INT) then
    ierror = ndarray_create_special_helper_int32(array, creator_function, a_shape, kwargs)
  endif
  
  call kwargs%destroy
end function

function ndarray_create_special_getargs_int32(args, a_shape) result(ierror)
  type(tuple), intent(out) :: args
  integer(kind=int32), dimension(:), intent(in) :: a_shape
  integer(kind=C_INT) :: ierror
  
  type(tuple) :: shape_tuple
  
  ierror = tuple_from_array_int32(shape_tuple, a_shape)
  if (ierror /= 0_C_INT) return
  
  ierror = tuple_create(args, 1_PY_SSIZE_T_KIND)
  if (ierror /= 0_C_INT) then
    call shape_tuple%destroy
    return
  endif
  
  ierror = args%setitem(0_PY_SSIZE_T_KIND, shape_tuple)
  call shape_tuple%destroy
end function

function ndarray_create_special_helper_int32(array, creator_function, a_shape, kwargs) result(ierror)
  type(ndarray), intent(out) :: array
  character(kind=C_CHAR, len=*), intent(in) :: creator_function
  integer(kind=int32), dimension(:), intent(in) :: a_shape
  type(dict), intent(in) :: kwargs
  integer(kind=C_INT) :: ierror
  
  type(tuple) :: args
  type(object) :: retval
  type(module_py) :: numpy_mod
  
  ierror = ndarray_create_special_getargs_int32(args, a_shape)
  if (ierror /= 0_C_INT) return

  numpy_mod%py_object = global_numpy_mod 
  ierror = call_py(retval, numpy_mod, creator_function, args, kwargs)
  
  if (ierror == 0_C_INT) then  
    ierror = cast(array, retval)
    call retval%destroy
  endif
  
  call args%destroy

end function

!> Helper function: create tuple from integer array
function tuple_from_array_int32(tu, arr) result(ierror)
  type(tuple), intent(out) :: tu
  integer(kind=int32), dimension(:), intent(in) :: arr
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: ii, ndim
  ndim = size(arr, kind=PY_SSIZE_T_KIND)
  
  ierror = tuple_create(tu, ndim)
  if (ierror /= 0_C_INT) return
  
  do ii = 1, ndim
    ierror = tu%setitem(ii-1, arr(ii))
    if (ierror /= 0_C_INT) then
      call tu%destroy
      return
    endif
  enddo
end function

function ndarray_create_special_impl_int64(array, creator_function, a_shape, dtype, order) result(ierror)
  type(ndarray), intent(out) :: array
  character(kind=C_CHAR, len=*), intent(in) :: creator_function
  integer(kind=int64), dimension(:), intent(in) :: a_shape
  character(kind=C_CHAR, len=*), intent(in) :: dtype
  character(kind=C_CHAR), intent(in) :: order
  integer(kind=C_INT) :: ierror
  
  type(dict) :: kwargs
  
  ierror = dict_create(kwargs)
  if (ierror /= 0_C_INT) return
  
  if (len(dtype) > 0) then
    ierror = kwargs%setitem("dtype", dtype)
    if (ierror /= 0_C_INT) then
      call kwargs%destroy
      return
    endif
  endif
  
  ierror = kwargs%setitem("order", order)
 
  if (ierror == 0_C_INT) then
    ierror = ndarray_create_special_helper_int64(array, creator_function, a_shape, kwargs)
  endif
  
  call kwargs%destroy
end function

function ndarray_create_special_getargs_int64(args, a_shape) result(ierror)
  type(tuple), intent(out) :: args
  integer(kind=int64), dimension(:), intent(in) :: a_shape
  integer(kind=C_INT) :: ierror
  
  type(tuple) :: shape_tuple
  
  ierror = tuple_from_array_int64(shape_tuple, a_shape)
  if (ierror /= 0_C_INT) return
  
  ierror = tuple_create(args, 1_PY_SSIZE_T_KIND)
  if (ierror /= 0_C_INT) then
    call shape_tuple%destroy
    return
  endif
  
  ierror = args%setitem(0_PY_SSIZE_T_KIND, shape_tuple)
  call shape_tuple%destroy
end function

function ndarray_create_special_helper_int64(array, creator_function, a_shape, kwargs) result(ierror)
  type(ndarray), intent(out) :: array
  character(kind=C_CHAR, len=*), intent(in) :: creator_function
  integer(kind=int64), dimension(:), intent(in) :: a_shape
  type(dict), intent(in) :: kwargs
  integer(kind=C_INT) :: ierror
  
  type(tuple) :: args
  type(object) :: retval
  type(module_py) :: numpy_mod
  
  ierror = ndarray_create_special_getargs_int64(args, a_shape)
  if (ierror /= 0_C_INT) return

  numpy_mod%py_object = global_numpy_mod 
  ierror = call_py(retval, numpy_mod, creator_function, args, kwargs)
  
  if (ierror == 0_C_INT) then  
    ierror = cast(array, retval)
    call retval%destroy
  endif
  
  call args%destroy

end function

!> Helper function: create tuple from integer array
function tuple_from_array_int64(tu, arr) result(ierror)
  type(tuple), intent(out) :: tu
  integer(kind=int64), dimension(:), intent(in) :: arr
  integer(kind=C_INT) :: ierror
  
  integer(kind=PY_SSIZE_T_KIND) :: ii, ndim
  ndim = size(arr, kind=PY_SSIZE_T_KIND)
  
  ierror = tuple_create(tu, ndim)
  if (ierror /= 0_C_INT) return
  
  do ii = 1, ndim
    ierror = tu%setitem(ii-1, arr(ii))
    if (ierror /= 0_C_INT) then
      call tu%destroy
      return
    endif
  enddo
end function


!------------------ Routines for wrapping values into Python objects ("boxing") ----------------------
#ifndef PYTHON2
function box_value_int32_as_long(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  integer(kind=int32), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  integer(kind=C_LONG_LONG) :: tmp
  ierror = 0_C_INT

  tmp = int(the_value, C_LONG_LONG)

  obj = PyLong_FromLongLong(tmp)

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function
#endif

function box_value_int64_as_long(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  integer(kind=int64), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  integer(kind=C_LONG_LONG) :: tmp
  ierror = 0_C_INT

  tmp = int(the_value, C_LONG_LONG)

  obj = PyLong_FromLongLong(tmp)

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function

#ifdef PYTHON2
function box_value_int32(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  integer(kind=int32), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  integer(kind=C_LONG) :: tmp
  ierror = 0_C_INT

  tmp = int(the_value, C_LONG)

  obj = PyInt_FromLong(tmp)

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function

function box_value_int64(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  integer(kind=int64), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  integer(kind=C_LONG) :: tmp
  ierror = 0_C_INT

  ! this is the case for Windows and 32-bit Linux
  if (huge(the_value) > huge(tmp)) then
    if (the_value > huge(tmp) .or. the_value < -huge(tmp)) then
      if (the_value /= (-huge(tmp) - 1)) then
        !overflow: must use 'long'-type
        ierror = box_value_int64_as_long(obj, the_value)
        return
      endif
    endif  
  endif

  tmp = int(the_value, C_LONG)

  obj = PyInt_FromLong(tmp)

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function
#endif

function box_value_real32(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  real(kind=real32), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  real(kind=C_DOUBLE) :: tmp

  ierror = 0_C_INT

  tmp = real(the_value, C_DOUBLE)

  obj = PyFloat_FromDouble(tmp)

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function

function box_value_real64(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  real(kind=real64), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  real(kind=C_DOUBLE) :: tmp

  ierror = 0_C_INT

  tmp = real(the_value, C_DOUBLE)

  obj = PyFloat_FromDouble(tmp)

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function

function box_value_complex_real32(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  complex(kind=real32), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  ierror = 0_C_INT

  obj = PyComplex_FromDoubles(real(the_value, C_DOUBLE), real(aimag(the_value), C_DOUBLE))

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function
function box_value_complex_real64(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  complex(kind=real64), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  ierror = 0_C_INT

  obj = PyComplex_FromDoubles(real(the_value, C_DOUBLE), real(aimag(the_value), C_DOUBLE))

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function

function box_value_chars(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  character(kind=C_CHAR, len=*), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

#ifdef PYTHON2
  ierror = box_value_chars_as_bytestr(obj, the_value)
#else
  ierror = box_value_chars_as_unicodestr(obj, the_value)
#endif

end function

function box_value_chars_as_bytestr(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  character(kind=C_CHAR, len=*), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  integer(kind=PY_SSIZE_T_KIND) :: length

  ierror = 0_C_INT

  length = len(the_value)

  obj = PyBytes_FromStringAndSize(the_value, length)

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function

function box_value_chars_as_unicodestr(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  character(kind=C_CHAR, len=*), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  integer(kind=PY_SSIZE_T_KIND) :: length

  ierror = 0_C_INT

  length = len(the_value)

  obj = PyUnicode_DecodeUTF8(the_value, length, "strict" // C_NULL_CHAR)

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function

function box_value_char_1d(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  character(kind=C_CHAR), dimension(:), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

#ifdef PYTHON2
  ierror = box_value_char_1d_as_bytestr(obj, the_value)
#else
  ierror = box_value_char_1d_as_unicodestr(obj, the_value)
#endif

end function

function box_value_char_1d_as_bytestr(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  character(kind=C_CHAR), dimension(:), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  integer(kind=PY_SSIZE_T_KIND) :: length

  ierror = 0_C_INT

  length = size(the_value)

  obj = PyBytes_FromStringAndSize(the_value, length)

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function

function box_value_char_1d_as_unicodestr(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  character(kind=C_CHAR), dimension(:), intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  integer(kind=PY_SSIZE_T_KIND) :: length

  ierror = 0_C_INT

  length = size(the_value)

  obj = PyUnicode_DecodeUTF8(the_value, length, "strict" // C_NULL_CHAR)

  if (.not. c_associated(obj)) then
    ierror = EXCEPTION_ERROR
  endif

end function


function box_value_logical(obj, the_value) result(ierror)
  type(c_ptr), intent(out) :: obj
  logical, intent(in) :: the_value
  integer(kind=C_INT) :: ierror

  ierror = 0_C_INT

  if (the_value) then
    obj = global_Py_TrueStruct_ptr
    call Py_IncRef(obj)
  else
    obj = global_Py_FalseStruct_ptr
    call Py_IncRef(obj)
  endif

end function

!------------- Routines for unboxing -----------------------------------
function unbox_value_int32(the_value, obj) result(ierror)
  integer(kind=int32), intent(out) :: the_value
  type(c_ptr), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  integer(kind=C_LONG_LONG) :: tmp
  integer :: overflow
  type(c_ptr) :: err_obj
  
  ierror = 0_C_INT
  tmp = PyLong_AsLongLongAndOverflow(obj, overflow)
  the_value = int(tmp, int32)
  
  if (tmp == -1_C_LONG_LONG) then
    if (overflow == 0) then
      err_obj = PyErr_Occurred()
      if (c_associated(err_obj)) then
        ierror = EXCEPTION_ERROR
        return
      endif 
    else
      ierror = EXCEPTION_ERROR
      call raise_exception(OverflowError, "int too big to convert")
      return
    endif
  endif
  
  if (huge(the_value) < huge(tmp)) then
    if (tmp > huge(the_value) .or. tmp < -huge(the_value)) then
      if (tmp /= (-huge(the_value) - 1)) then
        ierror = EXCEPTION_ERROR
        call raise_exception(OverflowError, "int too large for Fortran integer(kind=int32)")
      endif
    endif
  endif

end function

function unbox_value_int64(the_value, obj) result(ierror)
  integer(kind=int64), intent(out) :: the_value
  type(c_ptr), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  integer(kind=C_LONG_LONG) :: tmp
  integer :: overflow
  type(c_ptr) :: err_obj
  
  ierror = 0_C_INT
  tmp = PyLong_AsLongLongAndOverflow(obj, overflow)
  the_value = int(tmp, int64)
  
  if (tmp == -1_C_LONG_LONG) then
    if (overflow == 0) then
      err_obj = PyErr_Occurred()
      if (c_associated(err_obj)) then
        ierror = EXCEPTION_ERROR
        return
      endif 
    else
      ierror = EXCEPTION_ERROR
      call raise_exception(OverflowError, "int too big to convert")
      return
    endif
  endif
  
  if (huge(the_value) < huge(tmp)) then
    if (tmp > huge(the_value) .or. tmp < -huge(the_value)) then
      if (tmp /= (-huge(the_value) - 1)) then
        ierror = EXCEPTION_ERROR
        call raise_exception(OverflowError, "int too large for Fortran integer(kind=int64)")
      endif
    endif
  endif

end function


function unbox_value_real32(the_value, obj) result(ierror)
  real(kind=real32), intent(out) :: the_value
  type(c_ptr), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  real(kind=C_DOUBLE) :: tmp
  type(c_ptr) :: err_obj
  
  ierror = 0_C_INT
  tmp = PyFloat_AsDouble(obj)
  the_value = real(tmp, real32)
  
  if (tmp == -1.0_C_DOUBLE) then
    err_obj = PyErr_Occurred()
    if (c_associated(err_obj)) then
      ierror = EXCEPTION_ERROR
      return
    endif 
  endif

end function

function unbox_value_real64(the_value, obj) result(ierror)
  real(kind=real64), intent(out) :: the_value
  type(c_ptr), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  real(kind=C_DOUBLE) :: tmp
  type(c_ptr) :: err_obj
  
  ierror = 0_C_INT
  tmp = PyFloat_AsDouble(obj)
  the_value = real(tmp, real64)
  
  if (tmp == -1.0_C_DOUBLE) then
    err_obj = PyErr_Occurred()
    if (c_associated(err_obj)) then
      ierror = EXCEPTION_ERROR
      return
    endif 
  endif

end function


function unbox_value_complex_real32(the_value, obj) result(ierror)
  complex(kind=real32), intent(out) :: the_value
  type(c_ptr), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  type(Py_complex) :: tmp
  type(c_ptr) :: err_obj
  
  ierror = 0_C_INT
  tmp = PyComplex_AsCComplex(obj) !this handles objects with __complex__ method correctly
  
  the_value = cmplx(tmp%real_part, tmp%imag_part, kind=real32)
  
  if (tmp%real_part == -1.0_C_DOUBLE) then
    err_obj = PyErr_Occurred()
    if (c_associated(err_obj)) then
      ierror = EXCEPTION_ERROR
      return
    endif 
  endif

end function

function unbox_value_complex_real64(the_value, obj) result(ierror)
  complex(kind=real64), intent(out) :: the_value
  type(c_ptr), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  type(Py_complex) :: tmp
  type(c_ptr) :: err_obj
  
  ierror = 0_C_INT
  tmp = PyComplex_AsCComplex(obj) !this handles objects with __complex__ method correctly
  
  the_value = cmplx(tmp%real_part, tmp%imag_part, kind=real64)
  
  if (tmp%real_part == -1.0_C_DOUBLE) then
    err_obj = PyErr_Occurred()
    if (c_associated(err_obj)) then
      ierror = EXCEPTION_ERROR
      return
    endif 
  endif

end function


function unbox_value_logical(the_value, obj) result(ierror)
  logical, intent(out) :: the_value
  type(c_ptr), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  the_value = .false.
  ierror = PyObject_IsTrue(obj)
  if (ierror /= -1_C_INT) then
    the_value = (ierror == 1_C_INT)
    ierror = 0_C_INT
  endif
end function

! unboxes WITHOUT making a copy - returns a pointer
! do not change string via pointer, since they are supposed to be
! immutable in Python
function unbox_value_char_1d(the_value, obj) result(ierror)
  character(kind=C_CHAR), pointer, dimension(:), intent(out) :: the_value
  type(c_ptr), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: char_ptr
  integer(PY_SSIZE_T_KIND) :: length(1)
  
  logical :: obj_is_bytes
  type(object) :: dummy_obj ! just to be able to use is_bytes and is_unicode

  ierror = 0_C_INT
  dummy_obj%py_object = obj
      
  obj_is_bytes = is_bytes(dummy_obj)

  if (obj_is_bytes) then
    char_ptr = PyBytes_AsString(obj)  
  elseif (is_unicode(dummy_obj)) then
#ifndef PYTHON2
    !C-API-function not available in PY2
    char_ptr = PyUnicode_AsUTF8AndSize(obj, length(1)) 
#else
    char_ptr = C_NULL_PTR
    call raise_exception(TypeError, "forpy: cast of unicode object to character array not supported when using Python 2")
#endif
  else
    char_ptr = C_NULL_PTR
  endif
  
  if (.not. c_associated(char_ptr)) then
    ierror = EXCEPTION_ERROR
    if (.not. have_exception()) then
      call raise_exception(TypeError, "forpy: Cannot cast to character array")
    endif
    return
  endif
  
  if (obj_is_bytes) then
    length(1) = PyObject_Length(obj)
  endif
  
  if (length(1) == -1_PY_SSIZE_T_KIND) then
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Cannot cast to character array")
    return
  endif
  
  ! length 0 strings also seem to work
  
  call c_f_pointer(char_ptr, the_value, length)

end function

! unboxes making a copy
function unbox_value_chars(the_value, obj) result(ierror)
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: the_value
  type(c_ptr), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  character(kind=C_CHAR), dimension(:), pointer :: char_ptr
  
  ierror = unbox_value_char_1d(char_ptr, obj)
  
  if (ierror == 0_C_INT) then
    call char_1d_to_chars(char_ptr, the_value)
  endif
  
end function

subroutine char_1d_to_chars(inp, outp)
  character(kind=C_CHAR), dimension(:), intent(in) :: inp
  character(kind=C_CHAR, len=:), allocatable, intent(inout) :: outp
  
  integer :: length, ii
  
  length = size(inp)
  
  ! Check allocation fails?
  if (allocated(outp)) then
    deallocate(outp)
  endif
  
  allocate(character(kind=C_CHAR, len=length) :: outp)
  
  do ii = 1, length
    outp(ii:ii) = inp(ii)
  enddo
  
end subroutine

!------------- Routines for (safely) casting types ---------------------
! Note: They do not transfer ownership to casted object - both input and
! output object have to be destroyed

function cast_to_list(li, obj) result(ierror)
  type(list), intent(out) :: li
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_list(obj)) then
    ierror = 0_C_INT
    li%py_object = obj%py_object
    call Py_IncRef(obj%py_object)
  else
    li%py_object = C_NULL_PTR
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to list.")
  endif
end function

function cast_nonstrict_to_list(li, obj) result(ierror)
  type(list), intent(out) :: li
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_list(obj)) then
    ierror = 0_C_INT
    li%py_object = obj%py_object
    call Py_IncRef(obj%py_object)
  else
    ierror = list_create(li, obj)
  endif
end function

function cast_to_dict(di, obj) result(ierror)
  type(dict), intent(out) :: di
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_dict(obj)) then
    ierror = 0_C_INT
    di%py_object = obj%py_object
    call Py_IncRef(obj%py_object)
  else
    di%py_object = C_NULL_PTR
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to dict.")
  endif
end function

function cast_to_tuple(tu, obj) result(ierror)
  type(tuple), intent(out) :: tu
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_tuple(obj)) then
    ierror = 0_C_INT
    tu%py_object = obj%py_object
    call Py_IncRef(obj%py_object)
  else
    tu%py_object = C_NULL_PTR
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to tuple.")
  endif
end function

function cast_nonstrict_to_tuple(tu, obj) result(ierror)
  type(tuple), intent(out) :: tu
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_tuple(obj)) then
    ierror = 0_C_INT
    tu%py_object = obj%py_object
    call Py_IncRef(obj%py_object)
  else
    ierror = tuple_create(tu, obj)
  endif
end function

function cast_to_NoneType(no, obj) result(ierror)
  type(NoneType), intent(out) :: no
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_none(obj)) then
    ierror = 0_C_INT
    no%py_object = obj%py_object
    call Py_IncRef(obj%py_object)
  else
    no%py_object = C_NULL_PTR
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to NoneType.")
  endif
end function

function cast_to_ndarray(nd, obj) result(ierror)
  type(ndarray), intent(out) :: nd
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_ndarray(obj)) then
    ierror = 0_C_INT
    nd%py_object = obj%py_object
    call Py_IncRef(obj%py_object)
  else
    nd%py_object = C_NULL_PTR
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to ndarray.")
  endif
end function

function cast_to_object(plain_obj, obj) result(ierror)
  type(object), intent(out) :: plain_obj
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  ierror = 0_C_INT
  plain_obj%py_object = obj%py_object
  call Py_IncRef(obj%py_object)
end function

! casts to scalar fortran types

function cast_to_int32(out_value, obj) result(ierror)
  integer(kind=int32), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_int(obj)) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to integer(kind=int32).")
  endif
end function

function cast_to_int32_flex(out_value, obj, strict) result(ierror)
  integer(kind=int32), intent(out) :: out_value
  class(object), intent(in) :: obj
  logical, intent(in) :: strict
  integer(kind=C_INT) :: ierror

  if (.not. strict) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = cast_to_int32(out_value, obj)
  endif
end function

function cast_nonstrict_to_int32(out_value, obj) result(ierror)
  integer(kind=int32), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  ierror = cast(out_value, obj, .false.)
end function

function cast_to_int64(out_value, obj) result(ierror)
  integer(kind=int64), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_int(obj)) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to integer(kind=int64).")
  endif
end function

function cast_to_int64_flex(out_value, obj, strict) result(ierror)
  integer(kind=int64), intent(out) :: out_value
  class(object), intent(in) :: obj
  logical, intent(in) :: strict
  integer(kind=C_INT) :: ierror

  if (.not. strict) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = cast_to_int64(out_value, obj)
  endif
end function

function cast_nonstrict_to_int64(out_value, obj) result(ierror)
  integer(kind=int64), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  ierror = cast(out_value, obj, .false.)
end function

function cast_to_real32(out_value, obj) result(ierror)
  real(kind=real32), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_float(obj)) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to real(kind=real32).")
  endif
end function

function cast_to_real32_flex(out_value, obj, strict) result(ierror)
  real(kind=real32), intent(out) :: out_value
  class(object), intent(in) :: obj
  logical, intent(in) :: strict
  integer(kind=C_INT) :: ierror

  if (.not. strict) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = cast_to_real32(out_value, obj)
  endif
end function

function cast_nonstrict_to_real32(out_value, obj) result(ierror)
  real(kind=real32), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  ierror = cast(out_value, obj, .false.)
end function

function cast_to_real64(out_value, obj) result(ierror)
  real(kind=real64), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_float(obj)) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to real(kind=real64).")
  endif
end function

function cast_to_real64_flex(out_value, obj, strict) result(ierror)
  real(kind=real64), intent(out) :: out_value
  class(object), intent(in) :: obj
  logical, intent(in) :: strict
  integer(kind=C_INT) :: ierror

  if (.not. strict) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = cast_to_real64(out_value, obj)
  endif
end function

function cast_nonstrict_to_real64(out_value, obj) result(ierror)
  real(kind=real64), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  ierror = cast(out_value, obj, .false.)
end function

function cast_to_complex_real32(out_value, obj) result(ierror)
  complex(kind=real32), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_complex(obj)) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to complex(kind=real32).")
  endif
end function

function cast_to_complex_real32_flex(out_value, obj, strict) result(ierror)
  complex(kind=real32), intent(out) :: out_value
  class(object), intent(in) :: obj
  logical, intent(in) :: strict
  integer(kind=C_INT) :: ierror

  if (.not. strict) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = cast_to_complex_real32(out_value, obj)
  endif
end function

function cast_nonstrict_to_complex_real32(out_value, obj) result(ierror)
  complex(kind=real32), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  ierror = cast(out_value, obj, .false.)
end function

function cast_to_complex_real64(out_value, obj) result(ierror)
  complex(kind=real64), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_complex(obj)) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to complex(kind=real64).")
  endif
end function

function cast_to_complex_real64_flex(out_value, obj, strict) result(ierror)
  complex(kind=real64), intent(out) :: out_value
  class(object), intent(in) :: obj
  logical, intent(in) :: strict
  integer(kind=C_INT) :: ierror

  if (.not. strict) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = cast_to_complex_real64(out_value, obj)
  endif
end function

function cast_nonstrict_to_complex_real64(out_value, obj) result(ierror)
  complex(kind=real64), intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  ierror = cast(out_value, obj, .false.)
end function

function cast_to_logical(out_value, obj) result(ierror)
  logical, intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_bool(obj)) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to logical.")
  endif
end function

function cast_to_logical_flex(out_value, obj, strict) result(ierror)
  logical, intent(out) :: out_value
  class(object), intent(in) :: obj
  logical, intent(in) :: strict
  integer(kind=C_INT) :: ierror

  if (.not. strict) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = cast_to_logical(out_value, obj)
  endif
end function

function cast_nonstrict_to_logical(out_value, obj) result(ierror)
  logical, intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  ierror = cast(out_value, obj, .false.)
end function


! casts to strings
function cast_to_chars(out_value, obj) result(ierror)
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  if (is_str(obj) .or. is_bytes(obj) .or. is_unicode(obj)) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to character(kind=C_CHAR, len=:).")
  endif
end function

function cast_to_char_1d(out_value, obj) result(ierror)
  character(kind=C_CHAR), dimension(:), pointer :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror

  if (is_str(obj) .or. is_bytes(obj) .or. is_unicode(obj)) then
    ierror = unbox_value(out_value, obj%py_object)
  else
    ierror = EXCEPTION_ERROR
    call raise_exception(TypeError, "forpy: Could not cast to character(kind=C_CHAR), dimension(:), pointer.")
  endif
end function

function cast_nonstrict_to_chars(out_value, obj) result(ierror)
  character(kind=C_CHAR, len=:), allocatable, intent(out) :: out_value
  class(object), intent(in) :: obj
  integer(kind=C_INT) :: ierror
  
  type(c_ptr) :: str_obj
  
  if (is_str(obj) .or. is_bytes(obj) .or. is_unicode(obj)) then
    ierror = unbox_value(out_value, obj%py_object)
    return
  endif
  
  str_obj = PyObject_Str(obj%py_object)
  
  if (.not. c_associated(str_obj)) then
    ierror = EXCEPTION_ERROR
    return
  endif
  
  ierror = unbox_value(out_value, str_obj)
  call Py_DecRef(str_obj)
end function

! casting scalar Fortran types into Python objects

function cast_from_int32(obj, in_value) result(ierror)
  type(object), intent(out) :: obj
  integer(kind=int32), intent(in) :: in_value
  integer(kind=C_INT) :: ierror
  
  ierror = box_value(obj%py_object, in_value)
end function

function cast_from_int64(obj, in_value) result(ierror)
  type(object), intent(out) :: obj
  integer(kind=int64), intent(in) :: in_value
  integer(kind=C_INT) :: ierror
  
  ierror = box_value(obj%py_object, in_value)
end function

function cast_from_real32(obj, in_value) result(ierror)
  type(object), intent(out) :: obj
  real(kind=real32), intent(in) :: in_value
  integer(kind=C_INT) :: ierror
  
  ierror = box_value(obj%py_object, in_value)
end function

function cast_from_real64(obj, in_value) result(ierror)
  type(object), intent(out) :: obj
  real(kind=real64), intent(in) :: in_value
  integer(kind=C_INT) :: ierror
  
  ierror = box_value(obj%py_object, in_value)
end function

function cast_from_complex_real32(obj, in_value) result(ierror)
  type(object), intent(out) :: obj
  complex(kind=real32), intent(in) :: in_value
  integer(kind=C_INT) :: ierror
  
  ierror = box_value(obj%py_object, in_value)
end function

function cast_from_complex_real64(obj, in_value) result(ierror)
  type(object), intent(out) :: obj
  complex(kind=real64), intent(in) :: in_value
  integer(kind=C_INT) :: ierror
  
  ierror = box_value(obj%py_object, in_value)
end function

function cast_from_logical(obj, in_value) result(ierror)
  type(object), intent(out) :: obj
  logical, intent(in) :: in_value
  integer(kind=C_INT) :: ierror
  
  ierror = box_value(obj%py_object, in_value)
end function


!===============================================================================
! Assignment with Python semantics
!===============================================================================

! Usage: call assign_py(lhs, rhs)

!> Equivalent to the following Python code: lhs = rhs
!>
!> do not forget to destroy lhs after usage
subroutine assign_py_object(lhs, rhs)
  type(object), intent(out) :: lhs
  class(object), intent(in) :: rhs
  
  lhs%py_object = rhs%py_object
  call Py_IncRef(lhs%py_object)
end subroutine

!> Equivalent to the following Python code: lhs = rhs
!>
!> do not forget to destroy lhs after usage
subroutine assign_py_list(lhs, rhs)
  type(list), intent(out) :: lhs
  class(list), intent(in) :: rhs
  
  lhs%py_object = rhs%py_object
  call Py_IncRef(lhs%py_object)
end subroutine

!> Equivalent to the following Python code: lhs = rhs
!>
!> do not forget to destroy lhs after usage
subroutine assign_py_tuple(lhs, rhs)
  type(tuple), intent(out) :: lhs
  class(tuple), intent(in) :: rhs
  
  lhs%py_object = rhs%py_object
  call Py_IncRef(lhs%py_object)
end subroutine

!> Equivalent to the following Python code: lhs = rhs
!>
!> do not forget to destroy lhs after usage
subroutine assign_py_dict(lhs, rhs)
  type(dict), intent(out) :: lhs
  class(dict), intent(in) :: rhs
  
  lhs%py_object = rhs%py_object
  call Py_IncRef(lhs%py_object)
end subroutine

!> Equivalent to the following Python code: lhs = rhs
!>
!> do not forget to destroy lhs after usage
subroutine assign_py_ndarray(lhs, rhs)
  type(ndarray), intent(out) :: lhs
  class(ndarray), intent(in) :: rhs
  
  lhs%py_object = rhs%py_object
  call Py_IncRef(lhs%py_object)
end subroutine

!> Equivalent to the following Python code: lhs = rhs
!>
!> do not forget to destroy lhs after usage
subroutine assign_py_type_py(lhs, rhs)
  type(type_py), intent(out) :: lhs
  class(type_py), intent(in) :: rhs
  
  lhs%py_object = rhs%py_object
  call Py_IncRef(lhs%py_object)
end subroutine

!> Equivalent to the following Python code: lhs = rhs
!>
!> do not forget to destroy lhs after usage
subroutine assign_py_module_py(lhs, rhs)
  type(module_py), intent(out) :: lhs
  class(module_py), intent(in) :: rhs
  
  lhs%py_object = rhs%py_object
  call Py_IncRef(lhs%py_object)
end subroutine

!> Equivalent to the following Python code: lhs = rhs
!>
!> do not forget to destroy lhs after usage
subroutine assign_py_NoneType(lhs, rhs)
  type(NoneType), intent(out) :: lhs
  class(NoneType), intent(in) :: rhs
  
  lhs%py_object = rhs%py_object
  call Py_IncRef(lhs%py_object)
end subroutine


!===============================================================================
! Exception handling
!===============================================================================

!> Checks if a certain type of exception has occurred.
!>
!> Example: flag = exception_matches(KeyError)
!> Returns .false. if no exception has occurred.
function exception_matches(exc) result(is_match)
  class(object), intent(in) :: exc
  logical :: is_match
  
  type(c_ptr) :: err_obj
  
  err_obj = PyErr_Occurred()
  
  if (c_associated(err_obj) .and. c_associated(exc%py_object)) then
    is_match = (PyErr_GivenExceptionMatches(err_obj, exc%py_object) == 1)
    return
  endif
  
  is_match = .false.
end function

!> Clears an exception.
!>
!> No effect if no exception happened. Must be called 
!> after handling an exception, otherwise
!> future forpy/Python operations can fail in strange ways.
subroutine err_clear()
  call PyErr_Clear()
end subroutine

!> Prints and clears exception. If no exception has occurred, does nothing.
subroutine err_print()
  type(c_ptr) :: err_obj
  err_obj = PyErr_Occurred()
  ! check if there really is an error, otherwise call to PyErr_Print is fatal
  if (c_associated(err_obj)) then
    call PyErr_Print()
  endif
end subroutine

!> returns .true. if an exception has occurred
function have_exception()
  logical :: have_exception
  
  type(c_ptr) :: err_obj
  err_obj = PyErr_Occurred()
  have_exception = c_associated(err_obj)
end function

!> raises an exception
subroutine raise_exception(exc_type, message)
  !> The exception to raise.
  !>
  !> Example: call raise_exception(ValueError, "bad value")
  class(object), intent(in) :: exc_type
  character(kind=C_CHAR, len=*), intent(in) :: message
  
  call PyErr_SetString(exc_type%py_object, message // C_NULL_CHAR)
end subroutine

!=======================================================================
! Python extension development
!=======================================================================

function PythonModule_init(self, module_name, doc_string, method_table) result(module_ptr)
  class(PythonModule), intent(inout) :: self
  !> Name of the Python extension module.
  character(kind=C_CHAR, len=*), intent(in) :: module_name
  !> Doc string for the Python extension module.
  character(kind=C_CHAR, len=*), intent(in) :: doc_string
  !> Table of methods of the Python extension module.
  type(PythonMethodTable), intent(in) :: method_table
  type(c_ptr) :: module_ptr

  integer(kind=C_INT), parameter :: PYTHON_API_VERSION = 1013_C_INT !api-version is 1013 since 2006 and still is in 2018

  module_ptr = C_NULL_PTR
  self%module_ptr = C_NULL_PTR
  
  allocate(self%module_def) ! never deallocated, for reasons given in PythonMethodTable_init
  
  self%module_def = PyModuleDef(PyModuleDef_Base(1_PY_SSIZE_T_KIND, C_NULL_PTR, C_NULL_PTR, &
                                                 0_PY_SSIZE_T_KIND, C_NULL_PTR), &
                               C_NULL_PTR, C_NULL_PTR, -1_PY_SSIZE_T_KIND, C_NULL_PTR, C_NULL_PTR, &
                               C_NULL_FUNPTR, C_NULL_FUNPTR, C_NULL_FUNPTR)

  allocate(character(kind=C_CHAR,len=len(module_name)+1) :: self%module_name)
  self%module_name = module_name // C_NULL_CHAR
  allocate(character(kind=C_CHAR,len=len(doc_string)+1) :: self%doc_string)
  self%doc_string = doc_string // C_NULL_CHAR
                               
  self%module_def%m_methods = method_table%get_method_table()
  self%module_def%m_name = c_loc(self%module_name)
  self%module_def%m_doc = c_loc(self%doc_string)
  
#ifndef PYTHON2
  module_ptr = PyModule_Create2(c_loc(self%module_def), PYTHON_API_VERSION)
#else
  module_ptr = Py_InitModule4(self%module_name, self%module_def%m_methods, self%doc_string, C_NULL_PTR, PYTHON_API_VERSION)
#endif

  self%module_ptr = module_ptr
end function

!> add an object as a member to a module
function PythonModule_add_object(self, object_name, obj) result(ierror)
  class(PythonModule), intent(inout) :: self
  !> Name of the module member. It can be accessed by module_name.object_name in Python.
  character(kind=C_CHAR, len=*), intent(in) :: object_name
  !> The object to add as a member to the module.
  class(object), intent(in) :: obj
  !> Error code, 0 on success.
  integer(kind=C_INT) :: ierror
  
  call Py_IncRef(obj%py_object)  !PyModule_AddObject steals a reference
  ierror = PyModule_AddObject(self%module_ptr, object_name // C_NULL_CHAR, obj%py_object)
end function

subroutine PythonMethodTable_init(self, num_methods)
  class(PythonMethodTable), intent(inout) :: self
  !> The number of methods your Python module shall have.
  integer, intent(in) :: num_methods
  
  integer :: ii
  self%num_methods = num_methods
  self%method_count = 0
  ! These are never deallocated
  ! not a big problem, since "de-importing" modules in Python is unusual and
  ! sometimes impossible. There exist kind of a finish method, so this could be a TODO
  allocate(self%methods(num_methods+1)) !need extra space for sentinel entry
  allocate(self%strings(num_methods))
  
  do ii = 1, num_methods + 1
    ! at the end of methods array there has to be this sentinel value
    ! just to be safe, initialise complete method array with it
    self%methods(ii) = PyMethodDef(C_NULL_PTR, C_NULL_FUNPTR, 0_C_INT, C_NULL_PTR)
  enddo
end subroutine

subroutine PythonMethodTable_add_method(self, method_name, doc_string, flags, method_funptr)
  class(PythonMethodTable), intent(inout) :: self
  !> Name of the Python method.
  character(kind=C_CHAR, len=*), intent(in) :: method_name
  !> Doc string for the Python method
  character(kind=C_CHAR, len=*), intent(in) :: doc_string
  !> Controls which kind of arguments the Python method shall take.
  !>
  !> use flags=METH_VARARGS if method shall take only arguments.
  !> use flags=METH_KWARGS if method shall take only keyword args.
  !> use flags=METH_VARARGS+METH_KWARGS if method shall take both arguments and keyword args.
  !> use flags=METH_NOARGS if method shall take no arguments.
  integer(kind=C_INT), intent(in) :: flags
  !> Function pointer to the Fortran implementation of the method.
  !>
  !> Use C_FUNLOC(<Fortran function>) to get method_funptr
  !> The Fortran function must take "TYPE(c_ptr), VALUE" arguments (number depends on what arguments it takes)
  !> and have a type(c_ptr) return value.
  !> E. g. if flags=METH_VARARGS+METH_KWARGS, one needs 3 arguments
  !> (1st arg: module c_ptr, 2nd: arguments c_ptr, 3rd: kwargs c_ptr)
  !> use [[unsafe_cast_from_c_ptr]] to cast 2nd argument to a tuple
  !> and 3rd argument to a dict.
  !> The function must return a Python object type(c_ptr).
  !> Use object%get_c_ptr() to retrieve the c_ptr from a forpy object.
  type(c_funptr), intent(in) :: method_funptr
  
  integer :: ind
  type(c_ptr) :: method_name_loc, doc_string_loc
  
  if (self%method_count >= self%num_methods) then
    call raise_exception(ImportError, "forpy: Could not add method. Increase num_methods in PythonMethodTable%init")
    return
  endif
  
  ind = self%method_count + 1
  
  allocate(character(kind=C_CHAR,len=len(method_name)+1) :: self%strings(ind)%method_name)
  self%strings(ind)%method_name = method_name // C_NULL_CHAR
  
  allocate(character(kind=C_CHAR,len=len(doc_string)+1) :: self%strings(ind)%doc_string)
  self%strings(ind)%doc_string = doc_string // C_NULL_CHAR
  
  method_name_loc = c_loc(self%strings(ind)%method_name)
  doc_string_loc = c_loc(self%strings(ind)%doc_string)
  
  self%methods(ind) = PyMethodDef(method_name_loc, method_funptr, flags, doc_string_loc)
  self%method_count = self%method_count + 1
end subroutine

function PythonMethodTable_get(self) result(m)
  class(PythonMethodTable), intent(in) :: self
  type(c_ptr) :: m
  m = c_loc(self%methods)
end function

!> Creates an [[object]] from a type(c_ptr), no type checks
!>
!> use with care, use only for developing Python extensions
!> created object 'obj' has to be cleaned up by calling obj%destroy
subroutine unsafe_cast_from_c_ptr(obj, ptr)
  !> The created Python object.
  class(object), intent(out) :: obj
  !> C pointer to cast from
  type(c_ptr), intent(in) :: ptr
  
  call Py_IncRef(ptr)
  obj%py_object = ptr
end subroutine

!=======================================================================
! Tools
!=======================================================================

!> Python's print function.
function print_py(obj, kwargs) result(ierror)
  !> Object to print.
  class(object), intent(in) :: obj
  !> Optional dict of keyword arguments
  class(dict), intent(in), optional :: kwargs
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror

  type(c_ptr) :: builtin_dict, tmp
  type(object) :: print_fun
  type(tuple) :: args

  builtin_dict = PyEval_GetBuiltins() ! borrowed ref - do not decref
  if (.not. c_associated(builtin_dict)) then
    ierror = EXCEPTION_ERROR
    return
  endif
  
  tmp = PyDict_GetItemString(builtin_dict, "print" // C_NULL_CHAR) !borrowed ref
  print_fun%py_object = tmp

  ierror = tuple_create(args, 1)
  if (ierror /= 0_C_INT) return
  
  ierror = args%setitem(0, obj)
  if (ierror /= 0_C_INT) then
    call args%destroy
  endif
  
  if (present(kwargs)) then
    ierror = call_py_noret(print_fun, args, kwargs)
  else
    ierror = call_py_noret(print_fun, args)
  endif
  
  call args%destroy
  ! do not destroy print_fun: borrowed ref

end function

!> Getting the list of module search paths: sys.path
!>
!> Can be used to append additional directories, where modules shall be searched
function get_sys_path(paths) result(ierror)
  !> Output: list of module search paths
  type(list), intent(out) :: paths
  !> Error code, 0 on success  
  integer(kind=C_INT) :: ierror
  
  type(object) :: tmp
  
  tmp%py_object = PySys_GetObject("path" // C_NULL_CHAR)
  ierror = cast(paths, tmp)
  ! not destroying tmp, because PySys_GetObject returns borrowed reference
end function

!> Run Python code that is given as string.
function run_string(string) result(ierror)
  !> Python code to run.
  character(kind=C_CHAR, len=*), intent(in) :: string
  !> Error code, 0 on success
  integer(kind=C_INT) :: ierror
  
  integer :: length
  length = len(string)
  ! check if null-byte is at and of string, if not append it, which can
  ! be expensive
  if (string(length:length) == C_NULL_CHAR) then
    ierror = PyRun_SimpleString(string)
  else
    ierror = PyRun_SimpleString(string // C_NULL_CHAR)
  endif
end function

end module
