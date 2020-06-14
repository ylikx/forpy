# Forpy: A library for Fortran-Python interoperability.

Forpy allows you to use Python features in Fortran ("embedding Python in Fortran")

It provides datastructures such as list, dict, tuple and interoperability
of arrays using numpy.
With forpy you can even import Python modules in Fortran. Simply use your own or third-party Python modules
for tasks that you can easily do in Python. For example: plot with matplotlib or use scientific
functions from scipy or numpy.

Forpy also works to other way around: You can write Python modules entirely in Fortran (extending Python with Fortran - "Fortran in Python").

## Documentation

- This readme (start with that)
- [Wiki](https://github.com/ylikx/forpy/wiki)
- [API reference](https://ylikx.github.io/forpy/index.html)

## Contact

Elias Rabel (*ylikx.0 AT gmail.com*)

## Getting started

A simple example using a Python list:

```Fortran
program intro_to_forpy
  use forpy_mod
  implicit none

  integer :: ierror
  type(list) :: my_list

  ierror = forpy_initialize()
  ierror = list_create(my_list)

  ierror = my_list%append(19)
  ierror = my_list%append("Hello world!")
  ierror = my_list%append(3.14d0)
  ierror = print_py(my_list)

  call my_list%destroy
  call forpy_finalize

end program
```

Building the example:

To try the examples, copy the file [`forpy_mod.F90`](forpy_mod.F90) to your working directory.
Here I assume that you are using Python 3 (version >= 3.3) and 
gfortran (ifort also supported). 

If you are using *Anaconda* and have problems when building read
[Using forpy with Anaconda](#using-forpy-with-anaconda). 

If you are using *Windows*, read [Forpy on Windows](https://github.com/ylikx/forpy/wiki/Forpy-on-Windows).

For use with Python 2 read [Python 2 support](#python-2-support).
 
Save the example as `intro_to_forpy.F90` and type, depending on your Python version:

```bash
# Python 3.7 and earlier
gfortran -c forpy_mod.F90
gfortran intro_to_forpy.F90 forpy_mod.o `python3-config --ldflags`
```

```bash
# Python 3.8 and higher
gfortran -c forpy_mod.F90
gfortran intro_to_forpy.F90 forpy_mod.o `python3-config --ldflags --embed`
```

Then run the example with

```
./a.out
```

You should get the output:

```
[19, 'Hello world!', 3.14]
```

If `python3-config` is not found, you might have to install the package `python3-dev` (on Ubuntu, Debian). 

For simplicity this example and most following examples do not contain error handling code.

## Tuples, objects

This example introduces tuples and shows how to check for basic Python types.
It demonstrates the methods `getitem` and `setitem`, which also work
with `list`. These methods are generic for important Fortran types.


The type `object` can be used for any Python object. Use `cast` to transform an
`object` into a Fortran type or to transform into
a more specific Python object, such as `list` or `tuple`.

```Fortran
program tuple_example
  use forpy_mod
  implicit none

  integer :: ierror
  type(tuple) :: tu
  type(object) :: item
  integer :: int_value
  character(len=:), allocatable :: str_value
  integer :: ii
  integer :: tu_len

  ierror = forpy_initialize()
  
  ! Python: tu = (17, "hello", 23, "world")
  ierror = tuple_create(tu, 4) ! create tuple with 4 elements
  ! Must set all tuple elements before using tuple
  ierror = tu%setitem(0, 17)
  ierror = tu%setitem(1, "hello")
  ierror = tu%setitem(2, 23)
  ierror = tu%setitem(3, "world")
  
  ierror = tu%len(tu_len)
  
  do ii = 0, tu_len-1  ! Python indices start at 0
    ierror = tu%getitem(item, ii)
    
    ! Use is_int, is_str, is_float, is_none ...
    ! to check if an object is of a certain Python type 
    if (is_int(item)) then
      ! Use cast to transform 'item' into Fortran type 
      ierror = cast(int_value, item)
      write(*,*) int_value
    else if(is_str(item)) then
      ierror = cast(str_value, item)
      write(*,*) str_value 
    endif
    
    call item%destroy
  enddo

  call tu%destroy
  call forpy_finalize

end program
```

## Dictionaries, Error handling
The following example shows how to use a Python `dict` and shows some
error and exception handling.

```Fortran
program dict_example
  use forpy_mod
  implicit none

  integer :: ierror
  type(dict) :: di
  real :: a_value

  ierror = forpy_initialize()
  ierror = dict_create(di)  ! Python: di = {}

  ierror = di%setitem("temperature", 273.0)
  ierror = di%setitem("pressure", 1013.0)
  ierror = di%getitem(a_value, "pressure")
  write(*,*) "pressure = ", a_value
  
  ! Show some error handling
  ierror = di%getitem(a_value, "does not exist")
  if (ierror /= 0) then
    if (exception_matches(KeyError)) then
      write(*,*) "Key not found..."
      ! Must clear error after handling exception,
      ! if we want to continue with program!
      call err_clear
    else
      write(*,*) "Unknown error..."
      stop
    endif
  endif
  
  ! alternative to getitem: get - returns given default value if key
  ! not found, no KeyError exception raised
  ierror = di%get(a_value, "volume", 1.0)
  write(*,*) "volume = ", a_value

  call di%destroy
  call forpy_finalize

end program
```

## Import a Python module in Fortran

The following demo, shows how to use a module from Python's standard
library and introduces `call_py`, which is used to call Python methods and
to instantiate Python objects.

```Fortran
program date_demo
  use forpy_mod
  implicit none

  integer :: ierror
  type(module_py) :: datetime
  type(object) :: date, today, today_str
  character(len=:), allocatable :: today_fortran

  ! Python:
  ! import datetime
  ! date = datetime.date
  ! today = date.today()
  ! today_str = today.isoformat()
  ! print("Today is ", today_str)

  ierror = forpy_initialize()
  ierror = import_py(datetime, "datetime")
  ierror = datetime%getattribute(date, "date")

  ierror = call_py(today, date, "today")
  ierror = call_py(today_str, today, "isoformat")
  ierror = cast(today_fortran, today_str)

  write(*,*) "Today is ", today_fortran

  call datetime%destroy
  call date%destroy
  call today%destroy
  call today_str%destroy

  call forpy_finalize

end program
```

For Python to import a module that is not in one of the standard search
directories, you can set the environment variable `PYTHONPATH`:

```
export PYTHONPATH=$PYTHONPATH:path_to_my_python_module 
```

Alternatively, you can use forpy's `get_sys_path` function to retrieve and modify the list
of Python module search paths, as shown in the following example.

We want to import the following small Python module:

```Python
# File: mymodule.py

def print_args(*args, **kwargs):
    print("Arguments: ", args)
    print("Keyword arguments: ", kwargs)
    
    return "Returned from mymodule.print_args"
```

Now we use the module in Fortran, assuming that `mymodule.py` is in the current 
working directory:

```Fortran
program mymodule_example
  use forpy_mod
  implicit none

  integer :: ierror
  type(tuple) :: args
  type(dict) :: kwargs
  type(module_py) :: mymodule
  type(object) :: return_value
  type(list) :: paths
  character(len=:), allocatable :: return_string

  ierror = forpy_initialize()

  ! Instead of setting the environment variable PYTHONPATH,
  ! we can add the current directory "." to sys.path
  ierror = get_sys_path(paths)
  ierror = paths%append(".")
  
  ierror = import_py(mymodule, "mymodule")
  
  ! Python: 
  ! return_value = mymodule.print_args(12, "Hi", True, message="Hello world!")
  ierror = tuple_create(args, 3)
  ierror = args%setitem(0, 12)
  ierror = args%setitem(1, "Hi")
  ierror = args%setitem(2, .true.)
  
  ierror = dict_create(kwargs)
  ierror = kwargs%setitem("message", "Hello world!")
  
  ierror = call_py(return_value, mymodule, "print_args", args, kwargs)

  ierror = cast(return_string, return_value)
  write(*,*) return_string

  ! For call_py, args and kwargs are optional
  ! use call_py_noret to ignore the return value
  ! E. g.:
  ! ierror = call_py_noret(mymodule, "print_args")

  call args%destroy
  call kwargs%destroy
  call mymodule%destroy
  call return_value%destroy
  call paths%destroy
  
  call forpy_finalize

end program
```

## Working with arrays

Forpy offers interoperability of Fortran arrays and numpy arrays through
the type `ndarray`. In the
following examples, you will see various ways to create a numpy array.

### Creating a numpy array from a Fortran array

The simplest way to create a numpy array is with `ndarray_create`. This
function creates a numpy array with the same content as a Fortran array that is
passed to the function. For example: 

```Fortran
program ndarray01
  use forpy_mod
  implicit none

  integer, parameter :: NROWS = 2
  integer, parameter :: NCOLS = 3
  integer :: ierror, ii, jj
  
  real :: matrix(NROWS, NCOLS)
  
  type(ndarray) :: arr

  ierror = forpy_initialize()

  do jj = 1, NCOLS
    do ii = 1, NROWS
      matrix(ii, jj) = real(ii) * jj
    enddo
  enddo

  ! creates a numpy array with the same content as 'matrix'
  ierror = ndarray_create(arr, matrix)
  
  ierror = print_py(arr)

  call arr%destroy
  call forpy_finalize

end program
```

When arrays get very large, creating a copy might not be what you want. The next section
describes how to wrap a Fortran array with forpy without making a copy.

### Creating a numpy wrapper for a Fortran array

When creating a numpy array with `ndarray_create_nocopy`, no copy of the Fortran 
array is made. This is more efficient than `ndarray_create`, but there are
some things to consider: Changes to the Fortran array affect the numpy array
and vice versa. You have to make sure that the Fortran array is valid
as long as the numpy array is in use.

Since the Fortran array can now be modified not
only directly but also indirectly by the `ndarray`, it is necessary to
add the `asynchronous` attribute to the Fortran array declaration, since
without it compiler optimization related bugs
can occur (depending on code, compiler and compiler options).
Alternatively you could also use the `volatile` attribute.

```Fortran
program ndarray02
  use forpy_mod
  implicit none

  integer, parameter :: NROWS = 2
  integer, parameter :: NCOLS = 3
  integer :: ierror, ii, jj
  
  ! add the asynchronous attribute to the Fortran array that is wrapped
  ! as ndarray to avoid bugs caused by compiler optimizations
  real, asynchronous :: matrix(NROWS, NCOLS)
  
  type(ndarray) :: arr

  ierror = forpy_initialize()

  do jj = 1, NCOLS
    do ii = 1, NROWS
      matrix(ii, jj) = real(ii) * jj
    enddo
  enddo

  ! creates a numpy array that refers to 'matrix'
  ierror = ndarray_create_nocopy(arr, matrix)
  ierror = print_py(arr)

  matrix(1,1) = 1234.0 ! Change also affects 'arr'

  ierror = print_py(arr)

  call arr%destroy
  call forpy_finalize

end program
```

### Accessing array elements

The following example shows how to access the data of a ndarray with 
the method `ndarray%get_data`. It also shows how to return a ndarray
from a subroutine without using a copy of a Fortran array. 

We create a new `ndarray` with the function `ndarray_create_empty`, 
specifying the shape of the array.
In this case storage is allocated and managed by Python. Memory is freed, when
there is no reference to the ndarray anymore (don't forget to call the `destroy` method).

You can also create an array of zeros with `ndarray_create_zeros` and an array
of ones with `ndarray_create_ones`. 

To edit the values of the array, use the Fortran
pointer returned from `ndarray%get_data`.

```Fortran
! Example of how to return a ndarray from a subroutine
program ndarray03
  use forpy_mod
  use iso_fortran_env, only: real64
  implicit none

  integer :: ierror
  type(ndarray) :: arr

  ierror = forpy_initialize()

  call create_matrix(arr)
  ierror = print_py(arr)

  call arr%destroy
  call forpy_finalize

  CONTAINS

  subroutine create_matrix(arr)
    type(ndarray), intent(out) :: arr
    integer :: ierror, ii, jj
    integer, parameter :: NROWS = 2
    integer, parameter :: NCOLS = 3
    real(kind=real64), dimension(:,:), pointer :: matrix    

    ierror = ndarray_create_empty(arr, [NROWS, NCOLS], dtype="float64")
    
    !Use ndarray%getdata to access the content of a numpy array
    !from Fortran
    
    !type of matrix must be compatible with dtype of ndarray 
    !(here: real(kind=real64) and dtype="float64") 
    ierror = arr%get_data(matrix) 

    do jj = 1, NCOLS
      do ii = 1, NROWS
        matrix(ii, jj) = real(ii, kind=real64) * jj
      enddo
    enddo

  end subroutine

end program
```

## Matplotlib example
This example puts together, what you have learnt so far and demonstrates
a simple way to do complete error handling and some exception handling.
Save the file with an uppercase .F90 extension, since it uses a
C preprocessor macro for error handling.

```Fortran
#define errcheck if(ierror/=0) then;call err_print;stop;endif 
program matplotlib_example
  use forpy_mod
  implicit none

  integer :: ierror, ii
  real, parameter :: PI = 3.1415927
  integer, parameter :: NPOINTS = 200
  real :: x(NPOINTS)
  real :: y(NPOINTS)

  do ii = 1, NPOINTS
    x(ii) = ((ii-1) * 2. * PI)/(NPOINTS-1)
    y(ii) = sin(x(ii))
  enddo

  ierror = forpy_initialize()
  ! forpy_initialize returns NO_NUMPY_ERROR if numpy could not be imported
  ! You could still use forpy without the array features, but here we need them.
  if (ierror == NO_NUMPY_ERROR) then
    write(*,*) "This example needs numpy..."
    stop
  endif
  
  errcheck

  call simple_plot(x, y)

  call forpy_finalize

  CONTAINS

  subroutine simple_plot(x, y)
    real, asynchronous, intent(in) :: x(:)
    real, asynchronous, intent(in) :: y(:)

    integer :: ierror
    type(module_py) :: plt
    type(tuple) :: args    
    type(ndarray) :: x_arr, y_arr

    ierror = import_py(plt, "matplotlib.pyplot")
    
    ! You can also test for certain exceptions
    if (ierror /= 0) then
      if (exception_matches(ImportError)) then
        write(*,*) "This example needs matplotlib..."
        stop
      else
        call err_print
        stop
      endif
    endif

    ierror = ndarray_create_nocopy(x_arr, x)
    errcheck

    ierror = ndarray_create_nocopy(y_arr, y)
    errcheck

    ierror = tuple_create(args, 2)
    errcheck

    ierror = args%setitem(0, x_arr)
    errcheck
    ierror = args%setitem(1, y_arr)
    errcheck

    ierror = call_py_noret(plt, "plot", args)
    errcheck
    ierror = call_py_noret(plt, "show")
    errcheck

    call x_arr%destroy
    call y_arr%destroy
    call args%destroy
    call plt%destroy
  end subroutine

end program
```

## Converting between types: `cast` and `cast_nonstrict`

As we have seen in previous sections you can convert between types with
the `cast` interface.
The `cast` function has a rather strict behaviour, when casting between
types: For example it gives an error, when you try to convert a 
Python float to an integer or a list to a tuple.
Use `cast_nonstrict` if you need more flexibility: it does these type
conversion when possible. For example:

```Fortran
program cast_nonstrict_demo
use forpy_mod
  implicit none

  type(object) :: obj
  character(len=:), allocatable :: fstr
  integer :: an_int
  integer :: ierror

  ierror = forpy_initialize()
  ierror = cast(obj, 3.14d0)           !creates a Python float

  ierror = cast(an_int, obj)           !FAIL: strict cast float->integer
  call err_print                       !show and clear error
  ierror = cast(fstr, obj)             !FAIL: obj is a number, not a string
  call err_print                       !show and clear error

  ierror = cast_nonstrict(an_int, obj) !OK, truncates float (an_int = 3)
  ierror = cast_nonstrict(fstr, obj)   !OK, result is string "3.14"

  write(*,*) an_int
  write(*,*) fstr

  call obj%destroy
  call forpy_finalize
end program
```

## Python 2 support
Requirements: Python version >= 2.7

For Python 2 support, you have to define the preprocessor macro PYTHON2 (compiler option -DPYTHON2).

```
gfortran -c -DPYTHON2 forpy_mod.F90
gfortran intro_to_forpy.F90 forpy_mod.o `python2-config --ldflags`
```

Note that here, you use python2-config. 

If `python2-config` is not present on your system, install the package 
`python-dev` (Ubuntu, Debian). 

On a 32-bit system use the macro PYTHON2_32

```
gfortran -c -DPYTHON2_32 forpy_mod.F90
gfortran intro_to_forpy.F90 forpy_mod.o `python2-config --ldflags`
```

On a narrow Python 2 build (**Windows**, Mac?), add PYTHON_NARROW:

```
gfortran -c -DPYTHON2 -DPYTHON_NARROW forpy_mod.F90
gfortran intro_to_forpy.F90 forpy_mod.o `python2-config --ldflags`
```

"Narrow" Python builds use 2 bytes for Unicode characters, wereas
"wide" builds use 4 bytes. This distinction is not relevant
when using forpy with Python 3.

## Developing Python modules in Fortran

With forpy, you can not only use Python from Fortran, but also write
Python modules in Fortran, using all the Python datatypes you like.

Note that now we have to build a shared library and the commands for
building are different.
Save the example below as `extexample01.F90` and build with:

```
gfortran -c -fPIC forpy_mod.F90
gfortran -shared -fPIC -o extexample01.so extexample01.F90 forpy_mod.o
```

The following module `extexample01` will have one method `print_args` and a
numerical constant `pi` as members:

```Fortran
module extexample01
use forpy_mod
use iso_c_binding
implicit none

! You need to declare exactly one PythonModule and PythonMethodTable
! at Fortran module level
type(PythonModule), save :: mod_def
type(PythonMethodTable), save :: method_table

CONTAINS

! Initialisation function for Python 3
! called when importing module
! must use bind(c, name="PyInit_<module name>")
! return value must be type(c_ptr), use the return value of PythonModule%init
function PyInit_extexample01() bind(c, name="PyInit_extexample01") result(m)
  type(c_ptr) :: m
  m = init()
end function

! Initialisation function for Python 2
! called when importing module
! must use bind(c, name="init<module name>")
! Initialisation function for Python 2
! called when importing module
! must be called init<module name>
subroutine initextexample01() bind(c, name="initextexample01")
  type(c_ptr) :: m
  m = init()
end subroutine

function init() result(m)
  type(c_ptr) :: m
  integer :: ierror
  type(object) :: pi
  
  ierror = forpy_initialize()
  
  call method_table%init(1) ! module shall have 1 method
  
  ! must add function print_args to method table to be able to use it in Python
  
  call method_table%add_method("print_args", &                  ! method name
                               "Prints arguments and keyword arguments", &  !doc-string
                               METH_VARARGS + METH_KEYWORDS, &  ! this method takes arguments AND keyword arguments
                               c_funloc(print_args))            ! address of Fortran function to add
                               
  m = mod_def%init("extexample01", "A Python extension with a method and a member.", method_table)
  
  ! Example: Numerical constant as member of module
  ierror = cast(pi, 3.141592653589793d0)
  ierror = mod_def%add_object("pi", pi)
  call pi%destroy
end function

! Implementation of our Python method
!
! Corresponding Python method shall allow arguments and keyword arguments
! -> We need 3 "type(c_ptr), value" arguments
! First arg is c_ptr to module, second is c_ptr to argument tuple
! third is c_ptr to keyword argument dict
! Return value must be type(c_ptr)
! bind(c) attribute to make sure that C calling conventions are used
function print_args(self_ptr, args_ptr, kwargs_ptr) result(r) bind(c)
  type(c_ptr), value :: self_ptr
  type(c_ptr), value :: args_ptr
  type(c_ptr), value :: kwargs_ptr
  type(c_ptr) :: r
  
  type(tuple) :: args
  type(dict) :: kwargs
  type(NoneType) :: retval
  integer :: ierror
  
  ! use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
  call unsafe_cast_from_c_ptr(args, args_ptr)
  call unsafe_cast_from_c_ptr(kwargs, kwargs_ptr)
  
  r = C_NULL_PTR ! in case of exception return C_NULL_PTR

  if (is_null(kwargs)) then
    ! This is a check if keyword arguments were passed to this function.
    ! If is_null(kwargs), kwargs is not a valid Python object, therefore
    ! we initialise it as an empty dict
    ierror = dict_create(kwargs)
  endif
  
  ierror = print_py(args)
  ierror = print_py(kwargs)
  
  ! You always need to return a Python object (as c_ptr) in the error free case. 
  ! If you do not need a return value, return a Python None
  ! In case of an exception return C_NULL_PTR

  ierror = NoneType_create(retval)
  r = retval%get_c_ptr() ! need return value as c_ptr
  
  call args%destroy
  call kwargs%destroy
  
end function

end module
```

Python code to test the module:

```Python
import extexample01
extexample01.print_args("hello", 42, key="abc")
print(extexample01.pi)
```

# Developer info

## Running tests

```
cd tests
make clean
make runtests
```

For ifort use `make FC=ifort` and for testing with Python 2 use `make PY_VERSION=2`, e. g.
for ifort and Python 2:

```
make PY_VERSION=2 FC=ifort
```

## Developing forpy

Forpy is created from a template file. Therefore *do not* edit
forpy_mod.F90, but only `forpy_mod.fypp`. This template file has to be preprocessed using
Balint Aradi's [fypp](https://github.com/aradi/fypp).

Assuming that you have fypp in your current directory, type

```
python fypp.py forpy_mod.fypp forpy_mod.F90
```

## Building documentation

You can create documentation from the source code with Chris MacMackin's 
[FORD](https://github.com/Fortran-FOSS-Programmers/ford) documentation generator:

```
ford forpy_project.md
```

## Support for debug builds of Python

When using a debug build of Python, one has to define the preprocessor macro `Py_DEBUG` when compiling forpy.

## Running tests with reference count checks

You can run the forpy test suites such that the difference of the total reference count of Python objects
before and after each test is printed. This helps with detecting reference counting bugs. To do this you need
a debug build of Python *and* a debug build of numpy. Then build the tests with:

```
cd tests
make clean
make PY_DEBUG=1
```

If the difference in total reference count is non-zero, the difference is printed before the test status. A
non-zero difference in total reference count does not necessarily mean that there is an error, for example due to
internal caching or deleted objects. On the other hand, a difference of zero does not guarantee absence of reference
count errors. 

# Notes

## Using forpy with Anaconda

When using forpy with Anaconda and gfortran, you might encounter the following error:

```
/usr/bin/x86_64-linux-gnu-ld: error: lto-wrapper failed
collect2: error: ld returned 1 exit status
```

1) A solution to this problem is to add the `-fno-lto` (disable link-time optimisation) compiler flag in the linking step:

```
gfortran -c forpy_mod.F90
gfortran intro_to_forpy.F90 forpy_mod.o -fno-lto `python3-config --ldflags`
```

2) OR: Another solution is to use the `gfortran` compiler provided by the Anaconda distribution.
(Install on Linux with `conda install gfortran_linux-64`)

See [Anaconda compiler tools](https://docs.conda.io/projects/conda-build/en/latest/resources/compiler-tools.html)
