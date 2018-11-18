project: Forpy
src_dir: ./
exclude_dir: ./tests
             ./doc
             ./doc/src
output_dir: ./doc
project_github: https://github.com/ylikx/forpy
summary: Forpy - use Python in Fortran. A library for Fortran-Python interoperability
author: Elias Rabel
author_description: Graz, Austria
github: https://github.com/ylikx
predocmark: >
media_dir: ./media
docmark_alt: #
predocmark_alt: <
display: public
         protected
source: false
graph: false
search: true

Forpy allows you to use Python features in Fortran ("Python in Fortran")

For example: datastructures such as list, dict, tuple and interoperability
of arrays using numpy.
It allows you to use your own and third-party Python modules.

Furthermore you can write Python (extension) modules in Fortran ("Fortran in Python")

# Starting points

## Basics

- Start here: [README](https://www.github.com/ylikx/forpy/README.md)
- Initializing forpy: [[forpy_initialize]]
- Lists: [[list]], [[list_create]]
- Tuples: [[tuple]], [[tuple_create]]
- Dictionaries: [[dict]], [[dict_create]]
- Generic Python object: [[object]]
- Converting between Fortran and Python types: [[cast]], [[cast_nonstrict]]
- Calling Python functions: [[call_py]], [[call_py_noret]]
- Importing Python modules: [[import_py]]
- Error handling, debugging: [[err_print]], [[exception_matches]], [[err_clear]], [[print_py]]
- Assignment between Python objects: [[assign_py]]

## Arrays

- [[ndarray]], [[ndarray_create]]
- [[ndarray_create_empty]], [[ndarray_create_zeros]], [[ndarray_create_ones]]

## Python module development

- [[PythonModule]]
- [[PythonMethodTable]]

## License

LGPL v3
