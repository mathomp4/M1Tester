cmake_minimum_required(VERSION 3.22)
project(test VERSION 1.0.0 LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR})
include(GNU_Fortran)
include(Generic_Fortran)

set(CMAKE_BUILD_TYPE Release)

add_executable(test.exe test.F90)
