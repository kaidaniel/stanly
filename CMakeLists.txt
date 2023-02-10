cmake_minimum_required(VERSION 3.22)
project(ksar 
    VERSION 0.0.1
    LANGUAGES CXX)

list(APPEND CMAKE_MODULE_PATH cmake_modules)
include(set_globals)
include(add_target_project_options)
include(add_target_sparta)

add_library(firstorder src/firstorder.cpp)
target_link_libraries(firstorder project_options)
target_link_libraries(firstorder sparta)

# TODO configure in 'ctest -S build.make' (which can live in a project-agnostic location)
# adding e.g. valgrind, ubsan etc.
include(CTest)
