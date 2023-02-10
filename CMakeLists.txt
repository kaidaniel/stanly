cmake_minimum_required(VERSION 3.22)
project(ksar VERSION 0.0.1)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_library(project_options INTERFACE)
target_compile_options(project_options INTERFACE -Wall -Wextra)
target_compile_features(project_options INTERFACE cxx_std_20)
set_target_properties(project_options PROPERTIES 
    CXX_CLANG_TIDY clang-tidy
    CXX_INCLUDE_WHAT_YOU_USE iwyu
    CXX_LINK_WHAT_YOU_USE TRUE)

# TODO: get and build SPARTA from github instead
# e.g. by writing a find module Findsparta.cmake using include(FetchContent)?
# find_package(sparta REQUIRED)
add_library(sparta INTERFACE)
target_include_directories(sparta SYSTEM INTERFACE /usr/local/include/sparta)

add_library(firstorder src/firstorder.cpp)
target_link_libraries(firstorder project_options)
target_link_libraries(firstorder sparta)
#target_include_directories(firstorder SYSTEM PRIVATE /usr/local/include/sparta)

# TODO configure in 'ctest -S build.make' (which can live in a project-agnostic location)
# adding e.g. valgrind, ubsan etc.
include(CTest)
