cmake_minimum_required(VERSION 3.22)
project(ksar VERSION 0.0.1)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_library(firstorder src/firstorder.cpp)
target_include_directories(firstorder PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/SPARTA/include)

include(CTest)
