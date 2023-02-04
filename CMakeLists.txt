cmake_minimum_required(VERSION 3.22)
project(ksar VERSION 0.0.1)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)


add_executable(ksar src/ksar.cpp)
target_include_directories(ksar PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_include_directories(ksar PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/SPARTA/include)

include(CTest)
