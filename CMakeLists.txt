cmake_minimum_required(VERSION 3.20)
project(ksar 
    VERSION 0.0.1
    LANGUAGES CXX)

list(APPEND CMAKE_MODULE_PATH cmake)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_subdirectory(extern)
add_subdirectory(src)
add_subdirectory(docs)
add_subdirectory(test)
