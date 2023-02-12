cmake_minimum_required(VERSION 3.22)
project(ksar 
    VERSION 0.0.1
    LANGUAGES CXX)

list(APPEND CMAKE_MODULE_PATH cmake)
# TODO: add_custom_target to run sanitizers

add_subdirectory(extern)
add_subdirectory(src)
add_subdirectory(docs)
add_subdirectory(test)
