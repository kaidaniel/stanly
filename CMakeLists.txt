cmake_minimum_required(VERSION 3.20)
project(ksar VERSION 0.0.1 LANGUAGES CXX)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

find_package(Sparta REQUIRED)
find_package(fmt CONFIG REQUIRED)
find_package(Catch2 CONFIG REQUIRED)
find_package(unofficial-tree-sitter CONFIG REQUIRED)


add_subdirectory(src)
add_subdirectory(docs)
add_subdirectory(test)
