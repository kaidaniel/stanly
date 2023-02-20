cmake_minimum_required(VERSION 3.20)
project(stanly VERSION 0.0.1 LANGUAGES CXX)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

find_package(sparta REQUIRED)  # defines target 'sparta'
find_package(Catch2 REQUIRED)  # defines target 'Catch2::Catch2WithMain'
find_package(tree-sitter REQUIRED)  # defines target 'tree-sitter' and 'tree-sitter-python'
#find_package(fmt CONFIG REQUIRED)
#find_package(unofficial-tree-sitter CONFIG REQUIRED)

add_subdirectory(src)
add_subdirectory(docs)
enable_testing()
add_subdirectory(test)
