cmake_minimum_required(VERSION 3.20)
project(stanly VERSION 0.0.1 LANGUAGES CXX)
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

add_compile_options("$<$<CONFIG:DEBUG>:-ffile-prefix-map=/home/kai/CLionProjects/ksar/=.>")
add_compile_options("$<$<CONFIG:DEBUG>:-ffile-prefix-map=.build/default/=BUILD>")

find_package(sparta REQUIRED)  # defines target 'sparta'
find_package(Catch2 REQUIRED)  # defines target 'Catch2::Catch2WithMain'
find_package(tree-sitter REQUIRED)  # defines target 'tree-sitter' and 'tree-sitter-python'
find_package(fmt REQUIRED)
find_package(range-v3 REQUIRED)
#find_package(unofficial-tree-sitter CONFIG REQUIRED)

enable_testing()
add_subdirectory(src)
add_subdirectory(docs)
add_subdirectory(test)
