include(FetchContent)

project(
  tree-sitter
  LANGUAGES C
  VERSION 0.20.6
)

set(tree_sitter_dir "${CMAKE_BINARY_DIR}/tree-sitter")

FetchContent_Declare(tree-sitter
    GIT_REPOSITORY git@github.com:tree-sitter/tree-sitter.git
    GIT_TAG v0.20.7
    SOURCE_DIR        ${tree_sitter_dir}
)
FetchContent_Populate(tree-sitter)

add_library(tree-sitter ${tree_sitter_dir}/lib/src/lib.c)
target_include_directories(tree-sitter
  PRIVATE ${tree_sitter_dir}/lib/src
  PUBLIC ${tree_sitter_dir}/lib/include)

set_target_properties(tree-sitter PROPERTIES
    C_STANDARD 99 C_STANDARD_REQUIRED ON POSITION_INDEPENDENT_CODE ON)


find_program(tree-sitter-cli tree-sitter REQUIRED)

set(tree_sitter_python_dir ${CMAKE_BINARY_DIR}/tree-sitter-python)

FetchContent_Declare(tree_sitter_python
  GIT_REPOSITORY git@github.com:tree-sitter/tree-sitter-python.git
  GIT_TAG v0.20.0
  SOURCE_DIR ${tree_sitter_python_dir})

FetchContent_Populate(tree_sitter_python)
execute_process(COMMAND tree-sitter generate WORKING_DIRECTORY ${tree_sitter_python_dir})
add_library(tree-sitter-python
  ${tree_sitter_python_dir}/src/parser.c
  ${tree_sitter_python_dir}/src/scanner.cc)

target_link_libraries(tree-sitter-python PUBLIC tree-sitter)

set_target_properties(tree-sitter-python PROPERTIES
  C_STANDARD 99 C_STANDARD_REQUIRED ON POSITION_INDEPENDENT_CODE ON)
