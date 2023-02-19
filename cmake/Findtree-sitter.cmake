include(FetchContent)

project(
  tree-sitter
  LANGUAGES C
  VERSION 0.20.6
)

set(tree_sitter_dir "${CMAKE_BINARY_DIR}/tree-sitter-src")

FetchContent_Declare(
    tree_sitter_extern
    GIT_REPOSITORY git@github.com:tree-sitter/tree-sitter.git
    GIT_TAG v0.20.7
    SOURCE_DIR        ${tree_sitter_dir}
)
FetchContent_Populate(tree_sitter_extern)

file(GLOB tree_sitter_src "${tree_sitter_dir}/lib/src/*.c")

add_library(tree-sitter ${tree_sitter_src})
target_include_directories(tree-sitter
  PRIVATE ${tree_sitter_dir}/lib/src
  PUBLIC ${tree_sitter_dir}/lib/include)

set_target_properties(tree-sitter PROPERTIES
    C_STANDARD 99 C_STANDARD_REQUIRED ON POSITION_INDEPENDENT_CODE ON)