list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

add_library(firstorder 
    firstorder-graph.cpp 
    firstorder-analysis.cpp
    # ${tree_sitter_python_dir}/src/parser.c
    # ${tree_sitter_python_dir}/src/scanner.cc
    )
target_link_libraries(firstorder
    sparta
    tree-sitter
    tree-sitter-python
    fmt)

target_link_options(firstorder
    PUBLIC $<$<CONFIG:DEBUG>:-lboost_stacktrace_backtrace -ldl -lbacktrace>)
target_compile_definitions(firstorder
    PUBLIC $<$<CONFIG:DEBUG>:"BOOST_STACKTRACE_LINK">)


include(project_props)

# set_source_files_properties(
#     ${tree_sitter_python_dir}/src/scanner.cc
#     ${tree_sitter_python_dir}/src/parser.c
#     PROPERTIES COMPILE_OPTIONS -w)