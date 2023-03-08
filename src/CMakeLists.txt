list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

add_library(firstorder 
    firstorder-graph.cpp 
    firstorder-analysis.cpp)
target_link_libraries(firstorder
    sparta
    tree-sitter
    tree-sitter-python
    fmt
    range-v3)

target_link_options(firstorder
    PUBLIC $<$<CONFIG:DEBUG>:-lboost_stacktrace_backtrace -ldl -lbacktrace>)
target_compile_definitions(firstorder
    PUBLIC $<$<CONFIG:DEBUG>:"BOOST_STACKTRACE_LINK">)

include(project_props)

add_executable(test-iterator test-iterator.cpp)
target_link_libraries(test-iterator PRIVATE project_props fmt Catch2::Catch2WithMain)
include(Catch)
catch_discover_tests(test-iterator EXTRA_ARGS --colour-mode ansi)
