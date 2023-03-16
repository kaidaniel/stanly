list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
include(Catch)

# add_library(firstorder 
#     firstorder-graph.cpp 
#     firstorder-analysis.cpp
#     )
# target_link_libraries(firstorder
#     sparta
#     tree-sitter
#     tree-sitter-python
#     fmt
#     range-v3)

set_target_properties(fmt range-v3 sparta tree-sitter tree-sitter-python Catch2
    PROPERTIES EXCLUDE_FROM_ALL TRUE)

foreach(test test-iterator test-metaprogramming)
    add_executable(${test} ${test}.cpp)
    target_link_libraries(${test} PRIVATE Catch2::Catch2WithMain)
    catch_discover_tests(${test} EXTRA_ARGS --colour-mode ansi)
endforeach()

include(project_props)