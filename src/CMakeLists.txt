list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
include(Catch)

set_target_properties(fmt range-v3 sparta tree-sitter tree-sitter-python Catch2
    PROPERTIES EXCLUDE_FROM_ALL TRUE)

foreach(test test-metaprogramming test-firstorder-format test-firstorder-parser)
    add_executable(${test} ${test}.cpp)
    target_link_libraries(${test} PRIVATE fmt Catch2::Catch2WithMain)
    catch_discover_tests(${test} EXTRA_ARGS --colour-mode ansi)
endforeach()

add_library(parser parser.cpp)
target_link_libraries(parser PUBLIC tree-sitter tree-sitter-python)

add_library(firstorder firstorder-parser.cpp)
target_link_libraries(firstorder PUBLIC parser)

target_link_libraries(test-firstorder-parser PRIVATE firstorder)

include(project_props)
