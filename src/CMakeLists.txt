list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

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

include(project_props)

set_target_properties(
    fmt range-v3 sparta tree-sitter tree-sitter-python Catch2
    PROPERTIES EXCLUDE_FROM_ALL TRUE
    )

foreach(header firstorder-lang graph iterator language metaprogramming)
add_library(${header} ${header}.h)
target_link_libraries(${header} PUBLIC project_props fmt range-v3)
endforeach()
add_library(stanly-api include/stanly-api.h)
target_link_libraries(stanly-api PUBLIC project_props fmt range-v3)

include(Catch)
foreach(test test-iterator test-metaprogramming)
add_executable(${test} ${test}.cpp)
target_link_libraries(${test} PRIVATE project_props fmt Catch2::Catch2WithMain)
catch_discover_tests(${test} EXTRA_ARGS --colour-mode ansi)
endforeach()

