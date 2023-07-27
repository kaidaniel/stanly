list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
include(Catch)

set_target_properties(sparta tree-sitter tree-sitter-python Catch2
    PROPERTIES EXCLUDE_FROM_ALL TRUE)

foreach(test test-parse test-format test-analyse)
    add_executable(${test} ${test}.cpp)
    target_link_libraries(${test} PRIVATE Catch2::Catch2WithMain parse analyse)
    catch_discover_tests(${test} EXTRA_ARGS --colour-mode ansi)
endforeach()

add_library(cursor cursor.cpp)
target_link_libraries(cursor PRIVATE tree-sitter tree-sitter-python)

add_library(parse parse.cpp string-index.cpp)
target_link_libraries(parse PUBLIC tree-sitter tree-sitter-python)

add_library(parse2 parse2.cpp)
target_link_libraries(parse2 PUBLIC tree-sitter tree-sitter-python)

add_library(analyse analyse.cpp)
target_link_libraries(analyse PUBLIC sparta)

add_executable(stanly stanly.cpp)
target_link_libraries(stanly PRIVATE cxxopts parse analyse)

add_executable(lookup-symbols lookup-symbols.cpp)
target_link_libraries(lookup-symbols PRIVATE tree-sitter tree-sitter-python)

add_library(static-assertions static-assertions.cpp)
target_link_libraries(static-assertions PRIVATE analyse parse2)

foreach(target 
cursor parse parse2 analyse stanly lookup-symbols static-assertions
test-parse test-format test-analyse)
    target_compile_features(${target} PRIVATE cxx_std_20)
    target_compile_options(${target} PRIVATE -Wall -Wextra -Wpedantic -fdiagnostics-show-option -fdiagnostics-color=always)
    #target_include_directories(${target} ${Boost_INCLUDE_DIRS})
endforeach()
