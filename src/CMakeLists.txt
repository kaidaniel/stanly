list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
include(Catch)

set_target_properties(sparta tree-sitter tree-sitter-python Catch2
    PROPERTIES EXCLUDE_FROM_ALL TRUE)
add_library(project_props INTERFACE)
target_compile_features(project_props INTERFACE cxx_std_20)
target_compile_options(project_props INTERFACE 
    -Wall -Wextra -Wpedantic -fdiagnostics-show-option -fdiagnostics-color=always)

target_include_directories(project_props INTERFACE ${Boost_INCLUDE_DIRS})

foreach(test test-parse test-format test-analyse)
    add_executable(${test} ${test}.cpp)
    target_link_libraries(${test} PRIVATE Catch2::Catch2WithMain parse analyse project_props)
    catch_discover_tests(${test} EXTRA_ARGS --colour-mode ansi)
endforeach()

add_library(parse parse.cpp string-index.cpp)
target_link_libraries(parse PUBLIC tree-sitter tree-sitter-python project_props)

add_library(analyse analyse.cpp)
target_link_libraries(analyse PUBLIC sparta project_props)

add_executable(stanly stanly.cpp)
target_link_libraries(stanly PRIVATE cxxopts parse analyse)

add_executable(lookup-symbols lookup-symbols.cpp)
target_link_libraries(lookup-symbols PRIVATE tree-sitter tree-sitter-python project_props)
