list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
include(Catch)

set_target_properties(sparta tree-sitter tree-sitter-python Catch2
    PROPERTIES EXCLUDE_FROM_ALL TRUE)
add_library(project_props INTERFACE)
target_compile_features(project_props INTERFACE cxx_std_20)
target_compile_options(project_props INTERFACE 
    -Wall 
    -Wextra 
    -Wpedantic 
    -fdiagnostics-show-option
    -fdiagnostics-color=always)
target_include_directories(project_props INTERFACE ${Boost_INCLUDE_DIRS})

foreach(test test-format test-parser)
    add_executable(${test} ${test}.cpp)
    target_link_libraries(${test} PRIVATE project_props Catch2::Catch2WithMain)
    catch_discover_tests(${test} EXTRA_ARGS --colour-mode ansi)
endforeach()

add_library(parser firstorder-cursor.cpp parser.cpp)
target_link_libraries(parser PUBLIC tree-sitter tree-sitter-python project_props)

target_link_libraries(test-parser PRIVATE parser)
