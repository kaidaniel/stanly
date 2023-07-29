add_library(parser tree-sitter-ast.cpp)
target_link_libraries(parser PRIVATE tree-sitter tree-sitter-python)

add_library(parse parse.cpp string-index.cpp)
target_link_libraries(parse PRIVATE tree-sitter tree-sitter-python)

add_library(parse2 parse2.cpp)
target_link_libraries(parse2 PRIVATE tree-sitter tree-sitter-python)
target_compile_definitions(parse2 PRIVATE JUMP_TABLE_MAX_SIZE=250)

add_library(analyse analyse.cpp)
target_link_libraries(analyse PUBLIC sparta)

add_executable(main stanly.cpp)
target_link_libraries(main PRIVATE cxxopts stanly)
set_target_properties(main PROPERTIES OUTPUT_NAME stanly)

add_executable(lookup-symbols lookup-symbols.cpp)
target_link_libraries(lookup-symbols PRIVATE tree-sitter tree-sitter-python)

add_library(project-props INTERFACE)
target_compile_features(project-props INTERFACE cxx_std_20)
target_compile_options(project-props INTERFACE -Wall -Wextra -Wpedantic -fdiagnostics-show-option -fdiagnostics-color=always)
target_include_directories(project-props INTERFACE ${CMAKE_SOURCE_DIR}/include/)
#target_include_directories(${target} ${Boost_INCLUDE_DIRS})

get_property(tgts DIRECTORY PROPERTY BUILDSYSTEM_TARGETS)
foreach(tgt ${tgts})
    if(NOT "${tgt}" STREQUAL project-props)
        target_link_libraries(${tgt} PUBLIC project-props)
    endif()
endforeach()

add_library(stanly INTERFACE)
target_link_libraries(stanly INTERFACE parse analyse)
