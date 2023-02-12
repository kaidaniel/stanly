# TODO: add_custom_target to run sanitizers
add_library(firstorder ${CMAKE_CURRENT_SOURCE_DIR}/firstorder.cpp)
target_link_libraries(firstorder sparta)

add_library(project_props INTERFACE)
target_compile_options(project_props INTERFACE 
    -Wall 
    -Wextra 
    -Wpedantic
    $<$<AND:$<CONFIG:DEBUG>,$<CXX_COMPILER_ID:Clang>>:-O0 -g -fdebug-macro -ggdb3 -gsimple-template-names>)
target_include_directories(project_props INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)
set_target_properties(project_props PROPERTIES 
    CXX_CLANG_TIDY clang-tidy
    CXX_INCLUDE_WHAT_YOU_USE iwyu
    CXX_LINK_WHAT_YOU_USE TRUE)

get_property(tgts DIRECTORY PROPERTY BUILDSYSTEM_TARGETS)
foreach(tgt ${tgts})
    if(NOT "${tgt}" STREQUAL project_props)
        target_link_libraries(${tgt} project_props)
    endif()
endforeach()
