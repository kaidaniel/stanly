# TODO: add_custom_target to run sanitizers

add_library(project_props INTERFACE)
target_compile_options(project_props INTERFACE -Wall -Wextra -Wpedantic $<$<CONFIG:DEBUG>:-Werror>)
target_include_directories(project_props INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_features(project_props INTERFACE cxx_std_20)

get_property(tgts DIRECTORY PROPERTY BUILDSYSTEM_TARGETS)
foreach(tgt ${tgts})
    if(NOT "${tgt}" STREQUAL project_props)
        target_link_libraries(${tgt} project_props)
    endif()
endforeach()