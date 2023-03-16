add_library(project_props INTERFACE)
target_compile_features(project_props INTERFACE cxx_std_20)
target_compile_options(project_props INTERFACE 
    -Wall 
    -Wextra 
    -Wpedantic 
    "$<$<CONFIG:DEBUG>:-ffile-prefix-map=/home/kai/projects/src/=.>"
    "$<$<CONFIG:DEBUG>:-ffile-prefix-map=.build/default/=BUILD>")

get_property(tgts DIRECTORY PROPERTY BUILDSYSTEM_TARGETS)
foreach(tgt ${tgts})
    if(NOT "${tgt}" STREQUAL project_props)
        target_link_libraries(${tgt} PRIVATE project_props fmt range-v3)
    endif()
endforeach()
