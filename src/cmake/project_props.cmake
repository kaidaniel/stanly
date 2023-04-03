add_library(project_props INTERFACE)
target_compile_features(project_props INTERFACE cxx_std_20)
target_compile_options(project_props INTERFACE 
    -Wall 
    -Wextra 
    -Wpedantic 
    -fdiagnostics-show-option
    -fdiagnostics-color=always)

get_property(tgts DIRECTORY PROPERTY BUILDSYSTEM_TARGETS)
foreach(tgt ${tgts})
    if(NOT "${tgt}" STREQUAL project_props)
        target_link_libraries(
          ${tgt} PRIVATE 
          project_props 
          range-v3
          )
    endif()
endforeach()
