include(shared)

function(parse_args)
    set(help "usage: 'cmake -P test_repo.cmake [--help-script][--remove]'")
    set(first_arg ${CMAKE_ARGV3})
    if(first_arg STREQUAL --help-script)
        message(FATAL_ERROR ${help})
    endif()
    set(remove ${first_arg})
    set(_remove_vals --remove -r)
    if(CMAKE_ARGC GREATER 4 OR (CMAKE_ARGC EQUAL 4 AND NOT remove IN_LIST _remove_vals))
        message(FATAL_ERROR "unrecognized arguments. ${help} ${remove}")
    endif()
    if(NOT (EXISTS ${CMAKE_SOURCE_DIR}/CMakeLists.txt AND EXISTS ${CMAKE_SOURCE_DIR}/CMakePresets.json))
        message(FATAL_ERROR "only run from the project root")
    endif()
    if(remove)
        file(REMOVE_RECURSE ${binary_dir})
    endif()
endfunction()