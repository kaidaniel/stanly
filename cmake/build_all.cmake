function(execute_build opts)
    execute_process(
        COMMAND ${CMAKE_COMMAND} ${opts}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMAND_ECHO STDOUT
        COMMAND_ERROR_IS_FATAL ANY)
    message("")
endfunction()

execute_build(--preset=clang-debug)
execute_build(--preset=clang-release)
execute_build(--preset=gcc-debug)
execute_build(--preset=gcc-release)
execute_build("--build;--preset=clang-debug")
execute_build("--build;--preset=clang-release")
execute_build("--build;--preset=gcc-debug")
execute_build("--build;--preset=gcc-release")