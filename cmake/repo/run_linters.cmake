include(shared)

function(run_linters)
    file(GLOB source_files src/*.cpp src/*.h)
    introduce(clang-tidy)
    execute_cmd(clang-tidy ${source_files})
    introduce(clang-format)
    execute_cmd(clang-format --verbose -i ${source_files})
endfunction()