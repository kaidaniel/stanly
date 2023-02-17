include(shared)

function(run_linters)
    file(GLOB source_files src/*.cpp src/include/*.h)
    introduce(clang-tidy)
    execute_cmd(clang-tidy -p ${binary_dir}/clang/Release/ ${source_files})
    introduce(clang-format)
    execute_cmd(clang-format --verbose -i ${source_files})
endfunction()