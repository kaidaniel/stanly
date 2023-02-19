include(shared)

function(run_linters)
    file(GLOB source_files src/*.cpp src/include/*.h test/*.cpp)
    introduce(clang-tidy)
    execute_cmd(clang-tidy -p ${binary_dir}/clang++-15/Release/ ${source_files})
    introduce(clang-format)
    execute_cmd(clang-format --verbose -i ${source_files})
endfunction()