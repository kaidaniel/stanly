include(shared)

function(configure_build_test build_typ compiler)
    set(dir ${binary_dir}/${compiler}/${build_typ})
    cmake_path(RELATIVE_PATH dir BASE_DIRECTORY ${CMAKE_SOURCE_DIR} OUTPUT_VARIABLE target_dir)
    
    introduce(${target_dir}:configure)
    execute_cmd(
        ${CMAKE_COMMAND} -S . -B ${target_dir} -G Ninja -Werror=deprecated
        -DCMAKE_BUILD_TYPE=${build_typ}
        -DCMAKE_CXX_COMPILER=${compiler}
        -DCMAKE_COLOR_DIAGNOSTICS=ON
        -DCMAKE_EXPORT_COMPILE_COMMANDS=ON)

    introduce(${target_dir}:build)
    execute_cmd(${CMAKE_COMMAND} --build ${target_dir} --parallel 4)

    introduce(${target_dir}:test)
    execute_cmd(${CMAKE_CTEST_COMMAND} --test-dir ${target_dir} -T test)
endfunction()