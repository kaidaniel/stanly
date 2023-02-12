cmake_minimum_required(VERSION 3.22)
project(ksar 
    VERSION 0.0.1
    LANGUAGES CXX)

list(APPEND CMAKE_MODULE_PATH cmake)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT EXISTS "${CMAKE_BINARY_DIR}/dependency_graph.dot")
    add_custom_target(dependency_graph ALL
        COMMAND sh -c "${CMAKE_COMMAND} -S ${CMAKE_SOURCE_DIR} -B ${CMAKE_BINARY_DIR} --graphviz=${CMAKE_BINARY_DIR}/dependency_graph.dot"
        COMMAND sh -c "dot -Tpng ${CMAKE_BINARY_DIR}/dependency_graph.dot -o ${CMAKE_BINARY_DIR}/dependency_graph.png"
        COMMAND sh -c "rm ${CMAKE_BINARY_DIR}/dependency_graph.dot.*"
        BYPRODUCTS ${CMAKE_BINARY_DIR}/dependency_graph.dot ${CMAKE_BINARY_DIR}/dependency_graph.png
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "generating dependency_graph.png")
endif()


add_subdirectory(extern)
add_subdirectory(src)
add_subdirectory(docs)
add_subdirectory(test)
