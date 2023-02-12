set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
# TODO: add_custom_target to run sanitizers

add_custom_target(dependency_graph ALL
    COMMAND sh -c "${CMAKE_COMMAND} -S .-B ${CMAKE_BINARY_DIR} --graphviz=${CMAKE_BINARY_DIR}/dependency_graph.dot"
    COMMAND sh -c "dot -Tpng ${CMAKE_BINARY_DIR}/dependency_graph.dot -o ${CMAKE_BINARY_DIR}/dependency_graph.png"
    BYPRODCUTS ${CMAKE_BINARY_DIR}/dependency_graph.dot ${CMAKE_BINARY_DIR}/dependency_graph.png
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "generating dependency_graph.png")

add_library(project_options INTERFACE)
target_compile_options(project_options INTERFACE -Wall -Wextra)
target_include_directories(project_options INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)
set_target_properties(project_options PROPERTIES 
    CXX_CLANG_TIDY clang-tidy
    CXX_INCLUDE_WHAT_YOU_USE iwyu
    CXX_LINK_WHAT_YOU_USE TRUE)

add_library(firstorder ${CMAKE_CURRENT_SOURCE_DIR}/firstorder.cpp)
target_link_libraries(firstorder project_options)
target_link_libraries(firstorder sparta)
