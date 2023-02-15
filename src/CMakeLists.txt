add_library(firstorder ${CMAKE_CURRENT_SOURCE_DIR}/firstorder.cpp)
target_link_libraries(firstorder sparta)
include(cmake/project_props.cmake)
