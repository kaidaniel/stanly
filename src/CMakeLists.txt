list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

add_library(firstorder ${CMAKE_CURRENT_SOURCE_DIR}/firstorder.cpp)
target_link_libraries(firstorder sparta)
include(project_props)
