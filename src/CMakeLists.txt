list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

add_library(firstorder 
    firstorder-graph.cpp 
    firstorder-analysis.cpp)
target_link_libraries(firstorder sparta)
include(project_props)
