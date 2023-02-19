include(FetchContent)
FetchContent_Declare(
    sparta_extern
    GIT_REPOSITORY git@github.com:facebook/SPARTA.git
    GIT_TAG 21d24089a7b2bf2745a8f27903a2e6a71a64d2e1
    SOURCE_DIR        "${CMAKE_BINARY_DIR}/sparta-src"
)
FetchContent_Populate(sparta_extern)
add_library(sparta INTERFACE)
target_include_directories(sparta SYSTEM INTERFACE ${CMAKE_BINARY_DIR}/sparta-src/include)
