# TODO: get and build SPARTA from github instead
# e.g. by writing a find module Findsparta.cmake using include(FetchContent)?
# find_package(sparta REQUIRED)
add_library(sparta INTERFACE)
target_include_directories(sparta SYSTEM INTERFACE /usr/local/include/sparta)