#set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE "include-what-you-use;-xiwyu;/usr/local/include/c++/v1/libcxx.imp")
set(CMAKE_CXX_COMPILER "/usr/local/bin/clang++")
set(CMAKE_C_COMPILER "/usr/local/bin/clang")
set(CMAKE_CXX_FLAGS "-stdlib=libc++ -fexperimental-library") # std::format support
