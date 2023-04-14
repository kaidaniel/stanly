#set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE "include-what-you-use;-xiwyu;/usr/local/include/c++/v1/libcxx.imp")
#set(CMAKE_CXX_COMPILER "/usr/local/bin/clang++")
set(CMAKE_CXX_COMPILER "/home/kai/projects/install/bin/clang++")
#set(CMAKE_C_COMPILER "/usr/local/bin/clang")
set(CMAKE_C_COMPILER "/home/kai/projects/install/bin/clang")
set(CMAKE_CXX_FLAGS "-stdlib=libc++ -fexperimental-library") # std::format support
#set(CMAKE_EXE_LINKER_FLAGS "-Wl,-rpath=/usr/local/lib/x86_64-unknown-linux-gnu -L /usr/local/lib/x86_64-unknown-linux-gnu") # x86_64-unknown-linux-gnu not in /etc/ld.so.conf, but its where llvm installed libc++.so.1 etc.
set(CMAKE_EXE_LINKER_FLAGS "-Wl,-rpath=/home/kai/projects/install/lib/x86_64-unknown-linux-gnu -L /home/kai/projects/install/lib/x86_64-unknown-linux-gnu") # libc++ version with unstable ABI
