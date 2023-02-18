# TODO configure in 'ctest -S build.make' (which can live in a project-agnostic location)
# adding e.g. valgrind, ubsan etc.
enable_testing()
add_executable(firstorder_test firstorder_test.cpp)
target_link_libraries(firstorder_test PRIVATE firstorder Catch2::Catch2WithMain)
include(Catch)
catch_discover_tests(firstorder_test)