# TODO configure in 'ctest -S build.make' (which can live in a project-agnostic location)
# adding e.g. valgrind, ubsan etc.
add_executable(test-firstorder test-firstorder.cpp)
target_link_libraries(test-firstorder PRIVATE firstorder Catch2::Catch2WithMain)
include(Catch)
catch_discover_tests(test-firstorder EXTRA_ARGS --colour-mode ansi)