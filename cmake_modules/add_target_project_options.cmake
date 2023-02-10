add_library(project_options INTERFACE)
target_compile_options(project_options INTERFACE -Wall -Wextra)
set_target_properties(project_options PROPERTIES 
    CXX_CLANG_TIDY clang-tidy
    CXX_INCLUDE_WHAT_YOU_USE iwyu
    CXX_LINK_WHAT_YOU_USE TRUE)

# TODO: add_custom_target to run sanitizers