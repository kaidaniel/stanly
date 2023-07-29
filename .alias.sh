shopt -s nullglob  # if no .h files are in src/, src/*.h will stay a literal string, crashing clang-format.
shopt -s globstar
export cmake_preset="default"  # or: RelWithDebInfo
alias cf="clang-format -i {src,test,include}/**/*.{cpp,h}"
alias ts="ctest --preset $cmake_preset"
alias wf="CLICOLOR_FORCE=1 cmake --workflow --preset $cmake_preset 2>&1 | bat --pager \"less +F -R\""
alias bd="CLICOLOR_FORCE=1 cmake --build $cmake_preset 2>&1 | bat --pager \"less +F -R\""
# updatedb
# locate
# /home/kai/.config/Code/User/globalStorage/llvm-vs-code-extensions.vscode-clangd/install/16.0.2/clangd_16.0.2/bin/clangd