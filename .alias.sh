export cmake_preset="default"  # or: RelWithDebInfo
alias cf="clang-format -i src/*.cpp src/*.h"
alias ts="ctest --preset $cmake_preset"
alias wf="CLICOLOR_FORCE=1 cmake --workflow --preset $cmake_preset 2>&1 | bat --pager \"less +F -R\""
alias bd="CLICOLOR_FORCE=1 cmake --build $cmake_preset 2>&1 | bat --pager \"less +F -R\""
# updatedb
# locate
# /home/kai/.config/Code/User/globalStorage/llvm-vs-code-extensions.vscode-clangd/install/16.0.2/clangd_16.0.2/bin/clangd