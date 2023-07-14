#include <iostream>
#include <map>
#include <set>
#include <string>

#include "tree_sitter/api.h"

extern "C" {
TSLanguage*
tree_sitter_python(void);
}
const auto* const py = tree_sitter_python();

int
main(int argc, const char* argv[]) {
  std::map<int, std::string> names;
  std::string line;
  while (std::getline(std::cin, line)) {
    unsigned short sym{};
    if (argc > 1 && std::string_view{argv[1]} == "fields") {
      sym = ts_language_field_id_for_name(py, line.data(), line.length());
    } else {
      sym = ts_language_symbol_for_name(py, line.data(), line.length(), true);
    }
    names[sym] = std::move(line);
  }
  for (const auto& [sym, name] : names) {
    if (name.empty() || (sym == 0U)) { continue; }
    std::cout << "sym_" + name << " = " << sym << ",\n";
  }
}