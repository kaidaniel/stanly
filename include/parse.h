#include <memory>
#include <string>
#include <vector>

#include "string-index.h"
#include "syntax.h"

namespace stanly {
struct cfg {
  std::vector<basic_block> basic_blocks{};
  string_index idx{};
};

enum class lang { python };

template <lang>
struct lang_tag {};

std::unique_ptr<cfg> parse(std::string&&, lang_tag<lang::python>);
}  // namespace stanly