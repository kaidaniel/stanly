#include <memory>
#include <string_view>

namespace stanly::parser {
namespace treesitter {
  class Parser;
}
using Parser = treesitter::Parser;
std::unique_ptr<Parser> parse_firstorder(std::string_view program);
} // namespace stanly::parser
