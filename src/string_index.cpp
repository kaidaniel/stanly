#include "string_index.h"

namespace stanly {
string_index global_string_index{};
handle operator""_h(const char* str, std::size_t size) {
  return global_string_index.insert(std::string_view{str, size});
}
}  // namespace stanly