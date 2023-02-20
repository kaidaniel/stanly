#pragma once

#include "stanly-api.h"

namespace stanly {
GraphType parse_firstorder(const std::string &program);

using Parser = GraphType (*)(const std::string &);
consteval Parser make_parser(std::string_view language) {
  if (language == "firstorder") { return parse_firstorder; };
  throw "parser not defined for this language";
}
} // namespace stanly
