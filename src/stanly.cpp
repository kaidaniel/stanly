#include <__format/format_functions.h>

#include <cstdlib>
#include <cxxopts.hpp>
#include <format>
#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

#include "analyse.h"
#include "domain.h"
#include "parse.h"
#include "string-index.h"
#include "syntax.h"

using namespace stanly;

std::string
read_file(const std::string& filename) {
  std::ifstream file{filename};
  if (file) { return {std::istreambuf_iterator<char>{file}, std::istreambuf_iterator<char>{}}; }
  std::cerr << "Error opening file: " << filename << "\n";
  std::exit(1);
}

template <class T>
void
print(std::format_string<const T&> fmt, const T& t) {
  std::cout << std::format(fmt, t);
}

template <class T>
void
print(const T& t) {
  return print("{}\n", t);
}
template <>
void
print(const std::vector<node>& t) {
  for (const auto& n : t) { print(resolve_handles(n)); }
}
template <>
void
print(const state& t) {
  print(with_handles{t});
}

int
main(int argc, char* argv[]) {
  // clang-format off
  cxxopts::Options options(
    "stanly", "Statically analyse dynamic records (dictionaries, dataframes, ...).");
  options.add_options()
    ("c,command", "Program read from a string instead of a file")
    ("h,help", "Show usage")
    ("p,parse", "Only parse, don't analyse (printing IR)")
    ("program", "File or string containing a python program", cxxopts::value<std::string>())
    ;
  // clang-format on
  options.parse_positional({"program"});
  auto result = options.parse(argc, argv);
  if (result.count("help") != 0U) {
    std::cout << options.help() << "\n";
    return 1;
  }

  const auto& program = result["program"].as<std::string>();
  auto ast = parse(result.count("c") != 0U ? std::string{program} : read_file(program));
  result.count("p") != 0U ? print(ast) : print(analyse(ast));

  return 0;
}
