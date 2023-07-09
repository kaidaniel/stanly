#include <cxxopts.hpp>
#include <format>
#include <fstream>
#include <iostream>
#include <ranges>
#include <string>

#include "analyse.h"
#include "parse.h"

using namespace stanly;

std::string read_file(const std::string& filename) {
  std::ifstream file{filename};
  if (file) { return {std::istreambuf_iterator<char>{file}, std::istreambuf_iterator<char>{}}; }
  std::cerr << "Error opening file: " << filename << "\n";
  exit(1);
}

template <class T>
void print(std::format_string<const T&> fmt, const T& t) {
  std::cout << std::format(fmt, t);
}

template <class T>
void print(const T& t) {
  return print("{}\n", t);
}
template <>
void print(const std::vector<syntax::ast_node>& t) {
  for (const auto& n : t) { print(resolve_handles(n)); }
}
template <>
void print(const domain& t) {
  print(with_handles{t});
}

int main(int argc, char* argv[]) {
  cxxopts::Options options("stanly",
                           "Statically analyse dynamic records (dictionaries, dataframes, ...).");
  options.add_options()("c,command", "Program read from a string instead of a file")(
      "v,verbose", "Verbose output", cxxopts::value<bool>()->default_value("false"))(
      "h,help", "Show usage")("p,parse", "Only parse, don't analyse (printing IR)")(
      "program", "File or string containing a python program", cxxopts::value<std::string>());
  options.parse_positional({"program"});
  auto result = options.parse(argc, argv);
  if (result.count("help")) {
    std::cout << options.help() << "\n";
    return 1;
  }

  const auto& program = result["program"].as<std::string>();
  auto ast = parse(result.count("c") ? std::string{program} : read_file(program));

  result.count("p") ? print(ast) : print(analyse(ast));

  return 0;
}
