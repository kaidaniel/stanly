line_memory: str = ""

def process(line: str) -> str:
    global line_memory
    last_line = line_memory 
    line_memory = line
    line_ = line.replace("/home/kai/projects/stanly", ".")
    line_ = line_.replace("build-default", "b")
    line_ = line_.replace("/usr/local/bin/../include/c++/v1/__format/", "libc++·")
    line_ = line_.replace("/usr/local/bin/../include/c++/v1/", "libc++·")
    if "FAILED" in last_line:  # compiler invocation
        return line_.replace("  ", " ").replace(" ", " \\\n") 
    if "note:" in line or "error:" in line:
        for pattern, replacement in replacements:
            line_ = line_.replace(pattern, replacement)
#    if "note:" in line or "error:" in line:
#        line_ = line_.replace("'", "\x1b[36;23m'", 1) + "\x1b[0m"
    return line_ + "\n"

replacements = [
    ("std::variant<stanly::firstorder::syntax<std::string_view>::set_field, stanly::firstorder::syntax<std::string_view>::load_field, stanly::firstorder::syntax<std::string_view>::load_text, stanly::firstorder::syntax<std::string_view>::load_record, stanly::firstorder::syntax<std::string_view>::load_var, stanly::firstorder::syntax<std::string_view>::load_top>", "node"),
    ("stanly::firstorder::syntax<std::string_view>::", ""),
    ("stanly::firstorder::syntax<std::string_view>", "syntax"),
    ("vector", "vec"),
    ("string_view", "sv"),
    ("string", "str"),
    ("std::", "'"),
    ("typename ", "٭"),
    ("::type", "٭"),
    ("(anonymous class)", "(?)"),
    ("const ", "κ'"), 
    ("lambda at", "λ"),
    #("::", "·"),
    ("operator", "op'"),
    ("in instantiation of", "in"),
    ("function template specialization", "fn template"),
    ("requested here", ""),
    ("in instantiation of member function", "member fn"),
    ("static assertion failed due to requirement ", "")
]

try:
    while line := input():
        print(process(line), end="", flush=True)
except EOFError:
    raise SystemExit()
