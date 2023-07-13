[[ $1 ]] && nodes_json="$1" || nodes_json="build-default/tree-sitter-python/src/node-types.json"
[[ $2 ]] && lookup_symbols="$2" || lookup_symbols="build-default/src/lookup-symbols"

function preamble {
[[ $out_file ]] && { echo "ERROR: out_file set. Did you forget to call 'postamble' after 'preamble'?" > /dev/tty; exit 1; }
out_file="src/$1"
echo $(date +'%D %H:%M:%S') $out_file > /dev/tty
exec > $out_file
[[ ${out_file##*.} == "hpp" ]] && echo "# pragma once" || echo "#include \"$header_file\""
cat << EOF

// generated using "generate_parser_symbols.sh"
// nodes_json="$nodes_json"
// lookup_symbols="$lookup_symbols"

namespace stanly::$(echo $(basename ${out_file%.*}) | tr - _) {

EOF
}

function postamble {
echo "}"
clang-format -i $out_file
sed -i '/{}/d' $out_file
clang++ -fsyntax-only $out_file -Wall -Werror
clang-tidy $out_file
unset out_file
}

# ------ generate src/parser.hpp -------- #
header_file="parser.hpp"
preamble "$header_file"

cat << EOF
enum class terminals;
struct parser;
void parse(parser&, enum terminals);

enum class symbols {
$(jq -r '.[] | select(.named).type' < $nodes_json | $lookup_symbols)};

enum class terminals {
$(jq -r '.[] | select(.named and .children == null and .subtypes == null and ((.fields == null) or (.fields | length == 0))).type' < $nodes_json | $lookup_symbols)};

enum class fields {
$(jq -r ".[] | select(.named and has(\"fields\")) | select(.fields | length != 0).fields | keys[]" < $nodes_json | $lookup_symbols fields)};
EOF

for symbol in $(jq -r '.[] | select(has("subtypes")).type' < $nodes_json ); do
cat << EOF
    namespace $symbol { enum class subtypes {
    $(jq -r ".[] | select(.named and .type==\"$symbol\").subtypes[].type" < $nodes_json | $lookup_symbols)};}
EOF
done


# enum class ${symbol}::children
for symbol in $(jq -r '.[] | select(.named and has("children")).type' < $nodes_json); do
cat << EOF
    namespace $symbol { enum class children {
    $(jq -r ".[] | select(.named and .type==\"$symbol\").children[\"types\"][].type" < $nodes_json | $lookup_symbols) };}
EOF
done


# enum class ${symbol}::field_names
for symbol in $(jq -r '.[] | select(.named and (.fields | length != 0)).type' < $nodes_json); do
cat << EOF
    namespace $symbol { enum class field_names {
    $(jq -r ".[] | select(.named and .type==\"$symbol\").fields | keys[]" < $nodes_json | $lookup_symbols fields)};}
EOF
    # enum class ${symbol}::fields::${field}
    for field in $(jq -r ".[] | select(.named and .type==\"$symbol\").fields | keys[]" < $nodes_json); do
cat << EOF
        namespace $symbol::fields { enum class $field {
        $(jq -r ".[] | select(.named and .type==\"$symbol\").fields.$field.types[].type" < $nodes_json | $lookup_symbols)};}
EOF
    done
done

postamble


# ------ generate src/parser.cpp -------- #

preamble "${header_file%.*}.cpp"

echo "void f();"

non_terminals=$(jq -r -c '.[] | select(has("children") or (has("fields") and (.fields | length !=0)) or has("subtypes"))' < $nodes_json)
echo "// clang-format off"
for non_terminal in $non_terminals; do
    name=$(echo $non_terminal | jq -r '.type')
    echo "namespace $name { void parse(parser& p); }"
done
echo "// clang-format on"


postamble