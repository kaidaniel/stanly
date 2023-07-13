[[ $1 ]] && nodes_json="$1" || nodes_json="build-default/tree-sitter-python/src/node-types.json"
[[ $2 ]] && lookup_symbols="$2" || lookup_symbols="build-default/src/lookup-symbols"

function preamble {
[[ ${1##*.} == "hpp" ]] && echo "# pragma once" || echo "#include \"$header_file\""

cat << EOF

// generated using "generate_parser_symbols.sh"
// nodes_json="$nodes_json"
// lookup_symbols="$lookup_symbols"

namespace stanly::$(echo $(basename ${1%.*}) | tr - _) {

EOF
}

function postamble {
echo "}"
clang-format -i $1
sed -i '/{}/d' $1
clang++ -fsyntax-only $1 -Wall -Werror
clang-tidy $1
}

non_terminals=$(jq -r -c '.[] | select(has("children") or (has("fields") and (.fields | length !=0)) or has("subtypes"))' < $nodes_json)

# ------ generate src/parser.hpp -------- #
header_file="parser.hpp"
exec > src/$header_file
preamble "src/$header_file"

echo $(date +'%D %H:%M:%S') symbols terminals fields > /dev/tty

cat << EOF
enum class terminals;
struct parser;
void parse(parser&, enum terminals);

enum class symbols {
$(< $nodes_json jq -r '.[] | select(.named).type'| $lookup_symbols)};

enum class terminals {
$(< $nodes_json jq -r '.[] | select(.named and .children == null and .subtypes == null and ((.fields == null) or (.fields | length == 0))).type'| $lookup_symbols)};

enum class fields {
$(< $nodes_json jq -r ".[] | select(.named and has(\"fields\")) | select(.fields | length != 0).fields | keys[]" | $lookup_symbols fields)};
EOF

for symbol in $non_terminals; do
    name=$( <<< $symbol jq -r '.type') 
    echo $(date +'%D %H:%M:%S') $name > /dev/tty
    subtypes="$( <<< $symbol jq -r '.subtypes[]?.type'| $lookup_symbol)"
    children="$( <<< $symbol jq -r '.children.types[]?.type'| $lookup_symbol)"
    fields="$( <<< $symbol jq -r 'select(.fields | length != 0).fields')"
    field_names=$(<<< $fields jq -r 'keys[]')
    echo -e "\n namespace $name { void parse(parser&);"
    [[ "$subtypes" ]] && echo "enum class subtypes { $subtypes };"
    [[ "$children" ]] && echo "enum class children { $children };"
    if [[ "$fields" ]]; then
        echo "namespace fields  { enum class field_names { $(<<< $field_names $lookup_symbols fields) };"
        for field in $field_names; do 
            echo "enum class $field { $(<<< $fields jq -r ".$field.types[].type" | $lookup_symbols) };"
        done
        echo }
    fi
    echo -e "}\n"
done
postamble "src/$header_file"

# preamble "src/${header_file%.*}.cpp"

# postamble "src/${header_file%.*}.cpp"


# # ------ generate src/parser.cpp -------- #

# preamble "${header_file%.*}.cpp"

# echo "void f();"

# echo "// clang-format off"
# for non_terminal in $non_terminals; do
#     name=$(echo $non_terminal | jq -r '.type')
#     echo "namespace $name { void parse(parser& p); }"
# done
# echo "// clang-format on"


# postamble