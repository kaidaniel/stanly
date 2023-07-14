[[ $1 ]] && nodes_json="$1" || nodes_json="build-default/tree-sitter-python/src/node-types.json"
[[ $2 ]] && lookup_symbols="$2" || lookup_symbols="build-default/src/lookup-symbols"

#non_terminals=$(jq -r -c '.[] | select(has("children") or (has("fields") and (.fields | length !=0)) or has("subtypes"))' < $nodes_json)

non_terminals=$(jq -r -c '.[] | select(.named) | select(has("children") or (has("fields") and (.fields | length !=0)) or has("subtypes")) | {type: .type, subtypes: [.subtypes[]?.type], children: [.children.types[]?.type], fields: [select(.fields | length !=0).fields], field_names: [select(.fields | length !=0).fields | keys[]]}' < $nodes_json)
terminals=$(< $nodes_json jq -r '.[] | select(.named and .children == null and .subtypes == null and ((.fields == null) or (.fields | length == 0))).type')
echo $(date +'%D %H:%M:%S') symbols terminals fields > /dev/tty

cat << EOF > src/parser.cpp
#include "parser.hpp"
#include <optional>
#include <string_view>

// generated using "generate_parser_symbols.sh"
// nodes_json="$nodes_json"
// lookup_symbols="$lookup_symbols"

namespace stanly::parser {

    void f();

EOF

cat << EOF > src/parser.hpp
# pragma once

// generated using "generate_parser_symbols.sh"
// nodes_json="$nodes_json"
// lookup_symbols="$lookup_symbols"

namespace stanly::parser {

    enum class terminals;
    enum class fields;
    enum class symbols;
    struct parser;

    void parse(parser&, terminals);
    std::optional<std::string_view> parse_field(parser&, enum fields);
    std::optional<std::string_view> parse_children(parser&);

    enum class symbols {
    $(< $nodes_json jq -r '.[] | select(.named).type'| $lookup_symbols)};

    enum class terminals {
    $( $lookup_symbols <<< $terminals)};

    enum class fields {
    $(< $nodes_json jq -r ".[] | select(.named and has(\"fields\")) | select(.fields | length != 0).fields | keys[]" | $lookup_symbols fields)};

    $(
    for symbol in $non_terminals; do
        name="sym_"$( <<< $symbol jq -r '.type') 
        echo $(date +'%D %H:%M:%S') $name > /dev/tty
        subtypes=$(<<< $symbol jq -r '.subtypes[]')
        children=$(<<< $symbol jq -r '.children[]')
        fields=$(<<< $symbol jq -r -c '.fields[]')
        field_names=$(<<< $symbol jq -r -c '.field_names[]')
cat << HEADER
        namespace $name { 
            void parse(parser&);
            enum class subtypes { $(<<< $subtypes $lookup_symbols) };
            enum class children { $(<<< $children $lookup_symbols) };
            namespace fields { 
                enum class field_names { $(<<< $field_names $lookup_symbols fields) };
                $( [[ $field_names ]] && while read f; do echo "enum class $f { $(jq -r ".$f.types[]?.type" <<< $fields | $lookup_symbols) };"; done <<< "$field_names" )
            }
        }    
HEADER

cat << SOURCE >> src/parser.cpp
        namespace $name {
            void parse(parser& p){ 
                auto children = parse_children(p);
                $( [[ $field_names ]] && while read f; do echo "auto $f = parse_field(p, fields::field_names::$f);"; done <<< "$field_names")              
            }
        }
SOURCE
    
    done
    )
}
EOF

function lint {
    sed -i '/{}/d' $1
    clang-format -i $1
    sed -i '/{}/d' $1
    clang++ -fsyntax-only $1 -Wall -Werror
    clang-tidy $1
}
echo "}" >> src/parser.cpp
lint src/parser.hpp
lint src/parser.cpp

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