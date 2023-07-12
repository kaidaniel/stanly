if [[ $1 ]]; then
    nodes_json="$1"
else
    nodes_json="build-default/tree-sitter-python/src/node-types.json"
fi

if [[ $2 ]]; then
    generate_code="$2"
else
    lookup_symbols="build-default/src/lookup-symbols"
fi

function preamble {
cat << EOF
#pragma once

// generated using "generate_symbol_tables.sh"
// nodes_json="$nodes_json"
// lookup_symbols="$lookup_symbols"

namespace stanly {

EOF
}

function format {
clang-format -i $1
sed -i '/{}/d' $1
}

# ------ generate src/symbol-tables.h -------- #
exec > "src/symbols.h"
preamble

for non_terminal in $(jq -r '.[] | select(.named) | select((.fields | length !=0) or has("children")) | .type' < $nodes_json ); do
    echo "enum class $non_terminal {"
    jq -r ".[] | select(.named and .type == \"$non_terminal\") | .fields?[]?.types[]?.type, .children?[\"types\"][]?.type" < $nodes_json | $lookup_symbols
    echo "};"
done
for supertype in $(jq -r '.[] | select(has("subtypes")) | .type' < $nodes_json ); do
    echo "enum class $supertype {"
    jq -r ".[] | select(.named and .type==\"$supertype\") | .subtypes[].type" < $nodes_json | $lookup_symbols
    echo "};"
done
echo "enum class symbols {"
jq -r '.[] | select(.named) | .type' < $nodes_json | $lookup_symbols
echo "};"

echo "}"
format "src/symbols.h"

# ------ generate src/field-names.h -------- #
exec > "src/fields.h"
preamble

echo "enum class fields {"
jq -r ".[] | select(.named == true and has(\"fields\")) | select(.fields | length != 0) | .fields | keys[]" < $nodes_json | $lookup_symbols fields
echo "};"
echo "}"

format "src/fields.h"




# grep -v '{}' > "src/symbol-tables.h"
# clang-format "src/symbol-tables.h" | grep -v '{}' > 'src/symbol-tables.h';
# > resources/symbols/field_names < $nodes_json jq -r ".[] | select(.named == true and has(\"fields\")) | select(.fields | length != 0) | .fields | keys[]"
# > resources/symbols/supertypes < $nodes_json jq -r '.[] | select(has("subtypes")) | .type'
# > resources/symbols/all_types < $nodes_json jq -r '.[] | select(.named) | .type'