#! /usr/bin/env bash
#
# A meta lint manager for Chandeliers.
#
# Because Chandeliers is split across many crates, it can be tricky to
# keep track of the clippy flags and the unstable features.
# This scripts does not itself manage the configuration, but it can to
# an extent verify that it matches what is declared here.

# Values that we expect to find inside `#![warn(...)]`
LINTS=(
    'all|missing_docs'
    'all|unused_crate_dependencies'
    'all|unused_macro_rules'
    'all|variant_size_differences'
    'all|clippy::allow_attributes'
    'all|clippy::allow_attributes_without_reason'
    'all|clippy::expect_used'
    'all|clippy::indexing_slicing'
    'all|clippy::missing_docs_in_private_items'
    'sem|clippy::missing_inline_in_public_items'
    'all|clippy::multiple_inherent_impl'
    'all|clippy::panic'
    'all|clippy::pedantic'
    'all|clippy::str_to_string'
    'all|clippy::unreachable'
    'all|clippy::unwrap_used'
    'all|clippy::use_debug'
)

# Values that we expect to find inside `#![allow(...)]`
UNLINTS=(
    'std|non_camel_case_types'
)

# Values that we expect to find inside `#![feature(...)]`
FEATS=(
    'all|lint_reasons'
    'lus|proc_macro_diagnostic'
)

compare_with() {
    echo "Checking $MODE for $ID"
    mkdir -p .lint
    local F=".lint/$ID.$MODE"

    # Options found in the source code.
    #
    awk '
        BEGIN { RUN=0 }
        /#!\['$MODE'\(/ { RUN=1 }
        RUN==1 { print $1 }
        /)]/ { RUN=0 }
    ' $DIR/src/lib.rs > $F.found
    sed -Ei 's/#!\['$MODE'\(//' $F.found
    sed -Ei 's/\)\]//' $F.found
    sed -Ei 's/,//' $F.found
    sed -Ei '/^[[:space:]]*$/d' $F.found

    # Options that we expected.
    # The lists of options use the '<pat>|<flag>' syntax, where
    # '<flag>' will be selected when either '<pat>' is "all" or
    # '<pat>' matches the "$1" passed to the script (set as `ID`).
    : > $F.expect
    for line in "$@"; do
        if [[ "$line" =~ all\|.* ]] || [[ "$line" =~ .*$ID.*\|.* ]]; then
            echo $line | cut -d'|' -f2 >> $F.expect
        fi
    done

    diff $F.expect $F.found
}

compare_all() {
    MODE='warn' compare_with "${LINTS[@]}"
    MODE='feature' compare_with "${FEATS[@]}"
    MODE='allow' compare_with "${UNLINTS[@]}"
}

# Expected arguments:
# - "$1" the identifier of the subcrate (`err`, `san`, `std`, `syn`, `lus`)
# - "$2" the directory that it is found in.
ID=$1 DIR=$2 compare_all


