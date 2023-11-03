#! /usr/bin/env bash

SRC=( err lus san sem std syn )

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
UNLINTS=(
    'std|non_camel_case_types'
)

FEATS=(
    'all|lint_reasons'
    'lus|proc_macro_diagnostic'
)

mkdir -p .lint
for dir in ${SRC[@]}; do
    F=".lint/$dir.lint"
    awk '
        BEGIN { RUN=0 }
        /#!\[warn\(/ { RUN=1 }
        RUN==1 { print $1 }
        /)]/ { RUN=0 }
    ' chandeliers-$dir/src/lib.rs > $F.found
    sed -Ei 's/#!\[warn\(//' $F.found
    sed -Ei 's/\)\]//' $F.found
    sed -Ei 's/,//' $F.found
    sed -Ei '/^[[:space:]]*$/d' $F.found

    : > $F.expect
    for line in ${LINTS[@]}; do
        if [[ "$line" =~ all\|.* ]] || [[ "$line" =~ .*$dir.*\|.* ]]; then
            echo $line | cut -d'|' -f2 >> $F.expect
        fi
    done

    echo ""
    echo "Within $dir"
    diff $F.expect $F.found
done

for dir in ${SRC[@]}; do
    F=".lint/$dir.silence"
    awk '
        BEGIN { RUN=0 }
        /#!\[allow\(/ { RUN=1 }
        RUN==1 { print $1 }
        /)]/ { RUN=0 }
    ' chandeliers-$dir/src/lib.rs > $F.found
    sed -Ei 's/#!\[allow\(//' $F.found
    sed -Ei 's/\)\]//' $F.found
    sed -Ei 's/,//' $F.found
    sed -Ei '/^[[:space:]]*$/d' $F.found

    : > $F.expect
    for line in ${UNLINTS[@]}; do
        if [[ "$line" =~ all\|.* ]] || [[ "$line" =~ .*$dir.*\|.* ]]; then
            echo $line | cut -d'|' -f2 >> $F.expect
        fi
    done

    echo ""
    echo "Within $dir"
    diff $F.expect $F.found
done

for dir in ${SRC[@]}; do
    F=".lint/$dir.feat"
    awk '
        BEGIN { RUN=0 }
        /#!\[feature\(/ { RUN=1 }
        RUN==1 { print $1 }
        /)]/ { RUN=0 }
    ' chandeliers-$dir/src/lib.rs > $F.found
    sed -Ei 's/#!\[feature\(//' $F.found
    sed -Ei 's/\)\]//' $F.found
    sed -Ei 's/,//' $F.found
    sed -Ei '/^[[:space:]]*$/d' $F.found

    : > $F.expect
    for line in ${FEATS[@]}; do
        if [[ "$line" =~ all\|.* ]] || [[ "$line" =~ .*$dir.*\|.* ]]; then
            echo $line | cut -d'|' -f2 >> $F.expect
        fi
    done

    echo ""
    echo "Within $dir"
    diff $F.expect $F.found
done

