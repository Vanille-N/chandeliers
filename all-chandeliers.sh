#! /usr/bin/env bash

CRATES=( \
    chandeliers-sem \
    chandeliers-std \
    chandeliers-err \
    chandeliers-san \
    chandeliers-syn \
    chandeliers-lus \
)

each() {
    for crate in ${CRATES[@]}; do
        ( cd $crate && echo ">>> Submodule '$crate'" && $@ )
    done
}

cargo-test() {
    cargo test
}

cargo-update() {
    cargo update
}

version-bump() {
    sed -Ei 's/^version = ".*"$/version = "'"$1"'"/' Cargo.toml
    sed -Ei 's/^(chandeliers-.*)version = ".*"(.*)/\1version = "'"$1"'"\2/' Cargo.toml
}

cargo-publish() {
    cargo publish
}

main() {
    case "$1" in
        ("test") each cargo-test;;
        ("update") each cargo-update;;
        ("publish") each cargo-publish;;
        ("bump") each version-bump "$2";;
        ("help"|*)
            echo "Submodule manager for the Chandeliers project"
            echo "Author: Neven <vanille@crans.org>"
            echo ""
            echo "Usage: $0 CMD [ARGS]"
            echo "with CMD among"
            echo "    test           execute 'cargo test' in each submodule"
            echo "    update         execute 'cargo update' in each submodule"
            echo "    bump x.y.z     bump all crates chandeliers-* to version x.y.z"
            echo "    publish        publish all to crates.io"
    esac
}

main "$@"

