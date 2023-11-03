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
    for crate in "${CRATES[@]}"; do
        ( cd $crate && echo ">>> Submodule '$crate'" && $@ ) || [ -z STRICT ] || exit 100
    done
}

version-bump() {
    sed -Ei 's/^version = ".*"$/version = "'"$1"'"/' Cargo.toml
    sed -Ei 's/^(chandeliers-.*)version = ".*"(.*)/\1version = "'"$1"'"\2/' Cargo.toml
}

do-cargo() {
    cargo "$@" || [ -z STRICT ] || exit 100
}

main() {
    case "$1" in
        ("check"|"build"|"test")
            STRICT=1 each do-cargo "$@"
            STRICT=1 do-cargo "$@"
            ;;
        ("update"|"clippy"|"fmt")
            each do-cargo "$@"
            do-cargo "$@"
            ;;
        ("publish")
            each do-cargo "$@"
            ;;
        ("bump")
            each version-bump "$2"
            version-bump "$2"
            ;;
        ("bless") shift; TRYBUILD=overwrite IN=( ${CRATES[@]} . ) each do-cargo test "$@";;
        ("lint")
            for crate in "${CRATES[@]}"; do
                ./lint.sh ${crate#*-} $crate
            done
            ;;
        ("help"|*)
            echo "Submodule manager for the Chandeliers project"
            echo "Author: Neven <vanille@crans.org>"
            echo ""
            echo "Usage: $0 CMD [ARGS]"
            echo "with CMD among"
            echo "    bump x.y.z                   bump all crates chandeliers-* to version x.y.z"
            echo "    test, update, publish,...    passed directly to cargo"
            echo "    bless                        blesses the output of 'cargo test'"
    esac
}

main "$@"

