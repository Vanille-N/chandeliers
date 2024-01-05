#! /usr/bin/env bash

echo "
digraph {
    San -> {Sem Err};
    Lus -> {Syn San Sem Err};
    Std -> Sem;
    Syn -> {Err San};
    You -> {Lus Sem Std};
}" | dot -Tpng -o $1.png
