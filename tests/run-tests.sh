#!/bin/sh

cd "$(dirname "$0")"
set -e


elm-package install -y

CORE_VERSION_DIR="$(ls elm-stuff/packages/elm-lang/core/)"
CORE_PACKAGE_DIR="elm-stuff/packages/elm-lang/core/$CORE_VERSION_DIR"
CORE_GIT_DIR="$(dirname $PWD)"

echo "Linking $CORE_PACKAGE_DIR to $CORE_GIT_DIR"
rm -rf $CORE_PACKAGE_DIR
ln -s $CORE_GIT_DIR $CORE_PACKAGE_DIR

LAZY_VERSION_DIR="$(ls elm-stuff/packages/elm-lang/lazy/)"
LAZY_PACKAGE_DIR="elm-stuff/packages/elm-lang/lazy/$LAZY_VERSION_DIR"
echo "-module('Lazy')." > "$LAZY_PACKAGE_DIR/src/Native/Lazy.erl"

elm beam Main.elm
erl -noshell -s elm main -s init stop
