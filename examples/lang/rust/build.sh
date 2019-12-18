#!/bin/bash

set -e

THIS_FILE=${BASH_SOURCE[0]}
if echo $THIS_FILE | grep -q -e "^[^/]"
then
	THIS_FILE="$PWD/$THIS_FILE"
fi

THIS_DIR=$(dirname $THIS_FILE)
cd $THIS_DIR

echo "Building example for rust"

echo "Using cargo at $(which cargo):"
cargo --version

bash run-gugugu.sh

cargo build
