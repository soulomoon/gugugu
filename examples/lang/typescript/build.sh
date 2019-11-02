#!/bin/bash

set -e

THIS_FILE=${BASH_SOURCE[0]}
if echo $THIS_FILE | grep -q -e "^[^/]"
then
	THIS_FILE="$PWD/$THIS_FILE"
fi

THIS_DIR=$(dirname $THIS_FILE)
cd $THIS_DIR

echo "Building example for typescript"

echo "Using Node at $(which node):"
node --version
echo "Using NPM at $(which npm):"
npm --version

bash run-gugugu.sh
npm install
npm run build
