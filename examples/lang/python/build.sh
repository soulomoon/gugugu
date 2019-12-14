#!/bin/bash

# WARNING: This script expects Python 3.7+ available as `python` instead of
# `python3`

set -e

THIS_FILE=${BASH_SOURCE[0]}
if echo $THIS_FILE | grep -q -e "^[^/]"
then
	THIS_FILE="$PWD/$THIS_FILE"
fi

THIS_DIR=$(dirname $THIS_FILE)
cd $THIS_DIR

echo "Building example for python"

echo "Using python at $(which python):"
python --version

bash run-gugugu.sh

pip install --upgrade .
