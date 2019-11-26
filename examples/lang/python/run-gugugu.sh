#!/bin/bash

set -e

THIS_FILE=${BASH_SOURCE[0]}
if echo $THIS_FILE | grep -q -e "^[^/]"
then
	THIS_FILE="$PWD/$THIS_FILE"
fi

THIS_DIR=$(dirname $THIS_FILE)
EXAMPLE_DIR=$(dirname $(dirname $THIS_DIR))
SRC_OUTPUT=$THIS_DIR/gugugu-generated

rm -rf $SRC_OUTPUT

gugugu-python \
	--input=$EXAMPLE_DIR/gugugu \
	--output=$SRC_OUTPUT \
	--package-prefix=guguguexamples.definitions \
	--with-codec \
	--with-server \
	;
