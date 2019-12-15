#!/bin/bash

set -e

THIS_FILE=${BASH_SOURCE[0]}
if echo $THIS_FILE | grep -q -e "^[^/]"
then
	THIS_FILE="$PWD/$THIS_FILE"
fi

THIS_DIR=$(dirname $THIS_FILE)
EXAMPLE_DIR=$(dirname $(dirname $THIS_DIR))
SRC_OUTPUT=$THIS_DIR/build/generated/gugugu

rm -rf $SRC_OUTPUT

gugugu-haskell \
	--input=$EXAMPLE_DIR/gugugu \
	--output=$SRC_OUTPUT \
	--module-prefix=GuguguExamples.Definitions \
	--derivings=Show \
	--with-codec \
	--with-server \
	--with-client \
	;
