#!/bin/bash

THIS_DIR=$(dirname $PWD/${BASH_SOURCE[0]})
EXAMPLE_DIR=$(dirname $(dirname $THIS_DIR))
SRC_OUTPUT=$THIS_DIR/build/generated/gugugu/main/scala

rm -rf $THIS_DIR/build/generated/gugugu

gugugu-scala \
	--input=$EXAMPLE_DIR/gugugu \
	--output=$SRC_OUTPUT \
	--with-codec \
	--package-prefix=guguguexamples.definitions \
	;
