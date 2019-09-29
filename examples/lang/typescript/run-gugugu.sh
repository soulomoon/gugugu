#!/bin/bash

THIS_DIR=$(dirname $PWD/${BASH_SOURCE[0]})
EXAMPLE_DIR=$(dirname $(dirname $THIS_DIR))
SRC_OUTPUT=$THIS_DIR/build/generated/gugugu

rm -rf $THIS_DIR/build/generated/gugugu

gugugu-typescript \
	--input=$EXAMPLE_DIR/gugugu \
	--output=$SRC_OUTPUT \
	--with-codec \
	--with-server \
	--package-prefix=guguguexamples/definitions \
	;
