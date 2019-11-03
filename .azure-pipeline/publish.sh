#!/bin/bash

set -e

ARCHIVE_NAME=gugugu-$GUGUGU_VERSION-$OS_ARCH
PREPARING_DIR=build/preparing/$ARCHIVE_NAME
mkdir -p build/preparing/$ARCHIVE_NAME
stack $STACK_ARGS --local-bin-path=$PREPARING_DIR install

# Show dependencies
case "$AGENT_OS" in
	Linux)
		LS_DEPS="ldd"
		;;
	Darwin)
		LS_DEPS="otool -L"
		;;
	Windows_NT)
		VS_DIR="C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Enterprise"
		function list_dependencies()
		{
			local operand=$(cygpath --windows "$1")
			echo "CALL \"$VS_DIR\\VC\\Auxiliary\\Build\\vcvars64.bat\" && dumpbin /DEPENDENTS \"$operand\"" | cmd
		}
		LS_DEPS="list_dependencies"
		;;
esac
for EXE_FILE in $(ls build/preparing/$ARCHIVE_NAME/*)
do
	echo "Dependencies of $EXE_FILE:"
	$LS_DEPS "$EXE_FILE"
done

cp core/LICENSE $PREPARING_DIR/LICENSE
mkdir -p build/release
tar -czf build/release/$ARCHIVE_NAME.tar.gz -C build/preparing $ARCHIVE_NAME
