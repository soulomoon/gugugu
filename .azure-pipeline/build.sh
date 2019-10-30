#!/bin/bash

set -e

mkdir -p build
mkdir -p $UNIX_STACK_ROOT

if [ "$AGENT_OS" = Windows_NT ]
then
	export STACK_ARGS="--color=never"
	echo "local-programs-path: $STACK_ROOT\\programs" > $UNIX_STACK_ROOT/config.yaml
	echo "Using STACK_ARGS = $STACK_ARGS"
	echo "##vso[task.setvariable variable=STACK_ARGS]$STACK_ARGS"
fi

if [ "$CACHE_RESTORED_STACK_BIN" = true ]
then
	if echo $AGENT_OS | egrep -q -e "Linux|Darwin"
	then
		chmod -v +x $UNIX_STACK_ROOT/bin/stack
	fi
	echo "Restored stack executable"
else
	echo "Downloading stack executable"
	mkdir -p $UNIX_STACK_ROOT/bin
	mkdir -p /tmp/stack-root
	case "$AGENT_OS" in
		Linux)
			curl -L https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz \
				| tar -xz --strip-components=1 -C /tmp/stack-root
			mv /tmp/stack-root/stack $UNIX_STACK_ROOT/bin/stack
			;;
		Darwin)
			curl -L https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-osx-x86_64.tar.gz \
				| tar -xz --strip-components=1 -C /tmp/stack-root
			mv /tmp/stack-root/stack $UNIX_STACK_ROOT/bin/stack
			;;
		Windows_NT)
			curl -L -o /tmp/stack-$STACK_VERSION-windows-x86_64.tar.gz \
				https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-windows-x86_64.tar.gz
			tar -xzf /tmp/stack-$STACK_VERSION-windows-x86_64.tar.gz --strip-components=1 -C /tmp/stack-root
			mv /tmp/stack-root/stack.exe $UNIX_STACK_ROOT/bin/stack.exe
			;;
	esac
fi

if [ "$CACHE_RESTORED_STACK_PANTRY" = true ]
then
	echo "Restored stack package index"
else
	echo "Updating stack package index"
	stack $STACK_ARGS update
fi

if [ "$CACHE_RESTORED_STACK_PROGRAMS" = true ]
then
	case "$AGENT_OS" in
		Linux)
			find $UNIX_STACK_ROOT/programs | egrep -e '/bin/[^/]+$' | xargs chmod -v +x
			find $UNIX_STACK_ROOT/programs | egrep -e '\.so$' | xargs chmod -v +x
			;;
		Darwin)
			find $UNIX_STACK_ROOT/programs | egrep -e '/bin/[^/]+$' | xargs chmod -v +x
			find $UNIX_STACK_ROOT/programs | egrep -e '\.dylib$' | xargs chmod -v +x
			;;
	esac
	echo "Restored GHC"
else
	echo "Setting up GHC"
	stack $STACK_ARGS setup
fi

if [ "$CACHE_RESTORED_STACK_SNAPSHOTS" = true ]
then
	case "$AGENT_OS" in
		Linux)
			find $UNIX_STACK_ROOT/snapshots | egrep -e '/bin/[^/]+$' | xargs chmod -v +x
			find $UNIX_STACK_ROOT/snapshots | egrep -e '\.so$' | xargs chmod -v +x
			;;
		Darwin)
			find $UNIX_STACK_ROOT/snapshots | egrep -e '/bin/[^/]+$' | xargs chmod -v +x
			find $UNIX_STACK_ROOT/snapshots | egrep -e '\.dylib$' | xargs chmod -v +x
			;;
	esac
	echo "Restored dependencies"
else
	echo "Building dependencies"
	stack $STACK_ARGS build --only-dependencies
fi

if [ "$CACHE_RESTORED_STACK_SETUP_EXE_CACHE" = true ]
then
	case "$AGENT_OS" in
		Linux|Darwin)
			find $UNIX_STACK_ROOT/setup-exe-cache -type f | xargs chmod -v +x
			;;
	esac
	echo "Restored setup executable"
fi

stack $STACK_ARGS build --pedantic
