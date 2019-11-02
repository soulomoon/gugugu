#!/bin/bash

set -e

env | sort

if [ "$AGENT_OSARCHITECTURE" != X64 ]
then
	echo "Unexpected Agent.OSArchitecture = $AGENT_OSARCHITECTURE"
	exit 1
fi

case "$AGENT_OS" in
	Linux|Darwin)
		STACK_ROOT="$BUILD_SOURCESDIRECTORY/build/stack"
		UNIX_STACK_ROOT="$STACK_ROOT"
		STACK_BIN="$STACK_ROOT/bin"
		;;
	Windows_NT)
		STACK_ROOT="$BUILD_SOURCESDIRECTORY\\build\\stack"
		UNIX_STACK_ROOT=$(cygpath --unix $STACK_ROOT)
		STACK_BIN="$STACK_ROOT\\bin"
		;;
	*)
		echo "Unexpected Agent.OS = $AGENT_OS"
		exit 1
		;;
esac

echo "Using stack_root = $STACK_ROOT"
echo "##vso[task.setvariable variable=stack_root]$STACK_ROOT"

echo "Using UNIX_STACK_ROOT = $UNIX_STACK_ROOT"
echo "##vso[task.setvariable variable=UNIX_STACK_ROOT]$UNIX_STACK_ROOT"

echo "Prepend path $STACK_BIN"
echo "##vso[task.prependpath]$STACK_BIN"

STACK_RESOLVER=$(grep -e "^resolver:" stack.yaml | xargs echo | cut -d " " -f 2)
echo "Using stack_resolver = $STACK_RESOLVER"
echo "##vso[task.setvariable variable=stack_resolver]$STACK_RESOLVER"

GUGUGU_VERSION=$(grep -e "version:" hpack-common.yaml | xargs echo | cut -d " " -f 2)
echo "Using gugugu_version = $GUGUGU_VERSION"
echo "##vso[task.setvariable variable=gugugu_version]$GUGUGU_VERSION"
