#!/bin/bash

set -e

if [ "$AGENT_OS" = Windows_NT ]
then
	PYTHON=python
	LOCAL_INSTALL_ROOT=$(cygpath -u $(stack path --local-install-root))
else
	PYTHON=python3
	LOCAL_INSTALL_ROOT=$(stack path --local-install-root)
fi

export PATH=$LOCAL_INSTALL_ROOT/bin:$PATH

echo "Using Python at $(which $PYTHON):"
$PYTHON --version

$PYTHON -m pip install -U pip setuptools wheel
$PYTHON -m pip install -r scripts/requirements.txt

for EXAMPLE_BUILD in $(ls -d examples/lang/*/build.sh)
do
	bash $EXAMPLE_BUILD
done

if $PYTHON scripts/test_examples.py -vvv
then
	echo "##vso[task.setvariable variable=failed;isOutput=true]false"
else
	echo "##vso[task.setvariable variable=failed;isOutput=true]true"
	exit 1
fi
