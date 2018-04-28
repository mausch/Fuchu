#!/bin/bash
set -e
PAKET=./.paket/paket.exe
FAKE=./packages/build/FAKE/tools/FAKE.exe
mono $PAKET restore
msbuild /t:restore
mono $FAKE build.fsx "$@"
