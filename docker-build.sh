#!/bin/bash
docker run --rm -v $PWD:/opt/fuchu -w /opt/fuchu mcr.microsoft.com/dotnet/core/sdk:3.1 bash -c 'apt-get install ca-certificates && ./build.sh'
