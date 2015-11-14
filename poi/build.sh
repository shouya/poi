#!/bin/bash
#
# build poi binary using docker
#

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
OUTDIR=bin
DOCKER_MACHINE=myubuntu

cd $SCRIPTDIR

eval "$(docker-machine env "$DOCKER_MACHINE")"

stack build -v < <(echo)

find .stack-work/dist/x86_64-linux \
     -type f \
     -name poi-exe \
     -exec cp {} bin/poi \;
