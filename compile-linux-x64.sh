#!/bin/sh

set -e

STACK_OPTS='--allow-different-user'

docker build -t poi-linux-x64 .

docker run -it --rm \
       -v `pwd`:/tmp/poi \
       -v $HOME/.stack:/root/.stack \
       -w /tmp/poi \
       poi-linux-x64 \
       sh -c "stack ${STACK_OPTS} build"
