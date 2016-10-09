#!/bin/sh

set -e

STACK_OPTS='--allow-different-user'
VOLUME_CONTAINER='poi-stack-volume'

docker create -v /root/.stack \
              --name $VOLUME_CONTAINER \
              busybox \
              /bin/true 2>/dev/null && true

docker run -it --rm \
       -v `pwd`:/tmp/poi \
       --volumes-from $VOLUME_CONTAINER \
       -w /tmp/poi \
       fpco/stack-build:lts-7.2 \
       sh -c "stack ${STACK_OPTS} install && \
              mkdir -p /tmp/poi/bin/linux-x64 && \
              cp -r /root/.local/bin/* /tmp/poi/bin/linux-x64"


       # -v $HOME/.stack:/root/.stack
