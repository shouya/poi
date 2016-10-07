from mitchty/alpine-ghc:latest

RUN apk --update add build-base

COPY stack.yaml /root/.stack/global-project/stack.yaml

RUN stack setup




