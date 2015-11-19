from mitchty/alpine-ghc:latest

RUN apk --update add build-base

COPY stack.yaml poi.cabal /tmp/
