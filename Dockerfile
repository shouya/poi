from mitchty/alpine-ghc:latest

COPY stack.yaml poi.cabal /tmp/

RUN stack setup
