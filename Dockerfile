from mitchty/alpine-ghc:latest

COPY . /tmp/
# RUN stack setup
RUN stack build
