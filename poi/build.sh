#!/bin/bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd $DIR

set -e

cabal sandbox init
cabal configure
cabal build
cp -f dist/build/poi/poi ../poi
strip ../poi
