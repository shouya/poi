#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd "$SCRIPT_DIR"

[ -f service.list ] || echo "File <service.list> does not exist" && exit
[ -x poi-exe ]      || echo "poi executable does not exists"     && exit

./poi-exe
