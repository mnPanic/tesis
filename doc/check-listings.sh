#!/bin/bash

DIRECTORY="./listings"

for FILE in "$DIRECTORY"/*; do
    if [ -f "$FILE" ] && [[ "$FILE" == *.ppa ]]; then
        echo "ppa check $FILE"

        ppa check "$FILE"
    fi
done
