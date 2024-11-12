#!/bin/bash

DIRECTORY="./doc/examples"

walk_dir() {
    echo "walk $1"
    for FILE in "$1"/*; do
        if [ -f "$FILE" ] && [[ "$FILE" == *.ppa ]]; then
            echo "ppa check $FILE"

            ppa check "$FILE"
        fi
        if [ -d "$FILE" ]; then
            walk_dir $FILE
        fi
    done
}


walk_dir $DIRECTORY