#!/usr/bin/env bash
for file in *.md; do if [[ -f "$file" ]]; then
    pandoc -f markdown_strict -t rst $file -o ${file%.md}.rst
    echo $file
fi; done
