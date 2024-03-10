#!/usr/bin/env bash
#
# Extract docstrings to docs/
# make a copy for all languages
#

lazydocs \
    --output-path="./docs/en/api-reference" \
    --overview-file="index.md" \
    --src-base-url="https://github.com/andgineer/opensearch-log/blob/master/" \
    src/opensearch_log

cp -r ./docs/en/api-reference ./docs/ru/api-reference
