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

# I did not find more intelligent way have API Docs for all languages
for lang in bg de es fr ru; do
  cp -r ./docs/en/api-reference ./docs/$lang/api-reference
done
