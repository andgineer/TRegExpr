#!/usr/bin/env bash
#
# Create docs in docs/
#

#./scripts/docstrings.sh

for lang in en bg de es fr ru; do  # en should be the first language as it clears the root of the site
    ./docs-render-config.sh $lang
    mkdocs build --config-file _mkdocs.yml
    rm _mkdocs.yml
    rm -rf $lang/_css
done