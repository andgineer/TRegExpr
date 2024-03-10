#!/usr/bin/env bash
#
# Create docs in docs/
#

rm -rf ../site
#./scripts/docstrings.sh
for lang in bg de en es fr ru; do
    sed "s/LANG_PLACEHOLDER/$lang/g" mkdocs.yml > _mkdocs.yml

    if [ -d "../site" ]; then
        mkdocs build --dirty --config-file _mkdocs.yml
    else
        mkdocs build --config-file _mkdocs.yml
    fi

    rm _mkdocs.yml
done