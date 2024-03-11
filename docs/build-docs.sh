#!/usr/bin/env bash
#
# Create docs in docs/
#

SITE_FOLDER="../site"  # `site_dir` in mkdocs.yml

rm -rf ${SITE_FOLDER}

#./scripts/docstrings.sh

for lang in bg de en es fr ru; do
    ./docs-render-config.sh $lang
    mkdocs build --dirty --config-file _mkdocs.yml
    rm _mkdocs.yml
done