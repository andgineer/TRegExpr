#!/usr/bin/env bash
#
# Create docs in docs/
#

#./scripts/docstrings.sh

for lang in en bg de es fr ru; do  # en should be the first language as it clears the root of the site
    ./docs-render-config.sh $lang
    mkdocs build --config-file _mkdocs.yml
    if [ $lang = "en" ]; then
      mkdir -p ../site/en
      cp -r ../site/latest ../site/en/latest
      python fix_folder_redirects.py ../site/$lang/latest --shift-num=2
    else
      python fix_folder_redirects.py ../site/$lang/latest
    fi

    rm _mkdocs.yml
    rm -rf $lang/_css
done