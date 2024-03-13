#!/usr/bin/env bash
#
# Copy mkdocs.yml to _mkdocs.yml with substitute language and site.
# the language should be passed as an argument
# Place CSS to appropriate docs/src/ folder.
#

lang=$1

sed "s/LANG_PLACEHOLDER/$lang/g" mkdocs.yml > _mkdocs.yml
if [ $lang = "en" ]; then
    # place English to the root of the site
    sed -i'' -e "s/SITE_PLACEHOLDER//g" _mkdocs.yml
else
    sed -i'' -e "s/SITE_PLACEHOLDER/$lang/g" _mkdocs.yml
fi
cp -r css src/$lang/_css/  # mkdocs expects css to be in the root of the docs folder
