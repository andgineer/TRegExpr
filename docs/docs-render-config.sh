#!/usr/bin/env bash
#
# Substitute language and site in mkdocs.yml in _mkdocs.yml
# language should be passed as an argument
#

lang=$1

sed "s/LANG_PLACEHOLDER/$lang/g" mkdocs.yml > _mkdocs.yml
if [ $lang = "en" ]; then
    # place English to the root of the site
    sed -i'' -e "s/SITE_PLACEHOLDER//g" _mkdocs.yml
else
    sed -i'' -e "s/SITE_PLACEHOLDER/$lang/g" _mkdocs.yml
fi
