#!/usr/bin/env bash
#
# (Re)create po file for translation in locale/
# The same as map_transifex.sh but install all dependencies

sudo pip3 install sphinx-intl python-levenshtein

./map_trasifex.sh
