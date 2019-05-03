#!/usr/bin/env bash
#
# (Re)create po file for translation in locale/
#

# 1st create pot in _build/gettext
make gettext

# now create po-files with sphinx-intl
sudo pip3 install sphinx-intl python-levenshtein
sphinx-intl update -p _build/gettext -l ru -l es_ES -l bg_BG -l de_DE -l fr_FR

# map transifex to po-file
TRANSIFEX_PROJECT=tregexpr
tx config mapping-bulk \
    --project $TRANSIFEX_PROJECT \
    --file-extension '.pot' \
    --source-file-dir _build/gettext \
    --source-lang en \
    --type PO \
    --expression 'locale/<lang>/LC_MESSAGES/{filepath}/{filename}.po' \
    --execute
