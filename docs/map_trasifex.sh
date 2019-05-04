#!/usr/bin/env bash
#
# map transifex to po-file
#

# 1st create pot in _build/gettext
make gettext

# create sphinx locales
sphinx-intl update -p _build/gettext -l ru -l es -l bg -l de -l fr

TRANSIFEX_PROJECT=tregexpr
tx config mapping-bulk \
    --project $TRANSIFEX_PROJECT \
    --file-extension '.pot' \
    --source-file-dir _build/gettext \
    --source-lang en \
    --type PO \
    --expression 'locale/<lang>/LC_MESSAGES/{filepath}/{filename}.po' \
    --execute
