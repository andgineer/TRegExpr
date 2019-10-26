#!/usr/bin/env bash
#
# map transifex to po-file
#

./update_po.sh

TRANSIFEX_PROJECT=tregexpr
tx config mapping-bulk \
    --project $TRANSIFEX_PROJECT \
    --file-extension '.pot' \
    --source-file-dir _build/gettext \
    --source-lang en \
    --type PO \
    --expression 'locale/<lang>/LC_MESSAGES/{filepath}/{filename}.po' \
    --execute
