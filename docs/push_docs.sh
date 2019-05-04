#!/usr/bin/env bash
#
# upload to transiflex strings to translate
#
./map_trasifex.sh
tx push --source --translations
