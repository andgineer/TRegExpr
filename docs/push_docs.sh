#!/usr/bin/env bash
#
# upload to transiflex strings to translate
#
./create_translation.sh
tx push --source --translations
