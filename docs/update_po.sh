#!/usr/bin/env bash
#
# refresh po-file so they are ready to push to weblate for example
#

# 1st create pot in _build/gettext
make gettext

# create sphinx locales
sphinx-intl update -p _build/gettext -l ru -l es -l bg -l de -l fr

