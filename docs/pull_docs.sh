#!/usr/bin/env bash
#
# Download translation results from transiflex to locale/
#
tx pull --all

# local build to check
sphinx-build -b html -D language=ru_RU . _build/html/ru
google-chrome _build/html/ru/index.html

