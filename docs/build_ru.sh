#!/usr/bin/env bash
#
# Build translated docs in _build/html/ru_RU
#
sphinx-build -b html -D language=ru_RU . _build/html/ru
google-chrome _build/html/ru/index.html

