#!/usr/bin/env bash
#
# Build translated docs in _build/html/
#
sphinx-build -b html -D language=ru . _build/html/ru
#sphinx-build -b html -D language=es_ES . _build/html/es
#sphinx-build -b html -D language=bg_BG . _build/html/bg
#sphinx-build -b html -D language=de_DE . _build/html/de
#sphinx-build -b html -D language=fr_FR . _build/html/fr
google-chrome _build/html/ru/index.html

