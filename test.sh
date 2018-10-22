#!/usr/bin/env bash

echo To install free pascal run ./.travis-lazarus/.travis.install.py

rm -r test/bin
if lazbuild test/testregexpr.lpi ; then
    test/testregexpr --all --format=plain
    else echo "*** Compilation error ***"
  fi

