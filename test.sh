#!/usr/bin/env bash

echo To install free pascal run ./.travis-lazarus/.travis.install.py

rm -r test/bin
if lazbuild test/tregexpr_test.lpi ; then
    test/bin/tregexpr_test --all --format=plain
    else echo "*** Compilation error ***"
  fi

