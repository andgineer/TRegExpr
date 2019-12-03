#!/usr/bin/env bash

echo To install free pascal run ./.travis-lazarus/.travis.install.py

rm -r test/bin
if lazbuild test/test_fpc.lpi ; then
    test/test_fpc --all --format=plain
    else echo "*** Compilation error ***"
  fi
