#!/usr/bin/env bash

echo To install Free Pascal, run ./.travis-lazarus/.travis.install.py

rm -r test/bin

if lazbuild test/test_fpc.lpi ; then
    test/test_fpc --all --format=plain
    else echo "*** Compilation error ***"
  fi

if lazbuild -r --bm=Unicode test/test_fpc.lpi ; then
    test/test_fpc --all --format=plain
    else echo "*** Compilation error ***"
  fi

if lazbuild -r --bm=UnicodeAndChars test/test_fpc.lpi ; then
    test/test_fpc --all --format=plain
    else echo "*** Compilation error ***"
  fi
