#!/usr/bin/env bash

echo To install free pascal run ./.travis-lazarus/.travis.install.py

lazbuild test/tregexpr_test.lpi
test/bin/tregexpr_test --all --format=plain