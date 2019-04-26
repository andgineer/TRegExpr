#!/usr/bin/env bash
#
# Download translation results from transiflex to locale/
#
tx pull --all

# local build to check
./build.sh
