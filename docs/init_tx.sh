#!/usr/bin/env bash
#
# init transifex
#
sudo pip3 install transifex-client
# token https://www.transifex.com/user/settings/api/
tx init --token $TOKEN --no-interactive