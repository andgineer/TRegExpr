#!/usr/bin/env bash
#
# clear all resources from transifex site
#
tx delete --resource tregexpr._build_gettext_demos --force
tx delete --resource tregexpr._build_gettext_faq --force
tx delete --resource tregexpr._build_gettext_index --force
tx delete --resource tregexpr._build_gettext_regular_expressions --force
tx delete --resource tregexpr._build_gettext_tregexpr --force
