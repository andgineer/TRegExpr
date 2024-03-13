#!/usr/bin/env bash
#
# Convert .rst files to GitHub-style .md
# Used for migration from Sphinx & gettext to MkDocs
#
for lang in bg de en es fr ru; do
    find "$lang" -name "*.rst" -type f -exec sh -c '
        python adjust-rst.py "$0" &&
        pandoc -s --from=rst --to=gfm -o "${0%.rst}.md" "$0" &&
        sed -i'' -e "s@\\\\<a @<a @g" -e "s@\\\\>\\\</a\\\>@></a>@g" "${0%.rst}.md" &&
        rm "$0"
    ' {} \;
done
