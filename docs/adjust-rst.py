"""Adjust RST for Markdown conversion.

Fix headers and anchors for pandoc compatibility.
Used for migration from Sphinx & gettext to MkDocs
"""
import re
import sys


def adjust_rst(rst_file):
    with open(rst_file, 'r', encoding='utf-8') as file:
        lines = file.readlines()

    # Regular expression to find header lines followed by underline markers
    header_regex = re.compile(r'^(.*?)\n([-=~])\2{2,}\n', re.MULTILINE)
    # Regular expression to find RST anchors
    anchor_regex = re.compile(r'\.\.\s+_(\w+):')

    def header_replacement(match):
        text = match.group(1)
        marker = match.group(2)[0] * len(text)  # Use the first character of the marker
        return f"{text}\n{marker}\n"

    content = header_regex.sub(header_replacement, ''.join(lines))

    def anchor_replacement(match):
        return f'<a name="{match.group(1)}"></a>'

    new_content = anchor_regex.sub(anchor_replacement, content)

    with open(rst_file, 'w', encoding='utf-8') as file:
        file.write(new_content)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python adjust_rst_headers.py <path/to/input.rst>")
        sys.exit(1)
    adjust_rst(sys.argv[1])
