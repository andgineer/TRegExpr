"""Translate the .rst files using the .po files (GetText).

Used for migration from Spinx/rst & gettext to plain rst-files.
"""
import polib
import os
import sys


def apply_translations(rst_directory, po_directory, output_directory):
    """Apply translations to the RST files."""
    if not os.path.exists(output_directory):
        os.makedirs(output_directory)

    for rst_filename in os.listdir(rst_directory):
        if rst_filename.endswith('.rst'):
            base_filename = os.path.splitext(rst_filename)[0]
            po_file_path = os.path.join(po_directory, 'LC_MESSAGES', f'{base_filename}.po')
            print(f"Translate {rst_filename} using {po_file_path}.po ...", end="")
            rst_file_path = os.path.join(rst_directory, rst_filename)
            output_file_path = os.path.join(output_directory, rst_filename)

            if os.path.exists(po_file_path):
                po = polib.pofile(po_file_path)
                with open(rst_file_path, 'r', encoding='utf-8') as file:
                    rst_content = file.readlines()

                for entry in po.translated_entries():
                    rst_content = [line.replace(entry.msgid, entry.msgstr) for line in rst_content]

                with open(output_file_path, 'w', encoding='utf-8') as file:
                    file.writelines(rst_content)
                print("done.")

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python script.py <RST directory> <PO directory> <Output directory>")
        sys.exit(1)

    _, rst_directory, po_directory, output_directory = sys.argv
    apply_translations(rst_directory, po_directory, output_directory)
