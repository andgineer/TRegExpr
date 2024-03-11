import os
import shutil
import argparse


def shift_path_up(file_path, shift_num):
    with open(file_path, 'r', encoding='utf-8') as file:
        file_content = file.read()

    old_string = "../" * (shift_num + 1)
    new_string = "../" * shift_num

    updated_content = file_content.replace(old_string, new_string)

    with open(file_path, 'w', encoding='utf-8') as file:
        file.write(updated_content)


def fix_redirects(site_dir, shift_num):
    print(f"Fixing folder redirects in {site_dir}, shifting paths by {shift_num} level(s).")
    for root, dirs, files in os.walk(site_dir):
        for dir_name in dirs:
            source_path = os.path.join(root, dir_name, "index.html")
            if os.path.exists(source_path):
                # Construct the new .html filename based on the directory name
                dest_file_name = f"{dir_name}.html"
                dest_path = os.path.join(root, dest_file_name)

                # Copy <folder>/index.html to <folder>.html
                shutil.copy2(source_path, dest_path)
                shift_path_up(dest_path, shift_num)
                print(f"Copied: {source_path} to {dest_path}")
            else:
                print(f"index.html not found for {source_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert all MkDocs folder redirects to HTML equivalents.")
    parser.add_argument("site_dir", help="The path to the MkDocs site directory to be processed.")
    parser.add_argument("--shift-num", type=int, default=1, help="The number of '../' levels to remove from paths.")

    args = parser.parse_args()
    fix_redirects(args.site_dir, args.shift_num)
