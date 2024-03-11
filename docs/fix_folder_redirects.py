import os
import shutil
import argparse


def shift_path_one_level_up(file_path):
    with open(file_path, 'r', encoding='utf-8') as file:
        file_content = file.read()

    # Replace the old string with the new string
    updated_content = file_content.replace("../../", "../")

    # Write the updated content back to the file
    with open(file_path, 'w', encoding='utf-8') as file:
        file.write(updated_content)


def fix_redirects(site_dir):
    print(f"Fixing folder redirects in {site_dir}")
    for root, dirs, files in os.walk(site_dir):
        for dir_name in dirs:
            source_path = os.path.join(root, dir_name, "index.html")
            if os.path.exists(source_path):
                # Construct the new .html filename based on the directory name
                dest_file_name = f"{dir_name}.html"
                dest_path = os.path.join(root, dest_file_name)

                # Copy <folder>/index.html to <folder>.html
                shutil.copy2(source_path, dest_path)
                shift_path_one_level_up(dest_path)
                print(f"Copied: {source_path} to {dest_path}")
            else:
                print(f"index.html not found for {source_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert all MkDocs folder redirects to HTML equivalents.")
    parser.add_argument("site_dir", help="The path to the MkDocs site directory to be processed.")

    args = parser.parse_args()
    fix_redirects(args.site_dir)
