# TRegExpr documentation compilation

Sources from `docs/src/` are compiled by Github Action `docs` to `site/` using 
[Material for MkDocs](https://squidfunk.github.io/mkdocs-material/).

The English version (en) is placed at the root of the site, while other 
languages, are placed in corresponding subdirectories. 
Consequently, by default, the English version is served.

The GitHub action is triggered by pushes to the `main` branch. 
After compiling the docs to `site/`, it pushes them to the `gh-pages` branch. 
The GitHub repository is configured to serve the gh-pages branch as a 
website with the custom domain `regex.sorokin.engineer`.

Use `make en` and similar commands to compile and debug the documentation locally. 
To see all available commands, use `make help`.

## A bit of history

In March 2024, I decided to stop struggling with overly complicated [gettext](https://www.gnu.org/software/gettext/) 
translations with WebLate and took a significant leap of faith to abandon gettext
translations. 

It's not meant for RST files, and maintaining it has been challenging.

There's good reasoning for this decision outlined here:
[Should we use gettext for RST files?](https://github.com/natcap/invest.users-guide/issues/54)

Simultaneously, I opted to transition from Sphinx/RST to Markdown to simplify 
things. 

With `Material for MkDocs`, RST no longer presents any real advantages.
