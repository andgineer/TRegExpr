#!make

.PHONY: en # mark as phony so it always runs even we have a docs folder
.HELP: en  ## Docs preview English
en:
# 	docstrings.sh
	open -a "Google Chrome" http://127.0.0.1:8000/
	./docs-render-config.sh en
	mkdocs serve -f _mkdocs.yml

.PHONY: ru # mark as phony so it always runs even we have a docs folder
.HELP: ru  ## Docs preview Russian
ru:
# 	docstrings.sh
	open -a "Google Chrome" http://127.0.0.1:8000/
	./docs-render-config.sh ru
	mkdocs serve -f _mkdocs.yml

.HELP: help  ## Display this message
help:
	@grep -E \
		'^.HELP: .*?## .*$$' $(MAKEFILE_LIST) | \
		sort | \
		awk 'BEGIN {FS = ".HELP: |## "}; {printf "\033[36m%-19s\033[0m %s\n", $$2, $$3}'
