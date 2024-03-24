#!make

# If the first argument is "docs" treat additional "targets" as parameters
ifeq (docs,$(firstword $(MAKECMDGOALS)))
  DOCS_LANGUAGE := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  # turn the parameters into do-nothing targets
  $(eval $(RUN_ARGS):;@:)
endif

.PHONY: docs # mark as phony so it always runs even we have a docs folder
.HELP: docs  ## Docs preview for the language specified (bg de en es fr ru)
docs:
# 	docstrings.sh
	open -a "Google Chrome" http://127.0.0.1:8000/
	./docs-render-config.sh $(DOCS_LANGUAGE)
	mkdocs serve -f _mkdocs.yml

.HELP: help  ## Display this message
help:
	@grep -E \
		'^.HELP: .*?## .*$$' $(MAKEFILE_LIST) | \
		sort | \
		awk 'BEGIN {FS = ".HELP: |## "}; {printf "\033[36m%-19s\033[0m %s\n", $$2, $$3}'
