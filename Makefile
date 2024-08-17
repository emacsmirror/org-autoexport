# Makefile for managing MELPA package.

PACKAGE = org-autoexport.el

ELDEV = eldev $(ARGS)
CLEANERS = distribution

all: help

setup: ## Set up Eldev
	@ $(ELDEV) init -n

ehelp: ## Show Eldev help
	@ $(ELDEV) help

check: lint doctor test ## Run all tests

lint: ## Run lint checks
	@- $(ELDEV) lint $(OPTS)

doctor: ## Run package checks
	@- $(ELDEV) doctor $(OPTS)

test: ## Run unit tests
	@- $(ELDEV) test $(OPTS)

clean: ## Clean up
	@ $(ELDEV) clean $(OPTS) $(CLEANERS)

help: ## This help message
	@ echo "Usage: make [target]"
	@ echo
	@ grep -h ":.*##" $(MAKEFILE_LIST) | grep -v 'sed -e' | \
	  sed -e 's/:.*##/:/' | column -t -s:
