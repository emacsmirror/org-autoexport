# Makefile for managing MELPA package.

PACKAGE = org-autoexport.el

CLEANERS = distribution

all: help

setup: ## Set up Eldev
	@ eldev init -n

ehelp: ## Show Eldev help
	@ eldev help

check: ## Run lint checks
	@ eldev lint $(OPTS)

doctor: ## Run package checks
	@ eldev doctor $(OPTS)

test: ## Run tests
	@ eldev test $(OPTS)

clean: ## Clean up
	@ eldev clean $(OPTS) $(CLEANERS)

help: ## This help message
	@ echo "Usage: make [target]"
	@ echo
	@ grep -h ":.*##" $(MAKEFILE_LIST) | grep -v 'sed -e' | \
	  sed -e 's/:.*##/:/' | column -t -s:
