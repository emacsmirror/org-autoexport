# Front end makefile for eldev.

ELDEV = eldev $(OPTS)
CLEANERS = distribution

.DEFAULT:; @ $(ELDEV) $@ $(ARGS)

all: help

setup: ## Set up Eldev
	@ $(ELDEV) upgrade-self

ehelp: ## Show Eldev help
	@ $(ELDEV) help

check: lint doctor test ## Run all tests

lint: ## Run lint checks
	@- $(ELDEV) lint $(ARGS)

doctor: ## Run package checks
	@- $(ELDEV) doctor $(ARGS)

test: ## Run unit tests
	@- $(ELDEV) test $(ARGS)

clean: ## Clean up
	@ $(ELDEV) clean $(ARGS) $(CLEANERS)

help: ## This help message
	@ echo "Usage: make [target]"
	@ echo
	@ grep -h ":.*##" $(MAKEFILE_LIST) | grep -v 'sed -e' | \
	  sed -e 's/:.*##/:/' | column -t -s:
