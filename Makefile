SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

ifeq ($(origin .RECIPEPREFIX), undefined)
	$(error This Make does not support .RECIPEPREFIX.)
endif
.RECIPEPREFIX = >

STACK := stack
PROJ_NAME := hisp

build:
> $(STACK) build
.PHONY: build

exec:
> $(STACK) exec $(PROJ_NAME)_exe
.PHONY: exec

test:
> $(STACK) test
.PHONY: test

clean:
> $(STACK) clean
.PHONY: clean

purge:
> $(STACK) purge
.PHONY: purge
