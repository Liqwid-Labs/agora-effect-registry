# This really ought to be `/usr/bin/env bash`, but nix flakes don't like that.
SHELL := /bin/sh

.PHONY: hoogle haddock format_nix format_haskell format_check


SOURCE_FILES := $(shell git ls-tree -r HEAD --full-tree --name-only)
SOURCE_FILES := $(wildcard $(SOURCE_FILES))
HASKELL_SOURCES := $(filter %.hs,$(SOURCE_FILES))
CABAL_SOURCES := $(filter %.cabal,$(SOURCE_FILES))
NIX_SOURCES := $(filter %.nix,$(SOURCE_FILES))
FORMAT_EXTENSIONS := -o -XQuasiQuotes -o -XTemplateHaskell -o -XTypeApplications	\
										-o -XImportQualifiedPost -o -XPatternSynonyms
HLINT_EXTS := -XQuasiQuotes

THREADS ?= 8
TEST_CASE_TIMEOUT ?= 100

usage:
	@echo "usage: [env [<variable>=<value> ...]] make <command> [OPTIONS]"
	@echo
	@echo "Available variables:"
	@echo "  THREADS -- The number of threads for building the project"
	@echo "  TEST_CASE_TIMEOUT -- Timeout for individual tests. Default unit: s"
	@echo
	@echo "Available commands:"
	@echo "  format_haskell -- Format haskell stuff, including source code and cabal files"
	@echo "  format_nix -- Format *.nix files only"
	@echo "  format_check -- Check if all haskell stuff have been formatted correctly"
	@echo "  haddock -- Generate Haddock docs for project"
	@echo "  hoogle -- Start local hoogle"

requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'nix develop' first" && false)

format: format_haskell format_nix

format_nix: requires_nix_shell
	nixpkgs-fmt $(NIX_SOURCES)

format_haskell: requires_nix_shell
	fourmolu $(FORMAT_EXTENSIONS) -m inplace $(HASKELL_SOURCES)
	cabal-fmt -i $(CABAL_SOURCES)

format_check: requires_nix_shell
	fourmolu $(FORMAT_EXTENSIONS) -m check $(HASKELL_SOURCES)
	nixpkgs-fmt --check $(NIX_SOURCES) 
	cabal-fmt --check $(CABAL_SOURCES)

hoogle: requires_nix_shell
	pkill hoogle || true
	hoogle generate --local=haddock --database=hoo/local.hoo
	hoogle server --local -p 8081 >> /dev/null &
	hoogle server --local --database=hoo/local.hoo -p 8082 >> /dev/null &

haddock: requires_nix_shell
	cabal haddock --haddock-html --haddock-hoogle --builddir=haddock
