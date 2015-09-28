emacs ?= emacs

ert_tests_el = $(wildcard tests/*.el)
el = $(wildcard *.el)
elc = $(el:%.el=%.elc)

.PHONY: all test build test-ert test-install

all:

test: build test-ert test-install

build: $(elc)

$(elc): %.elc: %.el
	$(emacs) -batch -Q -f batch-byte-compile $<


test-ert: $(ert_tests_el)
	$(emacs) -batch -Q -L . --eval "(require 'ert)" $(^:%=-l "%") \
		-f ert-run-tests-batch-and-exit


##################################
# test-install

elisp_install_files := \
	(mapc \
		(lambda (f) \
			(message \"Install file: %s\" f) \
			(package-install-file f)) \
		command-line-args-left)


test-install: $(el)
	$(emacs) -batch -Q \
		--eval '(setq package-user-dir "$(PWD)/test-install")' \
		--eval "$(elisp_install_files)" \
		$^
