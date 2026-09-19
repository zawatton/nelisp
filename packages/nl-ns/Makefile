EMACS ?= emacs
PACKAGE_ROOT := $(dir $(lastword $(MAKEFILE_LIST)))
BASELINE := $(PACKAGE_ROOT)baseline/emacs-30.2.el

.PHONY: baseline ns-report

baseline:
	$(EMACS) -Q --batch -l $(PACKAGE_ROOT)scripts/nl-ns-dump-baseline.el \
	  --eval "(nl-ns-dump-baseline-write \"$(BASELINE)\")"

ns-report:
	$(EMACS) -Q --batch -L $(PACKAGE_ROOT)../nl-prelude/src -L $(PACKAGE_ROOT)src \
	  -l $(PACKAGE_ROOT)src/nl-ns-cmd.el -- \
	  --baseline $(BASELINE) --fail-severity 3
