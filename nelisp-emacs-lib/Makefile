## nelisp-emacs Makefile

EMACS = emacs --batch
# NeLisp is driven through the pure-Elisp standalone reader.
VENDOR_NELISP = vendor/nelisp
DEFAULT_NELISP_ROOT := $(firstword $(wildcard $(VENDOR_NELISP) ../nelisp $(HOME)/Notes/dev/nelisp))
NELISP_ROOT  ?= $(if $(DEFAULT_NELISP_ROOT),$(DEFAULT_NELISP_ROOT),$(VENDOR_NELISP))
DEFAULT_NELISP_BIN := $(firstword $(wildcard $(NELISP_ROOT)/target/nelisp-standalone-reader))
NELISP_BIN   ?= $(if $(DEFAULT_NELISP_BIN),$(DEFAULT_NELISP_BIN),$(NELISP_ROOT)/target/nelisp-standalone-reader)
NELISP_BOOT_TIMEOUT ?= 420s
NEMACS_NELISP_ERT_TIMEOUT ?= 420s
NELISP_BOOT_PROFILE_TIMEOUT ?= 1200s
NELISP_BOOT_PROFILE_LIMIT ?= nil
NELISP_VENDOR_CORE_TIMEOUT ?= 900s
NEMACS_RUNTIME_BAKE_TIMEOUT ?= 900s
NEMACS_VENDOR_CORE_RUNTIME_BAKE_TIMEOUT ?= 900s
NEMACS_RUNTIME_REPLAY_TIMEOUT ?= 900s
NEMACS_INTERACTIVE_RUNTIME_REPLAY_TIMEOUT ?= 1200s
NEMACS_VENDOR_CORE_RUNTIME_REPLAY_TIMEOUT ?= 1200s
NELISP_STACK_LIMIT ?= unlimited
BUILD_DIR ?= build
NEMACS_BOOTSTRAP_BUNDLE ?= $(BUILD_DIR)/nemacs-bootstrap.el
NEMACS_IMAGE ?= $(BUILD_DIR)/nemacs-loadup.nli
NEMACS_RUNTIME_IMAGE ?= $(BUILD_DIR)/nemacs-runtime.nlri
NEMACS_INTERACTIVE_RUNTIME_IMAGE ?= $(BUILD_DIR)/nemacs-interactive-runtime.nlri
NEMACS_VENDOR_CORE_RUNTIME_IMAGE ?= $(BUILD_DIR)/nemacs-vendor-core-runtime.nlri
NEMACS_RUNTIME_PRELOAD ?= scripts/nemacs-runtime-image-preload.el
VENDOR_CLASS_A_LIMIT ?= 18
VENDOR_CLASS_A_STRICT ?= 0
VENDOR_CLASS_A_STRICT_ELISP := $(if $(filter 1 t true yes,$(VENDOR_CLASS_A_STRICT)),t,nil)
VENDOR_CORE_LIMIT ?= 0
VENDOR_CORE_MODULES ?=
VENDOR_CORE_STRICT ?= 1
VENDOR_CORE_STRICT_ELISP := $(if $(filter 1 t true yes,$(VENDOR_CORE_STRICT)),t,nil)
VENDOR_FORM_WALK_FILE ?= $(abspath vendor/emacs-lisp/simple.el)
VENDOR_FORM_WALK_TIMEOUT ?= 900s
VENDOR_FORM_WALK_START_INDEX ?= 1
VENDOR_FORM_WALK_START_POS ?= nil
VENDOR_FORM_WALK_LIMIT ?= 0
VENDOR_FORM_WALK_PRINT_EVERY ?= 25
VENDOR_FORM_WALK_PRINT_READ ?= nil
VENDOR_FORM_WALK_NORMALIZE_FLOATS ?= nil
VENDOR_LOAD_PRELUDE ?= $(abspath $(NELISP_ROOT)/scripts/nelisp-stdlib-prelude.el)
VENDOR_LOAD_FILES ?= $(abspath vendor/emacs-lisp/emacs-lisp/lisp-mode.el) $(abspath vendor/emacs-lisp/isearch.el) $(abspath vendor/emacs-lisp/minibuffer.el) $(abspath vendor/emacs-lisp/progmodes/project.el)
VENDOR_LOAD_PROOF_FORM ?= (fboundp (quote emacs-keymap-define-key-after))
VENDOR_LOAD_TIMEOUT ?= 900s
NELISP_LOAD_PATH = -L $(NELISP_ROOT)/src \
	$(foreach d,$(wildcard $(NELISP_ROOT)/packages/*/src),-L $(d))
SRC_FILES = $(wildcard src/*.el)
TEST_FILES = $(wildcard test/*.el)

.PHONY: compile test test-redisplay-core-smoke doctor build-nelisp-bootstrap bake-image bake-runtime-image bake-interactive-runtime-image bake-vendor-core-runtime-image test-nelisp test-nelisp-runtime-image test-nelisp-interactive-runtime-image test-nelisp-vendor-core-runtime-image test-nelisp-ert profile-nelisp-bootstrap diagnose-vendor-form-walk diagnose-vendor-load-replay verify-nelisp-standalone verify-vendor verify-vendor-inventory verify-vendor-class-a verify-vendor-core bench demo demo-phase2 clean nelisp nelisp-rebuild nelisp-clean help

help:
	@echo "Targets:"
	@echo "  make compile         byte-compile src/*.el"
	@echo "  make test            run ERT under host emacs"
	@echo "  make test-redisplay-core-smoke  run isolated lightweight redisplay core smoke"
	@echo "  make doctor          run host/NeLisp driver readiness checks"
	@echo "  make build-nelisp-bootstrap  generate build/nemacs-bootstrap.el"
	@echo "  make bake-image      legacy .nli state image via emacs-dump"
	@echo "  make bake-runtime-image  generate build/nemacs-runtime.nlri via standalone reader"
	@echo "  make bake-interactive-runtime-image  generate image with TUI/editor features"
	@echo "  make bake-vendor-core-runtime-image  extend base .nlri with daily-driver vendor core"
	@echo "  make test-nelisp     build bundle + run one nelisp-driver boot smoke"
	@echo "  make test-nelisp-runtime-image  bake + smoke-test the runtime image path"
	@echo "  make test-nelisp-interactive-runtime-image  bake + smoke-test TUI realise"
	@echo "  make test-nelisp-vendor-core-runtime-image  experimental vendor-core image smoke"
	@echo "  make test-nelisp-ert run nelisp-driver bootstrap ERTs (very slow)"
	@echo "  make profile-nelisp-bootstrap  time standalone bootstrap sections"
	@echo "  make diagnose-vendor-form-walk  eval a vendor file form by form"
	@echo "  make diagnose-vendor-load-replay  load vendor files through standalone reader"
	@echo "  make verify-nelisp-standalone  run pure standalone-reader gates"
	@echo "  make verify-vendor   run Doc 03 vendor inventory + vendor smoke gates"
	@echo "  make bench           run redisplay benchmark"
	@echo "  make demo            run Phase 1 close demo"
	@echo "  make demo-phase2     run Phase 2 close demo"
	@echo "  make nelisp          fetch + build the NeLisp standalone reader into vendor/nelisp/"
	@echo "  make nelisp-rebuild  clean + rebuild the vendored NeLisp standalone reader"
	@echo "  make nelisp-clean    remove vendor/nelisp/ entirely"
	@echo "  make clean           remove .elc files"
	@echo "Variables:"
	@echo "  NELISP_STACK_LIMIT=$(NELISP_STACK_LIMIT)  stack limit for large pure-Elisp loads"
	@echo "  NEMACS_NELISP_ERT_TIMEOUT=$(NEMACS_NELISP_ERT_TIMEOUT)  opt-in nelisp bootstrap ERT timeout"
	@echo "  NEMACS_RUNTIME_BAKE_TIMEOUT=$(NEMACS_RUNTIME_BAKE_TIMEOUT)  .nlri source-v1 bake timeout"
	@echo "  NEMACS_RUNTIME_REPLAY_TIMEOUT=$(NEMACS_RUNTIME_REPLAY_TIMEOUT)  .nlri source-v1 replay smoke timeout"
	@echo "  VENDOR_CORE_LIMIT=$(VENDOR_CORE_LIMIT)  daily-driver vendor modules for verify-vendor-core (0=all)"
	@echo "  VENDOR_CORE_MODULES=$(VENDOR_CORE_MODULES)  comma/space list overriding VENDOR_CORE_LIMIT"
	@echo "  VENDOR_FORM_WALK_FILE=$(VENDOR_FORM_WALK_FILE)  file for diagnose-vendor-form-walk"
	@echo "  VENDOR_FORM_WALK_NORMALIZE_FLOATS=$(VENDOR_FORM_WALK_NORMALIZE_FLOATS)  normalize floats in standalone diagnostic probes"
	@echo "  VENDOR_LOAD_PRELUDE=$(VENDOR_LOAD_PRELUDE)  standalone prelude for diagnose-vendor-load-replay"
	@echo "  VENDOR_LOAD_FILES=$(VENDOR_LOAD_FILES)  files for diagnose-vendor-load-replay"
	@echo "  VENDOR_LOAD_PROOF_FORM=$(VENDOR_LOAD_PROOF_FORM)  post-load proof for diagnose-vendor-load-replay"

compile:
	$(EMACS) -L src $(NELISP_LOAD_PATH) \
		-f batch-byte-compile $(SRC_FILES)

test:
	$(EMACS) -L src -L test -L demo $(NELISP_LOAD_PATH) \
		$(foreach t,$(TEST_FILES),-l $(t)) \
		-f ert-run-tests-batch-and-exit

test-redisplay-core-smoke:
	$(EMACS) -L src -L scripts \
		-l scripts/emacs-redisplay-core-smoke.el \
		-f ert-run-tests-batch-and-exit

doctor:
	NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		NEMACS_NELISP="$(abspath $(NELISP_BIN))" \
		NEMACS_NELISP_STACK="$(NELISP_STACK_LIMIT)" \
		./bin/nemacs --doctor

build-nelisp-bootstrap: $(NEMACS_BOOTSTRAP_BUNDLE)

$(NEMACS_BOOTSTRAP_BUNDLE): scripts/build-nelisp-bootstrap.el $(SRC_FILES)
	$(EMACS) -L src -L scripts $(NELISP_LOAD_PATH) \
		--eval '(setq nelisp-bootstrap-output-file "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))")' \
		-l scripts/build-nelisp-bootstrap.el \
		-f nelisp-bootstrap-build-batch

bake-image:
	$(EMACS) -L src $(NELISP_LOAD_PATH) \
		--eval '(setq image-baker-output-file "$(abspath $(NEMACS_IMAGE))")' \
		-l image-baker \
		-f image-baker-bake-batch

bake-runtime-image: $(NEMACS_RUNTIME_IMAGE)

$(NEMACS_RUNTIME_IMAGE): $(NEMACS_BOOTSTRAP_BUNDLE) $(NEMACS_RUNTIME_PRELOAD)
	test -x "$(NELISP_BIN)"
	mkdir -p "$(dir $(NEMACS_RUNTIME_IMAGE))"
	ulimit -s "$(NELISP_STACK_LIMIT)" 2>/dev/null || true; \
	timeout $(NEMACS_RUNTIME_BAKE_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		"$(NELISP_BIN)" dump-runtime-image "$(abspath $(NEMACS_RUNTIME_IMAGE)).tmp" \
		'(progn (load "$(abspath $(NEMACS_RUNTIME_PRELOAD))" nil (quote no-message) t t) (nemacs-runtime-image-preload-batch "$(abspath .)" "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))"))'
	mv "$(NEMACS_RUNTIME_IMAGE).tmp" "$(NEMACS_RUNTIME_IMAGE)"

bake-interactive-runtime-image: $(NEMACS_INTERACTIVE_RUNTIME_IMAGE)

$(NEMACS_INTERACTIVE_RUNTIME_IMAGE): $(NEMACS_BOOTSTRAP_BUNDLE) $(NEMACS_RUNTIME_PRELOAD)
	test -x "$(NELISP_BIN)"
	mkdir -p "$(dir $(NEMACS_INTERACTIVE_RUNTIME_IMAGE))"
	ulimit -s "$(NELISP_STACK_LIMIT)" 2>/dev/null || true; \
	timeout $(NEMACS_RUNTIME_BAKE_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		"$(NELISP_BIN)" dump-runtime-image "$(abspath $(NEMACS_INTERACTIVE_RUNTIME_IMAGE)).tmp" \
		'(progn (load "$(abspath $(NEMACS_RUNTIME_PRELOAD))" nil (quote no-message) t t) (nemacs-runtime-image-preload-interactive "$(abspath .)" "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))"))'
	mv "$(NEMACS_INTERACTIVE_RUNTIME_IMAGE).tmp" "$(NEMACS_INTERACTIVE_RUNTIME_IMAGE)"

bake-vendor-core-runtime-image:
	test -r "$(NEMACS_RUNTIME_IMAGE)" || $(MAKE) "$(NEMACS_RUNTIME_IMAGE)"
	test -x "$(NELISP_BIN)"
	mkdir -p "$(dir $(NEMACS_VENDOR_CORE_RUNTIME_IMAGE))"
	ulimit -s "$(NELISP_STACK_LIMIT)" 2>/dev/null || true; \
	timeout $(NEMACS_VENDOR_CORE_RUNTIME_BAKE_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		"$(NELISP_BIN)" extend-runtime-image "$(abspath $(NEMACS_RUNTIME_IMAGE))" \
		"$(abspath $(NEMACS_VENDOR_CORE_RUNTIME_IMAGE)).tmp" \
		'(nemacs-runtime-image-preload-vendor-core-extension)'
	mv "$(NEMACS_VENDOR_CORE_RUNTIME_IMAGE).tmp" "$(NEMACS_VENDOR_CORE_RUNTIME_IMAGE)"

test-nelisp: build-nelisp-bootstrap
	test -x "$(NELISP_BIN)"
	case "$$(basename "$(NELISP_BIN)")" in \
	  nelisp-standalone-reader) \
	    tmp=$$(mktemp "$${TMPDIR:-/tmp}/nemacs-standalone-smoke.XXXXXX.el"); \
	    printf '%s\n' '(+ 40 2)' > "$$tmp"; \
	    set +e; timeout $(NELISP_BOOT_TIMEOUT) "$(NELISP_BIN)" "$$tmp"; rc=$$?; set -e; \
	    rm -f "$$tmp"; \
	    if [ "$$rc" -eq 42 ]; then echo "STANDALONE-READER=ok exit=42"; else echo "STANDALONE-READER=fail exit=$$rc expected=42"; exit 1; fi; \
	    timeout $(NELISP_BOOT_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
	      NEMACS_NELISP="$(abspath $(NELISP_BIN))" \
	      NEMACS_NELISP_STACK="$(NELISP_STACK_LIMIT)" \
	      NEMACS_RUNTIME_IMAGE= \
	      ./bin/nemacs --driver=nelisp --batch --no-banner \
	      --eval '(+ 40 2)'; \
	    echo "NEMACS-STANDALONE-BOOT=ok exit=0" ;; \
	  *) \
	    timeout $(NELISP_BOOT_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
	      NEMACS_NELISP="$(abspath $(NELISP_BIN))" \
	      NEMACS_NELISP_STACK="$(NELISP_STACK_LIMIT)" \
	      NEMACS_RUNTIME_IMAGE= \
	      ./bin/nemacs --driver=nelisp --batch --no-banner \
	      --eval '(princ (format "BOOT=%S\n" t))' ;; \
	esac

test-nelisp-runtime-image: bake-runtime-image
	test -x "$(NELISP_BIN)"
	timeout $(NEMACS_RUNTIME_REPLAY_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		NEMACS_NELISP="$(abspath $(NELISP_BIN))" \
		NEMACS_NELISP_STACK="$(NELISP_STACK_LIMIT)" \
		./bin/nemacs --driver=nelisp --runtime-image "$(abspath $(NEMACS_RUNTIME_IMAGE))" \
		--batch --no-banner --eval '(princ (format "BOOT=%S\n" t))'

test-nelisp-interactive-runtime-image: bake-interactive-runtime-image
	test -x "$(NELISP_BIN)"
	timeout $(NEMACS_INTERACTIVE_RUNTIME_REPLAY_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		NEMACS_NELISP="$(abspath $(NELISP_BIN))" \
		NEMACS_NELISP_STACK="$(NELISP_STACK_LIMIT)" \
		./bin/nemacs --driver=nelisp --runtime-image "$(abspath $(NEMACS_INTERACTIVE_RUNTIME_IMAGE))" \
		--batch --no-banner \
		--eval '(let ((h (nemacs-main--realise-tui))) (princ (format "TUI=%S\n" (and h t))) (when h (nemacs-main--shutdown-tui)))'

test-nelisp-vendor-core-runtime-image: bake-vendor-core-runtime-image
	test -x "$(NELISP_BIN)"
	timeout $(NEMACS_VENDOR_CORE_RUNTIME_REPLAY_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		NEMACS_NELISP="$(abspath $(NELISP_BIN))" \
		NEMACS_NELISP_STACK="$(NELISP_STACK_LIMIT)" \
		./bin/nemacs --driver=nelisp --runtime-image "$(abspath $(NEMACS_VENDOR_CORE_RUNTIME_IMAGE))" \
		--batch --no-banner \
		--eval '(let ((features (quote (files simple dired help-mode help-fns subr-x seq map lisp case-table cdl range regi lisp-mode ielm isearch minibuffer project hex-util map-ynp charprop charscript emoji-labels iso-transl cp51932 eucjp-ms fontset idna-mapping ja-dic-utl)))) (princ (format "VENDOR-CORE-FEATURES=%S PROBES=%S\n" (mapcar (lambda (feature) (cons feature (featurep feature))) features) (list (cons (quote C-x-C-f) (and (boundp (quote ctl-x-map)) (lookup-key ctl-x-map "\C-f"))) (cons (quote open-line) (fboundp (quote open-line))) (cons (quote dired) (fboundp (quote dired))) (cons (quote describe-function) (fboundp (quote describe-function))) (cons (quote project-current) (fboundp (quote project-current))) (cons (quote hex) (and (fboundp (quote encode-hex-string)) (encode-hex-string "A"))) (cons (quote idna-table) (and (boundp (quote idna-mapping-table)) (> (length idna-mapping-table) #x10ffff)))))))'

test-nelisp-ert: bake-runtime-image
	test -x "$(NELISP_BIN)"
	timeout $(NEMACS_NELISP_ERT_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		NEMACS_NELISP="$(abspath $(NELISP_BIN))" \
		NEMACS_NELISP_STACK="$(NELISP_STACK_LIMIT)" \
		NEMACS_RUNTIME_IMAGE="$(abspath $(NEMACS_RUNTIME_IMAGE))" \
		NEMACS_RUN_NELISP_BOOTSTRAP=1 \
		$(EMACS) -L src -L test -L demo $(NELISP_LOAD_PATH) \
		-l test/nemacs-bootstrap-nelisp-test.el \
		-f ert-run-tests-batch-and-exit

profile-nelisp-bootstrap: build-nelisp-bootstrap
	test -x "$(NELISP_BIN)"
	timeout $(NELISP_BOOT_PROFILE_TIMEOUT) $(EMACS) -Q -L scripts \
		--eval '(setq standalone-bootstrap-profile-reader "$(abspath $(NELISP_BIN))")' \
		--eval '(setq standalone-bootstrap-profile-bundle "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))")' \
		--eval '(setq standalone-bootstrap-profile-limit "$(NELISP_BOOT_PROFILE_LIMIT)")' \
		--eval '(setq standalone-bootstrap-profile-timeout "$(NELISP_BOOT_PROFILE_TIMEOUT)")' \
		-l standalone-bootstrap-profile \
		-f standalone-bootstrap-profile-batch

diagnose-vendor-form-walk: build-nelisp-bootstrap
	test -x "$(NELISP_BIN)"
	timeout $(VENDOR_FORM_WALK_TIMEOUT) $(EMACS) -Q -L scripts \
		--eval '(setq vendor-form-standalone-reader "$(abspath $(NELISP_BIN))")' \
		--eval '(setq vendor-form-standalone-bootstrap "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))")' \
		--eval '(setq vendor-form-standalone-file "$(VENDOR_FORM_WALK_FILE)")' \
		--eval '(setq vendor-form-standalone-start-index $(VENDOR_FORM_WALK_START_INDEX))' \
		--eval '(setq vendor-form-standalone-limit $(VENDOR_FORM_WALK_LIMIT))' \
		--eval '(setq vendor-form-standalone-print-every $(VENDOR_FORM_WALK_PRINT_EVERY))' \
		--eval '(setq vendor-form-standalone-normalize-floats $(VENDOR_FORM_WALK_NORMALIZE_FLOATS))' \
		--eval '(setq vendor-form-standalone-repo-root "$(abspath .)")' \
		-l vendor-form-standalone-walk \
		-f vendor-form-standalone-batch

diagnose-vendor-load-replay: build-nelisp-bootstrap
	test -x "$(NELISP_BIN)"
	timeout $(VENDOR_LOAD_TIMEOUT) $(EMACS) -Q -L scripts \
		--eval '(setq vendor-load-standalone-reader "$(abspath $(NELISP_BIN))")' \
		--eval '(setq vendor-load-standalone-prelude "$(VENDOR_LOAD_PRELUDE)")' \
		--eval '(setq vendor-load-standalone-bootstrap "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))")' \
		--eval '(setq vendor-load-standalone-files "$(VENDOR_LOAD_FILES)")' \
		--eval '(setq vendor-load-standalone-proof-form "$(VENDOR_LOAD_PROOF_FORM)")' \
		--eval '(setq vendor-load-standalone-repo-root "$(abspath .)")' \
		-l vendor-load-standalone-replay \
		-f vendor-load-standalone-batch

verify-nelisp-standalone: doctor test-nelisp test-nelisp-runtime-image verify-vendor-class-a verify-vendor-core

verify-vendor: verify-vendor-inventory verify-vendor-class-a verify-vendor-core

verify-vendor-inventory:
	$(EMACS) -Q -L scripts \
		-l audit-vendor-classify \
		-f vendor-audit-batch

verify-vendor-class-a: build-nelisp-bootstrap
	test -x "$(NELISP_BIN)"
	timeout $(NELISP_BOOT_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		NEMACS_NELISP_STACK="$(NELISP_STACK_LIMIT)" \
		NEMACS_RUNTIME_IMAGE= \
		VENDOR_CLASS_A_LIMIT="$(VENDOR_CLASS_A_LIMIT)" \
		VENDOR_CLASS_A_STRICT="$(VENDOR_CLASS_A_STRICT)" \
		./bin/nemacs --driver=nelisp --batch --no-banner \
		-l "$(abspath scripts/vendor-class-a-smoke.el)" \
		--eval '(progn (setq vendor-class-a-smoke-default-limit $(VENDOR_CLASS_A_LIMIT)) (setq vendor-class-a-smoke-strict $(VENDOR_CLASS_A_STRICT_ELISP)) (vendor-class-a-smoke-batch))'

verify-vendor-core: build-nelisp-bootstrap
	test -x "$(NELISP_BIN)"
	ulimit -s "$(NELISP_STACK_LIMIT)" 2>/dev/null || true; \
	tmp=$$(mktemp "$${TMPDIR:-/tmp}/nemacs-vendor-core.XXXXXX.el"); \
	{ \
		printf '%s\n' ';;; standalone vendor-core embedded smoke'; \
		printf '%s\n' '(setq nelisp-emacs-vendor-root "$(abspath vendor)")'; \
		printf '%s\n' '(setq load-path (list "$(abspath src)" "$(abspath scripts)" "$(abspath vendor/emacs-lisp)" "$(abspath vendor/emacs-lisp/emacs-lisp)" "$(abspath vendor/emacs-lisp/vc)"))'; \
		cat "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))"; \
		printf '\n'; \
		cat "$(abspath scripts/vendor-core-smoke.el)"; \
		printf '%s\n' '(setq vendor-core-smoke-module-spec "$(VENDOR_CORE_MODULES)")'; \
		printf '%s\n' '(setq vendor-core-smoke-default-limit $(VENDOR_CORE_LIMIT))'; \
		printf '%s\n' '(setq vendor-core-smoke-strict $(VENDOR_CORE_STRICT_ELISP))'; \
		printf '%s\n' '(vendor-core-smoke-batch)'; \
		printf '%s\n' '42'; \
	} > "$$tmp"; \
	timeout $(NELISP_VENDOR_CORE_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		"$(NELISP_BIN)" "$$tmp"; \
	rc=$$?; \
	rm -f "$$tmp"; \
	if [ "$$rc" -eq 42 ]; then echo "VENDOR-CORE-STANDALONE=ok exit=42"; else echo "VENDOR-CORE-STANDALONE=fail exit=$$rc expected=42"; exit "$$rc"; fi

bench:
	$(EMACS) -L src -L bench $(NELISP_LOAD_PATH) \
		-l bench-redisplay.el \
		-f bench-redisplay-run-all

demo:
	$(EMACS) -L src -L demo $(NELISP_LOAD_PATH) \
		-l phase1-close-demo \
		--eval "(prin1 (phase1-close-demo-run))" \
		--eval "(terpri)"

demo-phase2:
	$(EMACS) -L src -L demo $(NELISP_LOAD_PATH) \
		-l phase2-close-demo \
		--eval "(prin1 (phase2-close-demo-run))" \
		--eval "(terpri)"

# Layer-2 self-containment: fetch + build NeLisp into vendor/nelisp/.
# Pure-Elisp NeLisp checkouts provide target/nelisp-standalone-reader.
nelisp:
	bin/build-nelisp

nelisp-rebuild:
	bin/build-nelisp --rebuild

nelisp-clean:
	rm -rf $(VENDOR_NELISP)

clean:
	find . -name "*.elc" -delete
