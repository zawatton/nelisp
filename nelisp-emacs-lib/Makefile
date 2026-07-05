## nelisp-emacs Makefile

EMACS = emacs --batch
# NeLisp is driven through the pure-Elisp standalone reader.
VENDOR_NELISP = vendor/nelisp
DEFAULT_NELISP_ROOT := $(firstword $(wildcard $(VENDOR_NELISP) ../nelisp $(HOME)/Notes/dev/nelisp))
NELISP_ROOT  ?= $(if $(DEFAULT_NELISP_ROOT),$(DEFAULT_NELISP_ROOT),$(VENDOR_NELISP))
DEFAULT_NELISP_BIN := $(firstword $(wildcard $(NELISP_ROOT)/target/nelisp $(NELISP_ROOT)/target/nelisp-standalone-reader build/nelisp-experiment))
NELISP_BIN   ?= $(if $(DEFAULT_NELISP_BIN),$(DEFAULT_NELISP_BIN),$(NELISP_ROOT)/target/nelisp)
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
NEMACS_SERVER_CLIENT_TIMEOUT ?= 180s
NELISP_STACK_LIMIT ?= unlimited
BUILD_DIR ?= build
NEMACS_BOOTSTRAP_BUNDLE ?= $(BUILD_DIR)/nemacs-bootstrap.el
NEMACS_BOOTSTRAP_REPL ?= $(BUILD_DIR)/nemacs-bootstrap.repl
NEMACS_IMAGE ?= $(BUILD_DIR)/nemacs-loadup.nli
NEMACS_RUNTIME_IMAGE ?= $(BUILD_DIR)/nemacs-runtime.nlri
NEMACS_INTERACTIVE_RUNTIME_IMAGE ?= $(BUILD_DIR)/nemacs-interactive-runtime.nlri
NEMACS_VENDOR_CORE_RUNTIME_IMAGE ?= $(BUILD_DIR)/nemacs-vendor-core-runtime.nlri
NEMACS_RUNTIME_PRELOAD ?= scripts/nemacs-runtime-image-preload.el
NEMACS_RUNTIME_PROCESS_PRELOAD ?= scripts/nemacs-runtime-process-preload.el
NEMACS_RUNTIME_FRAME_TAB_PRELOAD ?= scripts/nemacs-runtime-frame-tab-preload.el
NEMACS_RUNTIME_IMAGE_INPUT_INVENTORY ?= $(BUILD_DIR)/nemacs-runtime-image-input-inventory.tsv
NEMACS_RUNTIME_IMAGE_INPUT_SUMMARY ?= $(BUILD_DIR)/nemacs-runtime-image-input-inventory.org
NEMACS_GUI_KEYMAP_COVERAGE_TSV ?= $(BUILD_DIR)/nemacs-gui-keymap-coverage.tsv
NEMACS_GUI_KEYMAP_COVERAGE_SUMMARY ?= $(BUILD_DIR)/nemacs-gui-keymap-coverage-summary.org
NEMACS_GUI_KEYMAP_COVERAGE_MISSING_TSV ?= $(BUILD_DIR)/nemacs-gui-keymap-coverage-missing.tsv
NEMACS_GUI_KEYMAP_COVERAGE_COMMAND_MISSING_TSV ?= $(BUILD_DIR)/nemacs-gui-keymap-coverage-command-missing.tsv
NEMACS_GUI_KEYMAP_COVERAGE_DIFFERENT_TSV ?= $(BUILD_DIR)/nemacs-gui-keymap-coverage-different.tsv
NEMACS_GUI_BRIDGE_PROFILE_LOG ?= $(BUILD_DIR)/nemacs-gui-bridge-profile.log
NEMACS_GUI_BRIDGE_PROFILE_SUMMARY ?= $(BUILD_DIR)/nemacs-gui-bridge-profile-summary.org
NEMACS_GUI_BRIDGE_RUN_SHAPE ?= $(BUILD_DIR)/nemacs-gui-bridge-run-shape.org
NEMACS_GUI_BRIDGE_RUNTIME_INVENTORY ?= $(BUILD_DIR)/gui-bridge-runtime-inventory.tsv
NEMACS_STUB_FALLBACK_SKIP_INVENTORY ?= $(BUILD_DIR)/nemacs-stub-fallback-skip-inventory.tsv
NEMACS_STUB_FALLBACK_SKIP_SUMMARY ?= $(BUILD_DIR)/nemacs-stub-fallback-skip-summary.org
NEMACS_DIRTY_REVIEW_UNITS ?= $(BUILD_DIR)/nemacs-dirty-review-units.tsv
NEMACS_LIBRARY_BOUNDARY_REPORT ?= $(BUILD_DIR)/nemacs-library-boundary-report.tsv
NEMACS_LIBRARY_BOUNDARY_SUMMARY ?= $(BUILD_DIR)/nemacs-library-boundary-summary.org
NEMACS_LIBRARY_PACKAGE_DEPS ?= $(BUILD_DIR)/nemacs-library-package-deps.tsv
NEMACS_LIBRARY_PACKAGE_DEPS_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-deps-summary.org
NEMACS_LIBRARY_PACKAGE_MIGRATION_QUEUE ?= $(BUILD_DIR)/nemacs-library-package-migration-queue.tsv
NEMACS_LIBRARY_PACKAGE_MIGRATION_QUEUE_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-migration-queue.org
NEMACS_LIBRARY_PACKAGE_DESCRIPTORS ?= $(BUILD_DIR)/nemacs-library-package-descriptors.tsv
NEMACS_LIBRARY_PACKAGE_DESCRIPTORS_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-descriptors.org
NEMACS_LIBRARY_PACKAGE_GUIDE ?= $(BUILD_DIR)/nemacs-library-package-guide.tsv
NEMACS_LIBRARY_PACKAGE_GUIDE_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-guide.org
NEMACS_LIBRARY_PACKAGE_API ?= $(BUILD_DIR)/nemacs-library-package-api.tsv
NEMACS_LIBRARY_PACKAGE_API_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-api.org
NEMACS_LIBRARY_PACKAGE_CATALOG ?= $(BUILD_DIR)/nemacs-library-package-catalog.tsv
NEMACS_LIBRARY_PACKAGE_CATALOG_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-catalog.org
NEMACS_LIBRARY_COMPAT_API_POLICY ?= $(BUILD_DIR)/nemacs-library-compat-api-policy.tsv
NEMACS_LIBRARY_COMPAT_API_POLICY_SUMMARY ?= $(BUILD_DIR)/nemacs-library-compat-api-policy.org
NEMACS_LIBRARY_API_PROMOTION_QUEUE ?= $(BUILD_DIR)/nemacs-library-api-promotion-queue.tsv
NEMACS_LIBRARY_API_PROMOTION_QUEUE_SUMMARY ?= $(BUILD_DIR)/nemacs-library-api-promotion-queue.org
NEMACS_LIBRARY_PACKAGE_LAYOUT ?= $(BUILD_DIR)/nemacs-library-package-layout.tsv
NEMACS_LIBRARY_PACKAGE_LAYOUT_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-layout.org
NEMACS_LIBRARY_PACKAGE_SCAFFOLD ?= $(BUILD_DIR)/nemacs-library-package-scaffold.tsv
NEMACS_LIBRARY_PACKAGE_SCAFFOLD_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-scaffold.org
NEMACS_LIBRARY_APP_SCAFFOLD ?= $(BUILD_DIR)/nemacs-library-app-scaffold.tsv
NEMACS_LIBRARY_APP_SCAFFOLD_SUMMARY ?= $(BUILD_DIR)/nemacs-library-app-scaffold.org
NEMACS_LIBRARY_APP_BOUNDARY ?= $(BUILD_DIR)/nemacs-library-app-boundary.tsv
NEMACS_LIBRARY_APP_BOUNDARY_SUMMARY ?= $(BUILD_DIR)/nemacs-library-app-boundary.org
NEMACS_LIBRARY_PACKAGE_APP_REQUIRE_GUARD ?= $(BUILD_DIR)/nemacs-library-package-app-require-guard.tsv
NEMACS_LIBRARY_PACKAGE_APP_REQUIRE_GUARD_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-app-require-guard.org
NEMACS_LIBRARY_PACKAGE_METADATA ?= $(BUILD_DIR)/nemacs-library-package-metadata.tsv
NEMACS_LIBRARY_PACKAGE_METADATA_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-metadata.org
NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE ?= $(BUILD_DIR)/nemacs-library-package-install-smoke.tsv
NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-install-smoke.org
NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE_ROOT ?= $(BUILD_DIR)/nemacs-library-package-install-smoke/install
NEMACS_LIBRARY_PACKAGE_ARCHIVE ?= $(BUILD_DIR)/nemacs-library-package-archive.tsv
NEMACS_LIBRARY_PACKAGE_ARCHIVE_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-archive.org
NEMACS_LIBRARY_PACKAGE_ARCHIVE_ROOT ?= $(BUILD_DIR)/nemacs-library-package-archives
NEMACS_LIBRARY_PACKAGE_ARCHIVE_STAGING_ROOT ?= $(BUILD_DIR)/nemacs-library-package-archive-staging
NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE ?= $(BUILD_DIR)/nemacs-library-package-archive-smoke.tsv
NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-archive-smoke.org
NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE_ROOT ?= $(BUILD_DIR)/nemacs-library-package-archive-smoke/install
NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM ?= $(BUILD_DIR)/nemacs-library-package-archive-checksum.tsv
NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-archive-checksum.org
NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM_ROOT ?= $(BUILD_DIR)/nemacs-library-package-archive-checksum
NEMACS_LIBRARY_PACKAGE_ARCHIVE_INDEX ?= $(BUILD_DIR)/nemacs-library-package-archive-index.tsv
NEMACS_LIBRARY_PACKAGE_ARCHIVE_INDEX_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-archive-index.org
NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE ?= $(BUILD_DIR)/nemacs-library-package-index-smoke.tsv
NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-index-smoke.org
NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE_ROOT ?= $(BUILD_DIR)/nemacs-library-package-index-smoke/install
NEMACS_LIBRARY_PACKAGE_PUBLICATION_POLICY ?= $(BUILD_DIR)/nemacs-library-package-publication-policy.tsv
NEMACS_LIBRARY_PACKAGE_PUBLICATION_POLICY_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-publication-policy.org
NEMACS_LIBRARY_PACKAGE_RELEASE_KEY_POLICY ?= $(BUILD_DIR)/nemacs-library-package-release-key-policy.tsv
NEMACS_LIBRARY_PACKAGE_RELEASE_KEY_POLICY_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-release-key-policy.org
NEMACS_LIBRARY_PACKAGE_SIGNATURE_POLICY ?= $(BUILD_DIR)/nemacs-library-package-signature-policy.tsv
NEMACS_LIBRARY_PACKAGE_SIGNATURE_POLICY_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-signature-policy.org
NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SIGN ?= $(BUILD_DIR)/nemacs-library-package-signature-release-sign.tsv
NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SIGN_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-signature-release-sign.org
NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE ?= $(BUILD_DIR)/nemacs-library-package-signature-release.tsv
NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-signature-release.org
NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_MANIFEST ?= $(BUILD_DIR)/nemacs-library-package-release-bundle-manifest.tsv
NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_MANIFEST_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-release-bundle-manifest.org
NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_ROOT ?= $(BUILD_DIR)/nemacs-library-package-release-bundle
NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE ?= $(BUILD_DIR)/nemacs-library-package-release-bundle-smoke.tsv
NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-release-bundle-smoke.org
NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_ROOT ?= $(BUILD_DIR)/nemacs-library-package-release-bundle-smoke/install
NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_WORK_ROOT ?= $(BUILD_DIR)/nemacs-library-package-release-bundle-smoke
NEMACS_LIBRARY_PACKAGE_RELEASE_PUBLICATION_POLICY ?= $(BUILD_DIR)/nemacs-library-package-release-publication-policy.tsv
NEMACS_LIBRARY_PACKAGE_RELEASE_PUBLICATION_POLICY_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-release-publication-policy.org
NEMACS_LIBRARY_RELEASE_PUBLICATION_STRICT ?= 0
NEMACS_LIBRARY_RELEASE_PUBLICATION_STRICT_ELISP := $(if $(filter 1 t true yes,$(NEMACS_LIBRARY_RELEASE_PUBLICATION_STRICT)),t,nil)
NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT ?= $(BUILD_DIR)/nemacs-library-package-release-rehearsal
NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_KEY ?= $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-rehearsal-key.tsv
NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_KEY_SUMMARY ?= $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-rehearsal-key.org
NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_GNUPGHOME ?= $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/gnupg
NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_PUBLIC_KEY ?= $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/nemacs-library-release-public-key.asc
NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL ?= $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-rehearsal.tsv
NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_SUMMARY ?= $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-rehearsal.org
NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_KEY_UID ?= nelisp-emacs release rehearsal <nelisp-emacs-release-rehearsal@example.invalid>
NEMACS_LIBRARY_RELEASE_CONFIG ?= docs/release/nemacs-library-release.local.mk
-include $(NEMACS_LIBRARY_RELEASE_CONFIG)
NEMACS_LIBRARY_PACKAGE_RELEASE_CONFIG_CHECK ?= $(BUILD_DIR)/nemacs-library-package-release-config-check.tsv
NEMACS_LIBRARY_PACKAGE_RELEASE_CONFIG_CHECK_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-release-config-check.org
NEMACS_LIBRARY_RELEASE_BUNDLE_STRICT ?= 0
NEMACS_LIBRARY_RELEASE_BUNDLE_STRICT_ELISP := $(if $(filter 1 t true yes,$(NEMACS_LIBRARY_RELEASE_BUNDLE_STRICT)),t,nil)
NEMACS_LIBRARY_RELEASE_SIGNATURE_STRICT ?= 0
NEMACS_LIBRARY_RELEASE_SIGNATURE_STRICT_ELISP := $(if $(filter 1 t true yes,$(NEMACS_LIBRARY_RELEASE_SIGNATURE_STRICT)),t,nil)
NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_STRICT ?= 0
NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_STRICT_ELISP := $(if $(filter 1 t true yes,$(NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_STRICT)),t,nil)
NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT ?=
NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_FILE ?= docs/release/nemacs-library-release-public-key.asc
NEMACS_LIBRARY_RELEASE_SIGNATURE_SUFFIX ?= .sig
NEMACS_LIBRARY_RELEASE_SIGNATURE_ARMOR ?= 0
NEMACS_LIBRARY_RELEASE_SIGNATURE_ARMOR_ELISP := $(if $(filter 1 t true yes,$(NEMACS_LIBRARY_RELEASE_SIGNATURE_ARMOR)),t,nil)
NEMACS_LIBRARY_RELEASE_GPG_PROGRAM ?= gpg
NEMACS_LIBRARY_RELEASE_GNUPGHOME ?=

ifneq ($(strip $(NEMACS_LIBRARY_RELEASE_GNUPGHOME)),)
export GNUPGHOME := $(NEMACS_LIBRARY_RELEASE_GNUPGHOME)
endif
NEMACS_LIBRARY_PACKAGE_DEPENDENCY_PUBLICATION_POLICY ?= $(BUILD_DIR)/nemacs-library-package-dependency-publication-policy.tsv
NEMACS_LIBRARY_PACKAGE_DEPENDENCY_PUBLICATION_POLICY_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-dependency-publication-policy.org
NEMACS_LIBRARY_PACKAGE_LAZY_METADATA ?= $(BUILD_DIR)/nemacs-library-package-lazy-metadata.tsv
NEMACS_LIBRARY_PACKAGE_LAZY_METADATA_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-lazy-metadata.org
NEMACS_LIBRARY_PACKAGE_VENDOR_LOCK ?= $(BUILD_DIR)/nemacs-library-package-vendor-lock.tsv
NEMACS_LIBRARY_PACKAGE_VENDOR_LOCK_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-vendor-lock.org
NEMACS_LIBRARY_PACKAGE_VENDOR_RELEASE_LOCK ?= $(BUILD_DIR)/nemacs-library-package-vendor-release-lock.tsv
NEMACS_LIBRARY_PACKAGE_VENDOR_RELEASE_LOCK_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-vendor-release-lock.org
NEMACS_LIBRARY_VENDOR_RELEASE_STRICT ?= 0
NEMACS_LIBRARY_VENDOR_RELEASE_STRICT_ELISP := $(if $(filter 1 t true yes,$(NEMACS_LIBRARY_VENDOR_RELEASE_STRICT)),t,nil)
NEMACS_LIBRARY_PACKAGE_VERIFY ?= $(BUILD_DIR)/nemacs-library-package-verify.tsv
NEMACS_LIBRARY_PACKAGE_VERIFY_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-verify.org
NEMACS_PRODUCTION_RUNTIME_PATH_SUMMARY ?= $(BUILD_DIR)/nemacs-production-runtime-path.org
NEMACS_LIBRARY_PACKAGE_LOAD_PATH ?= $(shell sh scripts/nemacs-library-package-load-path.sh 2>/dev/null)
NEMACS_LIBRARY_APP_SCAFFOLD_LOAD_PATH ?= -L packages/nelisp-emacs-app-gui/lisp
NEMACS_LIBRARY_PACKAGE_APP_LOAD_PATH ?= $(NEMACS_LIBRARY_PACKAGE_LOAD_PATH) $(NEMACS_LIBRARY_APP_SCAFFOLD_LOAD_PATH)
NEMACS_LIBRARY_PACKAGE_HOST_ERT_PRELUDE ?= --eval '(require (quote ert))'
NEMACS_LIBRARY_PACKAGE_MANIFEST ?= $(BUILD_DIR)/nemacs-library-package-manifest.tsv
NEMACS_LIBRARY_PACKAGE_MANIFEST_SUMMARY ?= $(BUILD_DIR)/nemacs-library-package-manifest-summary.org
NEMACS_LIBRARY_CONTRACT ?= $(BUILD_DIR)/nemacs-library-contract.tsv
NEMACS_LIBRARY_CONTRACT_SUMMARY ?= $(BUILD_DIR)/nemacs-library-contract.org
NEMACS_PUBLIC_API_INVENTORY ?= $(BUILD_DIR)/nemacs-public-api-inventory.tsv
NEMACS_PUBLIC_API_SUMMARY ?= $(BUILD_DIR)/nemacs-public-api-summary.org
NEMACS_OWNERSHIP_COVERAGE ?= $(BUILD_DIR)/nemacs-ownership-coverage.tsv
NEMACS_OWNERSHIP_COVERAGE_SUMMARY ?= $(BUILD_DIR)/nemacs-ownership-coverage-summary.org
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
VENDOR_SOURCE_CACHE_DIR ?= $(abspath build/standalone-source-cache)
VENDOR_FORM_WALK_PRELOAD_FILES ?=
VENDOR_LOAD_PRELUDE ?= $(abspath $(NELISP_ROOT)/scripts/nelisp-stdlib-prelude.el)
VENDOR_FORM_WALK_PRELUDE ?= $(VENDOR_LOAD_PRELUDE)
VENDOR_REPL_PRELUDE ?= $(VENDOR_LOAD_PRELUDE)
VENDOR_LOAD_FILES ?= $(abspath vendor/emacs-lisp/emacs-lisp/lisp-mode.el) $(abspath vendor/emacs-lisp/isearch.el) $(abspath vendor/emacs-lisp/minibuffer.el) $(abspath vendor/emacs-lisp/progmodes/project.el) $(abspath vendor/emacs-lisp/simple.el) $(abspath vendor/emacs-lisp/files.el) $(abspath vendor/emacs-lisp/dired.el) $(abspath vendor/emacs-lisp/help-mode.el) $(abspath vendor/emacs-lisp/help-fns.el) $(abspath vendor/emacs-lisp/emacs-lisp/subr-x.el) $(abspath vendor/emacs-lisp/emacs-lisp/seq.el) $(abspath vendor/emacs-lisp/emacs-lisp/map.el) $(abspath vendor/emacs-lisp/case-table.el) $(abspath vendor/emacs-lisp/cdl.el) $(abspath vendor/emacs-lisp/emacs-lisp/range.el) $(abspath vendor/emacs-lisp/emacs-lisp/regi.el) $(abspath vendor/emacs-lisp/emacs-lisp/ring.el) $(abspath vendor/emacs-lisp/emacs-lisp/generator.el) $(abspath vendor/emacs-lisp/emacs-lisp/avl-tree.el) $(abspath vendor/emacs-lisp/ielm.el) $(abspath vendor/emacs-lisp/hex-util.el) $(abspath vendor/emacs-lisp/international/charprop.el) $(abspath vendor/emacs-lisp/international/charscript.el) $(abspath vendor/emacs-lisp/international/emoji-labels.el) $(abspath vendor/emacs-lisp/international/idna-mapping.el) $(abspath vendor/emacs-lisp/emacs-lisp/lisp.el) $(abspath vendor/emacs-lisp/emacs-lisp/map-ynp.el) $(abspath vendor/emacs-lisp/international/iso-transl.el) $(abspath src/emacs-translation-table.el) $(abspath vendor/emacs-lisp/international/cp51932.el) $(abspath vendor/emacs-lisp/international/eucjp-ms.el) $(abspath vendor/emacs-lisp/international/fontset.el) $(abspath vendor/emacs-lisp/international/ja-dic-utl.el) $(abspath vendor/emacs-lisp/format-spec.el) $(abspath vendor/emacs-lisp/org/org-version.el) $(abspath vendor/emacs-lisp/org/org-macs.el) $(abspath vendor/emacs-lisp/org/org-compat.el) $(abspath vendor/emacs-lisp/org/org-fold-core.el) $(abspath vendor/emacs-lisp/org/org-fold.el) $(abspath vendor/emacs-lisp/org/org-duration.el) $(abspath vendor/emacs-lisp/org/oc.el) $(abspath vendor/emacs-lisp/org/org-keys.el) $(abspath vendor/emacs-lisp/org/org-cycle.el) $(abspath vendor/emacs-lisp/org/org.el) $(abspath vendor/emacs-lisp/org/ol.el) $(abspath vendor/emacs-lisp/org/org-refile.el) $(abspath vendor/emacs-lisp/org/org-clock.el) $(abspath vendor/emacs-lisp/org/org-capture.el) $(abspath vendor/emacs-lisp/org/org-datetree.el) $(abspath vendor/emacs-lisp/org/org-archive.el) $(abspath vendor/emacs-lisp/org/org-agenda.el) $(abspath vendor/emacs-lisp/org/org-element-ast.el) $(abspath vendor/emacs-lisp/org/org-footnote.el) $(abspath vendor/emacs-lisp/org/org-list.el) $(abspath vendor/emacs-lisp/org/org-entities.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/version.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/help-macro.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/org-macro.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/ob-eval.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/org-faces.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/oc-bibtex.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/oc-natbib.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/oc-biblatex.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/org-inlinetask.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/ol-doi.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/ol-info.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/ol-man.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/ol-rmail.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/ol-mhe.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/ol-w3m.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/ol-irc.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/tempo.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/org/org-tempo.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/inline.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/easymenu.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/let-alist.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/radix-tree.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/text-property-search.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/thunk.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/env.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/fileloop.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/rmc.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/generate-lisp-file.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obarray.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/soundex.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/cursor-sensor.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/indent-aux.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/display-fill-column-indicator.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/thingatpt.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/calendar/time-date.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/calendar/iso8601.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/calendar/parse-time.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/uni-lowercase.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/uni-mirrored.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/uni-special-lowercase.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/uni-special-titlecase.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/uni-special-uppercase.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/uni-titlecase.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/uni-uppercase.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/tabify.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/rot13.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/textmodes/underline.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/widget.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/dos-vars.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mb-depth.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/ietf-drums.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/rfc2045.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/hmac-def.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/hmac-md5.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/rfc2104.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/md4.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/compat.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/shorthands.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/dynamic-setting.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/uni-decimal.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/uni-digit.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/uni-numeric.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/benchmark.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/password-cache.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/double.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/chistory.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/scroll-lock.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/thread.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/qp.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/mailheader.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/yenc.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/flow-fill.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/uudecode.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/tq.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/mail-prsvr.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/gnus/mm-util.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/rfc2047.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/rfc2231.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/mail-parse.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/rfc6068.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/mail-utils.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/rfc822.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/ietf-drums-date.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/binhex.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/sasl.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/sasl-cram.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/sasl-digest.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/sasl-scram-rfc.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/sasl-scram-sha256.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/ntlm.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/sasl-ntlm.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/image/compface.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/tramp-uu.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/trampver.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/bobcat.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/cygwin.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/vt200.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/linux.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/vt100.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/AT386.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/news.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/lk201.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/w32console.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/meese.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/ps-def.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/ps-print-loaddefs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/textmodes/glyphless-mode.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/textmodes/word-wrap-mode.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/sqlite.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/url/url-future.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/url/url-domsuf.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/vt100-led.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/khmer.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/cham.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/czech.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/slovak.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/georgian.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/sinhala.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/romanian.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/utf-8-lang.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/burmese.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/tai-viet.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/english.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/lao.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/greek.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/ethiopic.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/philippine.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/korean.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/vietnamese.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/thai.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/tv-util.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/cyril-util.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/indonesian.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/korea-util.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/china-util.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/cyrillic.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/hebrew.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/japanese.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/viet-util.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/chinese.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/japan-util.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/language/misc-lang.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/play/studly.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/play/dissociate.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/makesum.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/vt-control.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/flow-ctrl.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/talk.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/nxml/nxml-maint.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/nxml/nxml-util.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/vc/vc-filewise.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/pgg-def.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/progmodes/autoconf.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/gnus/gssapi.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/scroll-all.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/utf-7.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/rfc2368.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/timer-list.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/master.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/helper.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/calendar/holiday-loaddefs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/cedet/ede/loaddefs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/theme-loaddefs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/eshell/esh-module-loaddefs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/cedet/srecode/loaddefs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/calendar/diary-loaddefs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/textmodes/texinfo-loaddefs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/calc/calc-loaddefs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/rfc1843.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/nxml/nxml-enc.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/textmodes/bibtex-style.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/dictionary-connection.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/progmodes/m4-mode.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/play/cookie1.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/play/spook.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/yow.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/bruce.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/autoarg.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/tvi970.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/sun.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/subdirs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emulation/edt-lk201.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emulation/edt-vt100.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/nxml/rng-util.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/nxml/rng-dt.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/url/url-vars.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/url/url-privacy.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emulation/edt-pc.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/w32-vars.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/novice.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/textmodes/page.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/cl-compat.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/elide-head.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/iimage.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/textmodes/emacs-authors-mode.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/textsec-check.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/debug-early.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/calc/calc-macs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/kinsoku.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/latexenc.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/reposition.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/ansi-osc.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/play/morse.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mh-e/mh-buffers.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/cedet/ede/make.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/cedet/cedet-files.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/epa-hook.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/cedet/ede/makefile-edit.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/international/isearch-x.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/term/wyse50.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/gulp.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/vc/ediff-hook.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/progmodes/ld-script.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/dig.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/nxml/rng-pttrn.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/net/sieve-mode.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/progmodes/bat-mode.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/netrc.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/minibuf-eldef.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/visual-wrap.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/display-line-numbers.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mouse-copy.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/play/animate.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/gnus/gmm-utils.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/userlock.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/rfn-eshadow.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/progmodes/asm-mode.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/textmodes/bib-mode.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/reveal.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lock.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/linum.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/textmodes/refill.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/gnus/nnnil.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/textmodes/po.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/cedet/cedet.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/cc-compat.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/cedet/cedet-cscope.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/metamail.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/textmodes/string-edit.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/progmodes/flymake-cc.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/external-completion.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/yank-media.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/leim/quail/cyril-jis.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/cedet/cedet-idutils.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/sup-mouse.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/cedet/cedet-global.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/mantemp.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/vc/ediff-vers.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/gs.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/mail/unrmail.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/backquote.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/dirtrack.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emulation/keypad.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/rtree.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/progmodes/executable.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/shadow.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/progmodes/cl-font-lock.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/obsolete/starttls.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/vc/diff.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/dos-fns.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/crm.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/epg-config.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/progmodes/subword.el)
VENDOR_LOAD_FILES += $(abspath vendor/emacs-lisp/font-core.el)
VENDOR_AVL_PROOF_FORM ?= (let ((tree (avl-tree-create (quote <)))) (avl-tree-enter tree 2) (avl-tree-enter tree 1) (avl-tree-enter tree 3) (and (equal (avl-tree-flatten tree) (quote (1 2 3))) (= (avl-tree-member tree 2) 2) (let ((it (avl-tree-iter tree))) (and (= (iter-next it) 1) (= (iter-next it) 2) (= (iter-next it) 3)))))
VENDOR_LOAD_PROOF_FORM ?= (and (= vendor-standalone-load-ok-count vendor-standalone-load-file-count) (fboundp (quote dired)) (fboundp (quote describe-function)) (fboundp (quote project-current)) (symbol-function (quote find-file)) (symbol-function (quote save-buffer)) (symbol-function (quote write-file)) (fboundp (quote forward-sexp)) (fboundp (quote mark-defun)) (fboundp (quote map-y-or-n-p)) (fboundp (quote read-answer)) (featurep (quote ring)) (fboundp (quote ring-ref)) (featurep (quote generator)) (featurep (quote avl-tree)) (fboundp (quote avl-tree-create)) (fboundp (quote avl-tree-p)) (fboundp (quote avl-tree-iter)) $(VENDOR_AVL_PROOF_FORM) (boundp (quote emoji--derived)) (boundp (quote emoji--names)) (boundp (quote idna-mapping-table)) (vectorp idna-mapping-table) (string= (elt idna-mapping-table 65) (char-to-string 97)) (eq (elt idna-mapping-table 173) (quote ignored)) (string= (elt idna-mapping-table 8490) (char-to-string 107)) (boundp (quote iso-transl-char-map)) (boundp (quote iso-transl-language-alist)) (fboundp (quote iso-transl-define-keys)) (fboundp (quote iso-transl-set-language)) (featurep (quote cp51932)) (get (quote cp51932-decode) (quote translation-table)) (get (quote cp51932-encode) (quote translation-table)) (featurep (quote eucjp-ms)) (get (quote eucjp-ms-decode) (quote translation-table)) (get (quote eucjp-ms-encode) (quote translation-table)) (featurep (quote fontset)) (fboundp (quote x-decompose-font-name)) (fboundp (quote x-compose-font-name)) (fboundp (quote create-default-fontset)) (boundp (quote standard-fontset-spec)) (featurep (quote ja-dic-utl)) (fboundp (quote skkdic-lookup-key)) (boundp (quote skkdic-okurigana-table)) (featurep (quote format-spec)) (fboundp (quote format-spec)) (featurep (quote org-version)) (fboundp (quote org-release)) (fboundp (quote org-git-version)) (stringp (org-release)) (stringp (org-git-version)) (equal (length (org-release)) 6) (equal (length (org-git-version)) 14) (featurep (quote org-macs)) (fboundp (quote org-with-gensyms)) (fboundp (quote org-string-nw-p)) (featurep (quote org-compat)) (fboundp (quote org-string-equal-ignore-case)) (fboundp (quote org-version-check)) (fboundp (quote org-with-silent-modifications)) (featurep (quote org-fold-core)) (fboundp (quote org-fold-core-add-folding-spec)) (fboundp (quote org-fold-core-region)) (fboundp (quote org-fold-core-folded-p)) (featurep (quote org-fold)) (fboundp (quote org-fold-region)) (fboundp (quote org-fold-show-all)) (fboundp (quote org-fold-hide-subtree)) (featurep (quote org-duration)) (fboundp (quote org-duration-p)) (fboundp (quote org-duration-to-minutes)) (fboundp (quote org-duration-from-minutes)) (fboundp (quote org-duration-h:mm-only-p)) (featurep (quote org)) (featurep (quote org-capture)) (featurep (quote org-refile)) (featurep (quote org-datetree)) (featurep (quote org-archive)) (featurep (quote org-clock)) (featurep (quote ol)) (featurep (quote org-footnote)) (featurep (quote org-list)) (fboundp (quote org-list-to-lisp)) (featurep (quote org-entities)) (fboundp (quote org-entity-get)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote org-macro)) (fboundp (quote org-macro--makeargs)) (fboundp (quote org-macro--set-templates)) (fboundp (quote org-macro-initialize-templates)) (fboundp (quote org-macro-expand)) (fboundp (quote org-macro-replace-all)) (fboundp (quote org-macro-escape-arguments)) (fboundp (quote org-macro-extract-arguments)) (fboundp (quote org-macro--counter-increment)) (boundp (quote org-macro-templates)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote ob-eval)) (boundp (quote org-babel-error-buffer-name)) (fboundp (quote org-babel-eval-error-notify)) (fboundp (quote org-babel-eval)) (fboundp (quote org-babel-eval-read-file)) (fboundp (quote org-babel--shell-command-on-region)) (fboundp (quote org-babel--write-temp-buffer-input-file)) (fboundp (quote org-babel-eval-wipe-error-buffer)) (fboundp (quote org-babel--get-shell-file-name)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote org-faces)) (boundp (quote org-level-faces)) (fboundp (quote org-set-tag-faces)) (boundp (quote org-todo-keyword-faces)) (boundp (quote org-tag-faces)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote oc)) (fboundp (quote org-cite-register-processor)) (fboundp (quote org-cite-get-processor)) (fboundp (quote org-cite-processor-has-capability-p)) (boundp (quote org-cite--processors)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote org-keys)) (fboundp (quote org-key)) (fboundp (quote org-defkey)) (fboundp (quote org-remap)) (fboundp (quote org-speed-command-help)) (fboundp (quote org-speed-command-activate)) (boundp (quote org-mode-map)) (boundp (quote org-mouse-map)) (boundp (quote org-babel-map)) (boundp (quote org-speed-commands)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote org-cycle)) (fboundp (quote org-cycle)) (fboundp (quote org-cycle-global)) (fboundp (quote org-cycle-overview)) (fboundp (quote org-cycle-content)) (fboundp (quote org-cycle-hide-drawers)) (fboundp (quote org-cycle-force-archived)) (boundp (quote org-cycle-hook)) (boundp (quote org-cycle-global-status)) (boundp (quote org-cycle-subtree-status)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote org-inlinetask)) (fboundp (quote org-inlinetask-insert-task)) (fboundp (quote org-inlinetask-outline-regexp)) (fboundp (quote org-inlinetask-end-p)) (fboundp (quote org-inlinetask-at-task-p)) (fboundp (quote org-inlinetask-in-task-p)) (fboundp (quote org-inlinetask-goto-beginning)) (fboundp (quote org-inlinetask-goto-end)) (fboundp (quote org-inlinetask-get-task-level)) (fboundp (quote org-inlinetask-promote)) (fboundp (quote org-inlinetask-demote)) (fboundp (quote org-inlinetask-fontify)) (fboundp (quote org-inlinetask-toggle-visibility)) (fboundp (quote org-inlinetask-hide-tasks)) (fboundp (quote org-inlinetask-remove-END-maybe)) (boundp (quote org-inlinetask-min-level)) (boundp (quote org-inlinetask-show-first-star)) (boundp (quote org-inlinetask-default-state)) (stringp (org-inlinetask-outline-regexp)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote ol-doi)) (featurep (quote org-link-doi)) (fboundp (quote org-link-doi-open)) (fboundp (quote org-link-doi-export)) (boundp (quote org-link-doi-server-url)) (stringp org-link-doi-server-url) (stringp (org-link-doi-export (symbol-name (quote doi)) nil (quote html) nil)) (featurep (quote ol-info)) (fboundp (quote org-info-open)) (fboundp (quote org-info-store-link)) (fboundp (quote org-info--link-file-node)) (fboundp (quote org-info-description-as-command)) (fboundp (quote org-info-map-html-url)) (fboundp (quote org-info--expand-node-name)) (fboundp (quote org-info-export)) (boundp (quote org-info-emacs-documents)) (boundp (quote org-info-other-documents)) (consp (org-info--link-file-node (symbol-name (quote elisp)))) (stringp (org-info-map-html-url (symbol-name (quote elisp)))) (stringp (org-info--expand-node-name (symbol-name (quote node)))))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote ol-man)) (fboundp (quote org-man-open)) (fboundp (quote org-man-store-link)) (fboundp (quote org-man-get-page-name)) (fboundp (quote org-man-export)) (fboundp (quote org-man-complete)) (boundp (quote org-man-command)) (stringp (org-man-export (symbol-name (quote printf)) nil (quote html))) (featurep (quote ol-rmail)) (fboundp (quote org-rmail-store-link)) (fboundp (quote org-rmail-open)) (fboundp (quote org-rmail-follow-link)) (featurep (quote ol-mhe)) (fboundp (quote org-mhe-store-link)) (fboundp (quote org-mhe-open)) (fboundp (quote org-mhe-get-message-real-folder)) (fboundp (quote org-mhe-get-message-folder)) (fboundp (quote org-mhe-get-message-num)) (fboundp (quote org-mhe-get-header)) (fboundp (quote org-mhe-follow-link)) (boundp (quote org-mhe-search-all-folders)) (featurep (quote ol-w3m)) (fboundp (quote org-w3m-store-link)) (fboundp (quote org-w3m-copy-for-org-mode)) (fboundp (quote org-w3m-get-anchor-start)) (fboundp (quote org-w3m-get-next-link-start)) (fboundp (quote org-w3m-no-next-link-p)) (featurep (quote ol-irc)) (fboundp (quote org-irc-visit)) (fboundp (quote org-irc-parse-link)) (fboundp (quote org-irc-store-link)) (fboundp (quote org-irc-ellipsify-description)) (fboundp (quote org-irc-get-current-erc-port)) (fboundp (quote org-irc-export)) (boundp (quote org-irc-client)) (boundp (quote org-irc-link-to-logs)) (stringp (org-irc-export (symbol-name (quote server)) nil (quote html))))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote tempo)) (fboundp (quote tempo-define-template)) (fboundp (quote tempo-insert-template)) (fboundp (quote tempo-add-tag)) (fboundp (quote tempo-use-tag-list)) (fboundp (quote tempo-complete-tag)) (boundp (quote tempo-tags)) (boundp (quote tempo-local-tags)) (featurep (quote org-tempo)) (fboundp (quote org-tempo-setup)) (fboundp (quote org-tempo-add-templates)) (fboundp (quote org-tempo-add-block)) (fboundp (quote org-tempo-complete-tag)) (boundp (quote org-tempo-tags)) (boundp (quote org-tempo-keywords-alist)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote inline)) (fboundp (quote define-inline)) (fboundp (quote inline-quote)) (fboundp (quote inline-letevals)) (fboundp (quote inline-const-p)) (fboundp (quote inline-const-val)) (fboundp (quote inline-error)) (fboundp (quote inline--do-quote)) (fboundp (quote inline--do-leteval)) (fboundp (quote inline--testconst-p)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote easymenu)) (fboundp (quote easy-menu-define)) (fboundp (quote easy-menu-create-menu)) (fboundp (quote easy-menu-add-item)) (fboundp (quote easy-menu-remove-item)) (fboundp (quote easy-menu-lookup-name)) (boundp (quote easy-menu-avoid-duplicate-keys)) (boundp (quote easy-menu-converted-items-table)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote let-alist)) (fboundp (quote let-alist)) (fboundp (quote let-alist--deep-dot-search)) (fboundp (quote let-alist--access-sexp)) (featurep (quote radix-tree)) (boundp (quote radix-tree-empty)) (fboundp (quote radix-tree-insert)) (fboundp (quote radix-tree-lookup)) (fboundp (quote radix-tree-prefixes)) (fboundp (quote radix-tree-count)) (fboundp (quote radix-tree-from-map)) (featurep (quote text-property-search)) (fboundp (quote text-property-search-forward)) (fboundp (quote text-property-search-backward)) (fboundp (quote prop-match-beginning)) (fboundp (quote prop-match-end)) (fboundp (quote prop-match-value)) (featurep (quote thunk)) (fboundp (quote thunk-force)) (fboundp (quote thunk-evaluated-p)) (= (thunk-force (lambda (&optional check) (if check t 42))) 42) (thunk-evaluated-p (lambda (&optional check) (if check t 42))))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (boundp (quote emacs-major-version)) (boundp (quote emacs-minor-version)) (fboundp (quote emacs-version)) (fboundp (quote emacs-repository-get-version)) (fboundp (quote emacs-repository-get-branch)) (featurep (quote help-macro)) (fboundp (quote make-help-screen)) (fboundp (quote help--help-screen)) (featurep (quote env)) (fboundp (quote substitute-env-vars)) (fboundp (quote substitute-env-in-file-name)) (fboundp (quote setenv)) (fboundp (quote getenv)) (featurep (quote fileloop)) (fboundp (quote fileloop-initialize)) (fboundp (quote fileloop-next-file)) (fboundp (quote fileloop-continue)) (fboundp (quote fileloop-initialize-search)) (fboundp (quote fileloop-initialize-replace)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote rmc)) (fboundp (quote read-multiple-choice)) (fboundp (quote rmc--add-key-description)) (featurep (quote generate-lisp-file)) (fboundp (quote generate-lisp-file-heading)) (fboundp (quote generate-lisp-file-trailer)) (featurep (quote obarray)) (fboundp (quote obarray-size)) (fboundp (quote obarray-get)) (fboundp (quote obarray-put)) (fboundp (quote obarray-map)) (featurep (quote soundex)) (fboundp (quote soundex)) (featurep (quote cursor-sensor)) (fboundp (quote cursor-sensor-tangible-pos)) (fboundp (quote cursor-sensor--detect)) (featurep (quote indent-aux)) (fboundp (quote kill-ring-deindent-buffer-substring-function)) (featurep (quote display-fill-column-indicator)) (fboundp (quote display-fill-column-indicator--turn-on)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote thingatpt)) (fboundp (quote thing-at-point)) (fboundp (quote bounds-of-thing-at-point)) (fboundp (quote forward-thing)) (fboundp (quote word-at-point)) (fboundp (quote symbol-at-point)) (fboundp (quote number-at-point)) (fboundp (quote thing-at-point-looking-at)) (boundp (quote thing-at-point-provider-alist)) (boundp (quote forward-thing-provider-alist)) (boundp (quote bounds-of-thing-at-point-provider-alist)) (boundp (quote thing-at-point-email-regexp)) (boundp (quote thing-at-point-uuid-regexp)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote time-date)) (fboundp (quote date-to-time)) (fboundp (quote seconds-to-time)) (fboundp (quote days-to-time)) (fboundp (quote time-since)) (fboundp (quote date-to-day)) (fboundp (quote days-between)) (fboundp (quote date-leap-year-p)) (fboundp (quote time-to-day-in-year)) (fboundp (quote time-to-days)) (fboundp (quote time-to-number-of-days)) (fboundp (quote safe-date-to-time)) (fboundp (quote format-seconds)) (fboundp (quote seconds-to-string)) (fboundp (quote date-days-in-month)) (fboundp (quote date-ordinal-to-time)) (fboundp (quote decoded-time-add)) (fboundp (quote make-decoded-time)) (fboundp (quote decoded-time-set-defaults)) (fboundp (quote decoded-time-period)) (boundp (quote seconds-to-string)) (date-leap-year-p 2024) (not (date-leap-year-p 2100)) (= (date-days-in-month 2024 2) 29) (= (date-days-in-month 2023 2) 28))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote iso8601)) (fboundp (quote iso8601-parse)) (fboundp (quote iso8601-parse-date)) (fboundp (quote iso8601-parse-time)) (fboundp (quote iso8601-parse-zone)) (fboundp (quote iso8601-valid-p)) (fboundp (quote iso8601-parse-duration)) (fboundp (quote iso8601-parse-interval)) (boundp (quote iso8601--date-match)) (boundp (quote iso8601--time-match)) (boundp (quote iso8601--combined-match)) (boundp (quote iso8601--duration-match)) (featurep (quote parse-time)) (fboundp (quote parse-time-string)) (fboundp (quote parse-time-tokenize)) (fboundp (quote parse-iso8601-time-string)) (boundp (quote parse-time-months)) (boundp (quote parse-time-weekdays)) (boundp (quote parse-time-zoneinfo)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote tabify)) (fboundp (quote untabify)) (fboundp (quote tabify)) (featurep (quote rot13)) (fboundp (quote rot13-string)) (fboundp (quote rot13-region)) (featurep (quote underline)) (fboundp (quote underline-region)) (fboundp (quote ununderline-region)) (featurep (quote widget)) (fboundp (quote define-widget)) (fboundp (quote define-widget-keywords)) (featurep (quote dos-vars)) (boundp (quote msdos-shells)) (featurep (quote mb-depth)) (fboundp (quote minibuffer-depth-setup)) (boundp (quote minibuffer-depth-indicator-function)) (featurep (quote ietf-drums)) (fboundp (quote ietf-drums-parse-address)) (fboundp (quote ietf-drums-parse-addresses)) (featurep (quote rfc2045)) (fboundp (quote rfc2045-encode-string)) (featurep (quote hmac-def)) (fboundp (quote define-hmac-function)) (featurep (quote hmac-md5)) (fboundp (quote md5-binary)) (fboundp (quote hmac-md5)) (fboundp (quote hmac-md5-96)) (featurep (quote rfc2104)) (fboundp (quote rfc2104-hash)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote compat)) (fboundp (quote compat-function)) (fboundp (quote compat-call)) (fboundp (quote hack-read-symbol-shorthands)) (fboundp (quote shorthands-font-lock-shorthands)) (featurep (quote dynamic-setting)) (fboundp (quote font-setting-change-default-font)) (fboundp (quote dynamic-setting-handle-config-changed-event)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote benchmark)) (fboundp (quote benchmark-call)) (fboundp (quote benchmark-run)) (featurep (quote password-cache)) (fboundp (quote password-cache-add)) (fboundp (quote password-read-from-cache)) (let ((password-cache t)) (password-cache-add (symbol-name (quote nelisp-vendor-smoke)) (symbol-name (quote secret))) (equal (password-read-from-cache (symbol-name (quote nelisp-vendor-smoke))) (symbol-name (quote secret)))) (featurep (quote double)) (fboundp (quote double-translate-key)) (featurep (quote chistory)) (fboundp (quote command-history)) (fboundp (quote list-command-history)) (featurep (quote scroll-lock)) (fboundp (quote scroll-lock-next-line)) (featurep (quote thread)) (fboundp (quote list-threads)) (fboundp (quote thread-list--get-entries)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote qp)) (fboundp (quote quoted-printable-decode-region)) (fboundp (quote quoted-printable-encode-string)) (featurep (quote mailheader)) (fboundp (quote mail-header-extract)) (fboundp (quote mail-header-format)) (featurep (quote yenc)) (fboundp (quote yenc-decode-region)) (fboundp (quote yenc-parse-line)) (featurep (quote flow-fill)) (fboundp (quote fill-flowed)) (fboundp (quote fill-flowed-encode)) (featurep (quote uudecode)) (fboundp (quote uudecode-decode-region)) (fboundp (quote uudecode-decode-region-internal)) (featurep (quote tq)) (fboundp (quote tq-create)) (fboundp (quote tq-enqueue)) (fboundp (quote tq-filter)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote mail-prsvr)) (boundp (quote mail-parse-charset)) (featurep (quote mm-util)) (fboundp (quote mm-charset-to-coding-system)) (fboundp (quote mm-mime-charset)) (featurep (quote rfc2047)) (fboundp (quote rfc2047-encode-string)) (fboundp (quote rfc2047-decode-string)) (featurep (quote rfc2231)) (fboundp (quote rfc2231-parse-string)) (fboundp (quote rfc2231-encode-string)) (featurep (quote mail-parse)) (fboundp (quote mail-header-parse-addresses-lax)) (fboundp (quote mail-header-parse-address-lax)) (featurep (quote rfc6068)) (fboundp (quote rfc6068-parse-mailto-url)) (fboundp (quote rfc6068-unhexify-string)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote mail-utils)) (fboundp (quote mail-file-babyl-p)) (fboundp (quote mail-fetch-field)) (fboundp (quote mail-strip-quoted-names)) (featurep (quote rfc822)) (fboundp (quote rfc822-addresses)) (fboundp (quote rfc822-nuke-whitespace)) (featurep (quote ietf-drums-date)) (fboundp (quote ietf-drums-parse-date-string)) (featurep (quote binhex)) (fboundp (quote binhex-decode-region)) (fboundp (quote binhex-decode-region-internal)) (fboundp (quote binhex-string-big-endian)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote sasl)) (featurep (quote sasl-plain)) (featurep (quote sasl-login)) (featurep (quote sasl-anonymous)) (fboundp (quote sasl-make-client)) (fboundp (quote sasl-next-step)) (fboundp (quote sasl-find-mechanism)) (featurep (quote sasl-cram)) (fboundp (quote sasl-cram-md5-response)) (featurep (quote sasl-digest)) (fboundp (quote sasl-digest-md5-response)) (featurep (quote sasl-scram-rfc)) (featurep (quote sasl-scram-sha-1)) (fboundp (quote sasl-scram-sha-1-client-final-message)) (featurep (quote sasl-scram-sha256)) (fboundp (quote sasl-scram-sha-256-client-final-message)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote md4)) (fboundp (quote md4)) (featurep (quote ntlm)) (fboundp (quote ntlm-build-auth-request)) (fboundp (quote ntlm-build-auth-response)) (fboundp (quote ntlm-get-password-hashes)) (fboundp (quote ntlm-md4hash)) (featurep (quote sasl-ntlm)) (fboundp (quote sasl-ntlm-request)) (fboundp (quote sasl-ntlm-response)) (= (length (md4 (symbol-name (quote abc)) 3)) 16) (= (length (ntlm-build-auth-request (symbol-name (quote user)))) 36))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote compface)) (fboundp (quote uncompface)) (featurep (quote tramp-uu)) (fboundp (quote tramp-uuencode-region)) (fboundp (quote tramp-uu-byte-to-uu-char)) (fboundp (quote tramp-uu-b64-char-to-byte)) (featurep (quote trampver)) (boundp (quote tramp-version)) (stringp tramp-version) (fboundp (quote tramp-inside-emacs)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote term/bobcat)) (featurep (quote term/cygwin)) (featurep (quote term/vt200)) (featurep (quote term/linux)) (featurep (quote term/vt100)) (featurep (quote term/AT386)) (featurep (quote term/news)) (featurep (quote term/lk201)) (featurep (quote term/w32console)) (fboundp (quote terminal-init-bobcat)) (fboundp (quote terminal-init-cygwin)) (fboundp (quote terminal-init-vt200)) (fboundp (quote terminal-init-linux)) (fboundp (quote terminal-init-vt100)) (fboundp (quote terminal-init-AT386)) (fboundp (quote terminal-init-news)) (fboundp (quote terminal-init-lk201)) (fboundp (quote terminal-init-w32console)) (boundp (quote lk201-function-map)) (boundp (quote w32-tty-standard-colors)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote meese)) (fboundp (quote protect-innocence-hook)) (featurep (quote ps-def)) (fboundp (quote ps-mark-active-p)) (fboundp (quote ps-face-foreground-name)) (fboundp (quote ps-face-background-name)) (featurep (quote ps-print-loaddefs)) (boundp (quote ps-multibyte-buffer)) (featurep (quote glyphless-mode)) (boundp (quote glyphless-mode-types)) (fboundp (quote glyphless-mode--setup)) (featurep (quote word-wrap-mode)) (boundp (quote word-wrap-whitespace-characters)) (boundp (quote word-wrap-mode--previous-state)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote sqlite)) (fboundp (quote with-sqlite-transaction)) (featurep (quote url-future)) (fboundp (quote make-url-future)) (fboundp (quote url-future-call)) (featurep (quote url-domsuf)) (boundp (quote url-domsuf-domains)) (fboundp (quote url-domsuf-cookie-allowed-p)) (featurep (quote vt100-led)) (boundp (quote led-state)) (fboundp (quote led-on)) (fboundp (quote led-off)) (fboundp (quote led-flash)) (fboundp (quote led-update)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote cham)) (featurep (quote czech)) (featurep (quote slovak)) (featurep (quote georgian)) (featurep (quote romanian)) (featurep (quote utf-8-lang)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (boundp (quote burmese-composable-pattern)) (featurep (quote tai-viet)) (featurep (quote lao)) (featurep (quote greek)) (featurep (quote ethiopic)) (featurep (quote philippine)) (featurep (quote korean)) (featurep (quote vietnamese)) (featurep (quote thai)) (boundp (quote tai-tham-composable-pattern)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote tai-viet-util)) (boundp (quote tai-viet-re)) (fboundp (quote tai-viet-compose-region)) (featurep (quote cyril-util)) (boundp (quote cyrillic-language-alist)) (fboundp (quote standard-display-cyrillic-translit)) (featurep (quote indonesian)) (featurep (quote korea-util)) (fboundp (quote setup-korean-environment-internal)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote china-util)) (fboundp (quote decode-hz-region)) (fboundp (quote encode-hz-region)) (featurep (quote cyrillic)) (featurep (quote hebrew)) (fboundp (quote hebrew-shape-gstring)) (featurep (quote japanese)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote viet-util)) (fboundp (quote viet-decode-viqr-region)) (fboundp (quote viet-encode-viqr-region)) (featurep (quote chinese)) (featurep (quote japan-util)) (fboundp (quote setup-japanese-environment-internal)) (fboundp (quote japanese-katakana)) (fboundp (quote japanese-hiragana)) (featurep (quote misc-lang)) (fboundp (quote arabic-shape-gstring)) (fboundp (quote egyptian-shape-grouping)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote studly)) (fboundp (quote studlify-region)) (fboundp (quote studlify-word)) (featurep (quote dissociate)) (fboundp (quote dissociated-press)) (featurep (quote makesum)) (fboundp (quote make-command-summary)) (fboundp (quote double-column)) (featurep (quote vt-control)) (boundp (quote vt-applications-keypad-p)) (boundp (quote vt-wide-p)) (fboundp (quote vt-wide)) (fboundp (quote vt-narrow)) (fboundp (quote vt-toggle-screen)) (featurep (quote flow-ctrl)) (boundp (quote flow-control-c-s-replacement)) (boundp (quote flow-control-c-q-replacement)) (fboundp (quote enable-flow-control)) (fboundp (quote enable-flow-control-on)) (featurep (quote talk)) (boundp (quote talk-display-alist)) (fboundp (quote talk-connect)) (fboundp (quote talk)) (fboundp (quote talk-add-display)) (fboundp (quote talk-disconnect)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote nxml-maint)) (fboundp (quote nxml-insert-target-repertoire-glyph-set)) (featurep (quote nxml-util)) (fboundp (quote nxml-make-namespace)) (fboundp (quote nxml-namespace-name)) (featurep (quote vc-filewise)) (fboundp (quote vc-master-name)) (fboundp (quote vc-filewise-registered)) (featurep (quote pgg-def)) (boundp (quote pgg-default-scheme)) (fboundp (quote pgg-truncate-key-identifier)) (featurep (quote autoconf)) (featurep (quote autoconf-mode)) (fboundp (quote autoconf-mode)) (fboundp (quote autoconf-current-defun-function)) (featurep (quote gssapi)) (fboundp (quote open-gssapi-stream)) (featurep (quote scroll-all)) (fboundp (quote scroll-all-function-all)) (fboundp (quote scroll-all-check-to-scroll)) (featurep (quote utf-7)) (fboundp (quote utf-7-decode)) (fboundp (quote utf-7-encode)) (featurep (quote rfc2368)) (fboundp (quote rfc2368-unhexify-string)) (fboundp (quote rfc2368-parse-mailto-url)) (featurep (quote timer-list)) (fboundp (quote list-timers)) (fboundp (quote timer-list-cancel)) (featurep (quote master)) (fboundp (quote master-set-slave)) (fboundp (quote master-says)) (featurep (quote helper)) (fboundp (quote Helper-help)) (fboundp (quote Helper-describe-function)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote holiday-loaddefs)) (featurep (quote theme-loaddefs)) (featurep (quote esh-module-loaddefs)) (featurep (quote loaddefs)) (boundp (quote global-srecode-minor-mode)) (featurep (quote diary-loaddefs)) (featurep (quote texinfo-loaddefs)) (featurep (quote calc-loaddefs)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote rfc1843)) (fboundp (quote rfc1843-decode-string)) (featurep (quote nxml-enc)) (fboundp (quote nxml-detect-coding-system)) (featurep (quote bibtex-style)) (fboundp (quote bibtex-style-mode)) (featurep (quote dictionary-connection)) (fboundp (quote dictionary-connection-create-data)) (featurep (quote m4-mode)) (fboundp (quote m4-mode)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote cookie1)) (fboundp (quote cookie)) (fboundp (quote cookie-read)) (featurep (quote spook)) (fboundp (quote spook)) (featurep (quote yow)) (fboundp (quote yow)) (featurep (quote bruce)) (fboundp (quote bruce)) (featurep (quote autoarg)) (fboundp (quote autoarg-kp-digit-argument)) (featurep (quote term/tvi970)) (fboundp (quote terminal-init-tvi970)) (featurep (quote term/sun)) (fboundp (quote terminal-init-sun)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (boundp (quote *EDT-keys*)) (fboundp (quote edt-set-term-width-80)) (fboundp (quote edt-set-term-width-132)) (featurep (quote rng-util)) (fboundp (quote rng-make-datatypes-uri)) (fboundp (quote rng-escape-string)) (featurep (quote rng-dt)) (fboundp (quote rng-dt-builtin-compile)) (featurep (quote url-vars)) (boundp (quote url-privacy-level)) (boundp (quote url-user-agent)) (featurep (quote url-privacy)) (fboundp (quote url-device-type)) (fboundp (quote url-setup-privacy-info)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote w32-vars)) (boundp (quote w32-use-w32-font-dialog)) (boundp (quote w32-fixed-font-alist)) (featurep (quote novice)) (fboundp (quote disabled-command-function)) (fboundp (quote enable-command)) (fboundp (quote disable-command)) (featurep (quote page)) (fboundp (quote forward-page)) (fboundp (quote backward-page)) (fboundp (quote what-page)) (featurep (quote cl-compat)) (fboundp (quote keyword-of)) (fboundp (quote setnth)) (featurep (quote elide-head)) (fboundp (quote elide-head)) (fboundp (quote elide-head-show)) (featurep (quote iimage)) (fboundp (quote iimage-recenter)) (fboundp (quote iimage-mode-buffer)) (featurep (quote emacs-authors-mode)) (fboundp (quote emacs-authors-next-author)) (fboundp (quote emacs-authors-prev-author)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote textsec-check)) (boundp (quote textsec-check)) (fboundp (quote textsec-suspicious-p)) (fboundp (quote debug-early)) (fboundp (quote debug-early-backtrace)) (featurep (quote calc-macs)) (fboundp (quote calc-wrapper)) (fboundp (quote math-with-extra-prec)) (featurep (quote kinsoku)) (boundp (quote kinsoku-limit)) (fboundp (quote kinsoku)) (featurep (quote latexenc)) (fboundp (quote latexenc-inputenc-to-coding-system)) (fboundp (quote latexenc-coding-system-to-inputenc)) (featurep (quote reposition)) (fboundp (quote reposition-window)) (fboundp (quote repos-count-screen-lines)) (featurep (quote ansi-osc)) (boundp (quote ansi-osc-control-seq-regexp)) (fboundp (quote ansi-osc-filter-region)) (fboundp (quote ansi-osc-apply-on-region)) (featurep (quote morse)) (boundp (quote morse-code)) (fboundp (quote morse-region)) (fboundp (quote unmorse-region)) (fboundp (quote nato-region)) (fboundp (quote denato-region)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote mh-buffers)) (boundp (quote mh-log-buffer)) (fboundp (quote mh-truncate-log-buffer)) (featurep (quote ede/make)) (fboundp (quote ede-make-check-version)) (featurep (quote cedet-files)) (fboundp (quote cedet-directory-name-to-file-name)) (fboundp (quote cedet-file-name-to-directory-name)) (featurep (quote epa-hook)) (boundp (quote epa-file-handler)) (fboundp (quote epa-file-name-regexp-update)) (featurep (quote ede/makefile-edit)) (fboundp (quote makefile-macro-file-list)) (fboundp (quote makefile-extract-varname-from-text)))
# Accumulated true-load proof is count-only; detailed feature/function
# surfaces are covered by isolated/smaller proof partitions.  The single
# giant proof expression currently segfaults in standalone proof evaluation.
VENDOR_LOAD_PROOF_FORM := (and (= vendor-standalone-load-ok-count vendor-standalone-load-file-count) (= vendor-standalone-load-ok-count 319))
VENDOR_LOAD_TIMEOUT ?= 900s
VENDOR_REPL_EXCLUDE_FILES ?= $(abspath vendor/emacs-lisp/international/cp51932.el) $(abspath vendor/emacs-lisp/international/eucjp-ms.el)
VENDOR_REPL_EXCLUDE_FILES += $(abspath vendor/emacs-lisp/emacs-lisp/backquote.el)
VENDOR_REPL_EXCLUDE_FILES += $(abspath vendor/emacs-lisp/progmodes/executable.el) $(abspath vendor/emacs-lisp/emacs-lisp/shadow.el) $(abspath vendor/emacs-lisp/progmodes/cl-font-lock.el) $(abspath vendor/emacs-lisp/obsolete/starttls.el) $(abspath vendor/emacs-lisp/vc/diff.el) $(abspath vendor/emacs-lisp/dos-fns.el) $(abspath vendor/emacs-lisp/emacs-lisp/crm.el) $(abspath vendor/emacs-lisp/epg-config.el)
VENDOR_REPL_EXCLUDE_FILES += $(abspath vendor/emacs-lisp/org/ob-eval.el) $(abspath vendor/emacs-lisp/org/oc-bibtex.el) $(abspath vendor/emacs-lisp/org/oc-natbib.el) $(abspath vendor/emacs-lisp/org/oc-biblatex.el)
VENDOR_REPL_FILES ?= $(filter-out $(VENDOR_REPL_EXCLUDE_FILES),$(VENDOR_LOAD_FILES)) $(abspath src/cp51932.el) $(abspath src/eucjp-ms.el)
VENDOR_REPL_PROOF_FORM ?= (and (= vendor-standalone-load-ok-count vendor-standalone-load-file-count) (fboundp (quote dired)) (fboundp (quote describe-function)) (fboundp (quote project-current)) (symbol-function (quote find-file)) (symbol-function (quote save-buffer)) (symbol-function (quote write-file)) (fboundp (quote forward-sexp)) (fboundp (quote mark-defun)) (fboundp (quote map-y-or-n-p)) (fboundp (quote read-answer)) (featurep (quote ring)) (fboundp (quote ring-ref)) (featurep (quote generator)) (featurep (quote avl-tree)) (fboundp (quote avl-tree-create)) (fboundp (quote avl-tree-p)) (fboundp (quote avl-tree-iter)) $(VENDOR_AVL_PROOF_FORM) (boundp (quote emoji--derived)) (boundp (quote emoji--names)) (boundp (quote idna-mapping-table)) (vectorp idna-mapping-table) (string= (elt idna-mapping-table 65) (char-to-string 97)) (eq (elt idna-mapping-table 173) (quote ignored)) (string= (elt idna-mapping-table 8490) (char-to-string 107)) (boundp (quote iso-transl-char-map)) (boundp (quote iso-transl-language-alist)) (fboundp (quote iso-transl-define-keys)) (fboundp (quote iso-transl-set-language)) (featurep (quote cp51932)) (get (quote cp51932-decode) (quote translation-table)) (get (quote cp51932-encode) (quote translation-table)) (featurep (quote eucjp-ms)) (get (quote eucjp-ms-decode) (quote translation-table)) (get (quote eucjp-ms-encode) (quote translation-table)) (featurep (quote fontset)) (fboundp (quote x-decompose-font-name)) (fboundp (quote x-compose-font-name)) (fboundp (quote create-default-fontset)) (boundp (quote standard-fontset-spec)) (featurep (quote ja-dic-utl)) (fboundp (quote skkdic-lookup-key)) (boundp (quote skkdic-okurigana-table)) (featurep (quote format-spec)) (fboundp (quote format-spec)) (featurep (quote org-version)) (fboundp (quote org-release)) (fboundp (quote org-git-version)) (stringp (org-release)) (stringp (org-git-version)) (equal (length (org-release)) 6) (equal (length (org-git-version)) 14) (featurep (quote org-macs)) (fboundp (quote org-with-gensyms)) (fboundp (quote org-string-nw-p)) (featurep (quote org-compat)) (fboundp (quote org-string-equal-ignore-case)) (fboundp (quote org-version-check)) (fboundp (quote org-with-silent-modifications)) (featurep (quote org-fold-core)) (fboundp (quote org-fold-core-add-folding-spec)) (fboundp (quote org-fold-core-region)) (fboundp (quote org-fold-core-folded-p)) (featurep (quote org-fold)) (fboundp (quote org-fold-region)) (fboundp (quote org-fold-show-all)) (fboundp (quote org-fold-hide-subtree)) (featurep (quote org-duration)) (fboundp (quote org-duration-p)) (fboundp (quote org-duration-to-minutes)) (fboundp (quote org-duration-from-minutes)) (fboundp (quote org-duration-h:mm-only-p)) (featurep (quote org)) (featurep (quote org-capture)) (featurep (quote org-refile)) (featurep (quote org-datetree)) (featurep (quote org-archive)) (featurep (quote org-clock)) (featurep (quote ol)) (featurep (quote org-footnote)) (featurep (quote org-list)) (fboundp (quote org-list-to-lisp)) (featurep (quote org-entities)) (fboundp (quote org-entity-get)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote org-macro)) (fboundp (quote org-macro--makeargs)) (fboundp (quote org-macro--set-templates)) (fboundp (quote org-macro-initialize-templates)) (fboundp (quote org-macro-expand)) (fboundp (quote org-macro-replace-all)) (fboundp (quote org-macro-escape-arguments)) (fboundp (quote org-macro-extract-arguments)) (fboundp (quote org-macro--counter-increment)) (boundp (quote org-macro-templates)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote ob-eval)) (boundp (quote org-babel-error-buffer-name)) (fboundp (quote org-babel-eval-error-notify)) (fboundp (quote org-babel-eval)) (fboundp (quote org-babel-eval-read-file)) (fboundp (quote org-babel--shell-command-on-region)) (fboundp (quote org-babel--write-temp-buffer-input-file)) (fboundp (quote org-babel-eval-wipe-error-buffer)) (fboundp (quote org-babel--get-shell-file-name)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote org-faces)) (boundp (quote org-level-faces)) (fboundp (quote org-set-tag-faces)) (boundp (quote org-todo-keyword-faces)) (boundp (quote org-tag-faces)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote oc)) (fboundp (quote org-cite-register-processor)) (fboundp (quote org-cite-get-processor)) (fboundp (quote org-cite-processor-has-capability-p)) (boundp (quote org-cite--processors)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote org-keys)) (fboundp (quote org-key)) (fboundp (quote org-defkey)) (fboundp (quote org-remap)) (fboundp (quote org-speed-command-help)) (fboundp (quote org-speed-command-activate)) (boundp (quote org-mode-map)) (boundp (quote org-mouse-map)) (boundp (quote org-babel-map)) (boundp (quote org-speed-commands)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote org-cycle)) (fboundp (quote org-cycle)) (fboundp (quote org-cycle-global)) (fboundp (quote org-cycle-overview)) (fboundp (quote org-cycle-content)) (fboundp (quote org-cycle-hide-drawers)) (fboundp (quote org-cycle-force-archived)) (boundp (quote org-cycle-hook)) (boundp (quote org-cycle-global-status)) (boundp (quote org-cycle-subtree-status)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote org-inlinetask)) (fboundp (quote org-inlinetask-insert-task)) (fboundp (quote org-inlinetask-outline-regexp)) (fboundp (quote org-inlinetask-end-p)) (fboundp (quote org-inlinetask-at-task-p)) (fboundp (quote org-inlinetask-in-task-p)) (fboundp (quote org-inlinetask-goto-beginning)) (fboundp (quote org-inlinetask-goto-end)) (fboundp (quote org-inlinetask-get-task-level)) (fboundp (quote org-inlinetask-promote)) (fboundp (quote org-inlinetask-demote)) (fboundp (quote org-inlinetask-fontify)) (fboundp (quote org-inlinetask-toggle-visibility)) (fboundp (quote org-inlinetask-hide-tasks)) (fboundp (quote org-inlinetask-remove-END-maybe)) (boundp (quote org-inlinetask-min-level)) (boundp (quote org-inlinetask-show-first-star)) (boundp (quote org-inlinetask-default-state)) (stringp (org-inlinetask-outline-regexp)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote ol-doi)) (featurep (quote org-link-doi)) (fboundp (quote org-link-doi-open)) (fboundp (quote org-link-doi-export)) (boundp (quote org-link-doi-server-url)) (stringp org-link-doi-server-url) (stringp (org-link-doi-export (symbol-name (quote doi)) nil (quote html) nil)) (featurep (quote ol-info)) (fboundp (quote org-info-open)) (fboundp (quote org-info-store-link)) (fboundp (quote org-info--link-file-node)) (fboundp (quote org-info-description-as-command)) (fboundp (quote org-info-map-html-url)) (fboundp (quote org-info--expand-node-name)) (fboundp (quote org-info-export)) (boundp (quote org-info-emacs-documents)) (boundp (quote org-info-other-documents)) (consp (org-info--link-file-node (symbol-name (quote elisp)))) (stringp (org-info-map-html-url (symbol-name (quote elisp)))) (stringp (org-info--expand-node-name (symbol-name (quote node)))))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote ol-man)) (fboundp (quote org-man-open)) (fboundp (quote org-man-store-link)) (fboundp (quote org-man-get-page-name)) (fboundp (quote org-man-export)) (fboundp (quote org-man-complete)) (boundp (quote org-man-command)) (stringp (org-man-export (symbol-name (quote printf)) nil (quote html))) (featurep (quote ol-rmail)) (fboundp (quote org-rmail-store-link)) (fboundp (quote org-rmail-open)) (fboundp (quote org-rmail-follow-link)) (featurep (quote ol-mhe)) (fboundp (quote org-mhe-store-link)) (fboundp (quote org-mhe-open)) (fboundp (quote org-mhe-get-message-real-folder)) (fboundp (quote org-mhe-get-message-folder)) (fboundp (quote org-mhe-get-message-num)) (fboundp (quote org-mhe-get-header)) (fboundp (quote org-mhe-follow-link)) (boundp (quote org-mhe-search-all-folders)) (featurep (quote ol-w3m)) (fboundp (quote org-w3m-store-link)) (fboundp (quote org-w3m-copy-for-org-mode)) (fboundp (quote org-w3m-get-anchor-start)) (fboundp (quote org-w3m-get-next-link-start)) (fboundp (quote org-w3m-no-next-link-p)) (featurep (quote ol-irc)) (fboundp (quote org-irc-visit)) (fboundp (quote org-irc-parse-link)) (fboundp (quote org-irc-store-link)) (fboundp (quote org-irc-ellipsify-description)) (fboundp (quote org-irc-get-current-erc-port)) (fboundp (quote org-irc-export)) (boundp (quote org-irc-client)) (boundp (quote org-irc-link-to-logs)) (stringp (org-irc-export (symbol-name (quote server)) nil (quote html))))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote tempo)) (fboundp (quote tempo-define-template)) (fboundp (quote tempo-insert-template)) (fboundp (quote tempo-add-tag)) (fboundp (quote tempo-use-tag-list)) (fboundp (quote tempo-complete-tag)) (boundp (quote tempo-tags)) (boundp (quote tempo-local-tags)) (featurep (quote org-tempo)) (fboundp (quote org-tempo-setup)) (fboundp (quote org-tempo-add-templates)) (fboundp (quote org-tempo-add-block)) (fboundp (quote org-tempo-complete-tag)) (boundp (quote org-tempo-tags)) (boundp (quote org-tempo-keywords-alist)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote inline)) (fboundp (quote define-inline)) (fboundp (quote inline-quote)) (fboundp (quote inline-letevals)) (fboundp (quote inline-const-p)) (fboundp (quote inline-const-val)) (fboundp (quote inline-error)) (fboundp (quote inline--do-quote)) (fboundp (quote inline--do-leteval)) (fboundp (quote inline--testconst-p)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote easymenu)) (fboundp (quote easy-menu-define)) (fboundp (quote easy-menu-create-menu)) (fboundp (quote easy-menu-add-item)) (fboundp (quote easy-menu-remove-item)) (fboundp (quote easy-menu-lookup-name)) (boundp (quote easy-menu-avoid-duplicate-keys)) (boundp (quote easy-menu-converted-items-table)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote let-alist)) (fboundp (quote let-alist)) (fboundp (quote let-alist--deep-dot-search)) (fboundp (quote let-alist--access-sexp)) (featurep (quote radix-tree)) (boundp (quote radix-tree-empty)) (fboundp (quote radix-tree-insert)) (fboundp (quote radix-tree-lookup)) (fboundp (quote radix-tree-prefixes)) (fboundp (quote radix-tree-count)) (fboundp (quote radix-tree-from-map)) (featurep (quote text-property-search)) (fboundp (quote text-property-search-forward)) (fboundp (quote text-property-search-backward)) (fboundp (quote prop-match-beginning)) (fboundp (quote prop-match-end)) (fboundp (quote prop-match-value)) (featurep (quote thunk)) (fboundp (quote thunk-force)) (fboundp (quote thunk-evaluated-p)) (= (thunk-force (lambda (&optional check) (if check t 42))) 42) (thunk-evaluated-p (lambda (&optional check) (if check t 42))))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (boundp (quote emacs-major-version)) (boundp (quote emacs-minor-version)) (fboundp (quote emacs-version)) (fboundp (quote emacs-repository-get-version)) (fboundp (quote emacs-repository-get-branch)) (featurep (quote help-macro)) (fboundp (quote make-help-screen)) (fboundp (quote help--help-screen)) (featurep (quote env)) (fboundp (quote substitute-env-vars)) (fboundp (quote substitute-env-in-file-name)) (fboundp (quote setenv)) (fboundp (quote getenv)) (featurep (quote fileloop)) (fboundp (quote fileloop-initialize)) (fboundp (quote fileloop-next-file)) (fboundp (quote fileloop-continue)) (fboundp (quote fileloop-initialize-search)) (fboundp (quote fileloop-initialize-replace)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote rmc)) (fboundp (quote read-multiple-choice)) (fboundp (quote rmc--add-key-description)) (featurep (quote generate-lisp-file)) (fboundp (quote generate-lisp-file-heading)) (fboundp (quote generate-lisp-file-trailer)) (featurep (quote obarray)) (fboundp (quote obarray-size)) (fboundp (quote obarray-get)) (fboundp (quote obarray-put)) (fboundp (quote obarray-map)) (featurep (quote soundex)) (fboundp (quote soundex)) (featurep (quote cursor-sensor)) (fboundp (quote cursor-sensor-tangible-pos)) (fboundp (quote cursor-sensor--detect)) (featurep (quote indent-aux)) (fboundp (quote kill-ring-deindent-buffer-substring-function)) (featurep (quote display-fill-column-indicator)) (fboundp (quote display-fill-column-indicator--turn-on)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote thingatpt)) (fboundp (quote thing-at-point)) (fboundp (quote bounds-of-thing-at-point)) (fboundp (quote forward-thing)) (fboundp (quote word-at-point)) (fboundp (quote symbol-at-point)) (fboundp (quote number-at-point)) (fboundp (quote thing-at-point-looking-at)) (boundp (quote thing-at-point-provider-alist)) (boundp (quote forward-thing-provider-alist)) (boundp (quote bounds-of-thing-at-point-provider-alist)) (boundp (quote thing-at-point-email-regexp)) (boundp (quote thing-at-point-uuid-regexp)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote time-date)) (fboundp (quote date-to-time)) (fboundp (quote seconds-to-time)) (fboundp (quote days-to-time)) (fboundp (quote time-since)) (fboundp (quote date-to-day)) (fboundp (quote days-between)) (fboundp (quote date-leap-year-p)) (fboundp (quote time-to-day-in-year)) (fboundp (quote time-to-days)) (fboundp (quote time-to-number-of-days)) (fboundp (quote safe-date-to-time)) (fboundp (quote format-seconds)) (fboundp (quote seconds-to-string)) (fboundp (quote date-days-in-month)) (fboundp (quote date-ordinal-to-time)) (fboundp (quote decoded-time-add)) (fboundp (quote make-decoded-time)) (fboundp (quote decoded-time-set-defaults)) (fboundp (quote decoded-time-period)) (boundp (quote seconds-to-string)) (date-leap-year-p 2024) (not (date-leap-year-p 2100)) (= (date-days-in-month 2024 2) 29) (= (date-days-in-month 2023 2) 28))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote iso8601)) (fboundp (quote iso8601-parse)) (fboundp (quote iso8601-parse-date)) (fboundp (quote iso8601-parse-time)) (fboundp (quote iso8601-parse-zone)) (fboundp (quote iso8601-valid-p)) (fboundp (quote iso8601-parse-duration)) (fboundp (quote iso8601-parse-interval)) (boundp (quote iso8601--date-match)) (boundp (quote iso8601--time-match)) (boundp (quote iso8601--combined-match)) (boundp (quote iso8601--duration-match)) (featurep (quote parse-time)) (fboundp (quote parse-time-string)) (fboundp (quote parse-time-tokenize)) (fboundp (quote parse-iso8601-time-string)) (boundp (quote parse-time-months)) (boundp (quote parse-time-weekdays)) (boundp (quote parse-time-zoneinfo)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote tabify)) (fboundp (quote untabify)) (fboundp (quote tabify)) (featurep (quote rot13)) (fboundp (quote rot13-string)) (fboundp (quote rot13-region)) (featurep (quote underline)) (fboundp (quote underline-region)) (fboundp (quote ununderline-region)) (featurep (quote widget)) (fboundp (quote define-widget)) (fboundp (quote define-widget-keywords)) (featurep (quote dos-vars)) (boundp (quote msdos-shells)) (featurep (quote mb-depth)) (fboundp (quote minibuffer-depth-setup)) (boundp (quote minibuffer-depth-indicator-function)) (featurep (quote ietf-drums)) (fboundp (quote ietf-drums-parse-address)) (fboundp (quote ietf-drums-parse-addresses)) (featurep (quote rfc2045)) (fboundp (quote rfc2045-encode-string)) (featurep (quote hmac-def)) (fboundp (quote define-hmac-function)) (featurep (quote hmac-md5)) (fboundp (quote md5-binary)) (fboundp (quote hmac-md5)) (fboundp (quote hmac-md5-96)) (featurep (quote rfc2104)) (fboundp (quote rfc2104-hash)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote compat)) (fboundp (quote compat-function)) (fboundp (quote compat-call)) (fboundp (quote hack-read-symbol-shorthands)) (fboundp (quote shorthands-font-lock-shorthands)) (featurep (quote dynamic-setting)) (fboundp (quote font-setting-change-default-font)) (fboundp (quote dynamic-setting-handle-config-changed-event)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote benchmark)) (fboundp (quote benchmark-call)) (fboundp (quote benchmark-run)) (featurep (quote password-cache)) (fboundp (quote password-cache-add)) (fboundp (quote password-read-from-cache)) (let ((password-cache t)) (password-cache-add (symbol-name (quote nelisp-vendor-smoke)) (symbol-name (quote secret))) (equal (password-read-from-cache (symbol-name (quote nelisp-vendor-smoke))) (symbol-name (quote secret)))) (featurep (quote double)) (fboundp (quote double-translate-key)) (featurep (quote chistory)) (fboundp (quote command-history)) (fboundp (quote list-command-history)) (featurep (quote scroll-lock)) (fboundp (quote scroll-lock-next-line)) (featurep (quote thread)) (fboundp (quote list-threads)) (fboundp (quote thread-list--get-entries)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote qp)) (fboundp (quote quoted-printable-decode-region)) (fboundp (quote quoted-printable-encode-string)) (featurep (quote mailheader)) (fboundp (quote mail-header-extract)) (fboundp (quote mail-header-format)) (featurep (quote yenc)) (fboundp (quote yenc-decode-region)) (fboundp (quote yenc-parse-line)) (featurep (quote flow-fill)) (fboundp (quote fill-flowed)) (fboundp (quote fill-flowed-encode)) (featurep (quote uudecode)) (fboundp (quote uudecode-decode-region)) (fboundp (quote uudecode-decode-region-internal)) (featurep (quote tq)) (fboundp (quote tq-create)) (fboundp (quote tq-enqueue)) (fboundp (quote tq-filter)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote mail-prsvr)) (boundp (quote mail-parse-charset)) (featurep (quote mm-util)) (fboundp (quote mm-charset-to-coding-system)) (fboundp (quote mm-mime-charset)) (featurep (quote rfc2047)) (fboundp (quote rfc2047-encode-string)) (fboundp (quote rfc2047-decode-string)) (featurep (quote rfc2231)) (fboundp (quote rfc2231-parse-string)) (fboundp (quote rfc2231-encode-string)) (featurep (quote mail-parse)) (fboundp (quote mail-header-parse-addresses-lax)) (fboundp (quote mail-header-parse-address-lax)) (featurep (quote rfc6068)) (fboundp (quote rfc6068-parse-mailto-url)) (fboundp (quote rfc6068-unhexify-string)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote mail-utils)) (fboundp (quote mail-file-babyl-p)) (fboundp (quote mail-fetch-field)) (fboundp (quote mail-strip-quoted-names)) (featurep (quote rfc822)) (fboundp (quote rfc822-addresses)) (fboundp (quote rfc822-nuke-whitespace)) (featurep (quote ietf-drums-date)) (fboundp (quote ietf-drums-parse-date-string)) (featurep (quote binhex)) (fboundp (quote binhex-decode-region)) (fboundp (quote binhex-decode-region-internal)) (fboundp (quote binhex-string-big-endian)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote sasl)) (featurep (quote sasl-plain)) (featurep (quote sasl-login)) (featurep (quote sasl-anonymous)) (fboundp (quote sasl-make-client)) (fboundp (quote sasl-next-step)) (fboundp (quote sasl-find-mechanism)) (featurep (quote sasl-cram)) (fboundp (quote sasl-cram-md5-response)) (featurep (quote sasl-digest)) (fboundp (quote sasl-digest-md5-response)) (featurep (quote sasl-scram-rfc)) (featurep (quote sasl-scram-sha-1)) (fboundp (quote sasl-scram-sha-1-client-final-message)) (featurep (quote sasl-scram-sha256)) (fboundp (quote sasl-scram-sha-256-client-final-message)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote md4)) (fboundp (quote md4)) (featurep (quote ntlm)) (fboundp (quote ntlm-build-auth-request)) (fboundp (quote ntlm-build-auth-response)) (fboundp (quote ntlm-get-password-hashes)) (fboundp (quote ntlm-md4hash)) (featurep (quote sasl-ntlm)) (fboundp (quote sasl-ntlm-request)) (fboundp (quote sasl-ntlm-response)) (= (length (md4 (symbol-name (quote abc)) 3)) 16) (= (length (ntlm-build-auth-request (symbol-name (quote user)))) 36))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote compface)) (fboundp (quote uncompface)) (featurep (quote tramp-uu)) (fboundp (quote tramp-uuencode-region)) (fboundp (quote tramp-uu-byte-to-uu-char)) (fboundp (quote tramp-uu-b64-char-to-byte)) (featurep (quote trampver)) (boundp (quote tramp-version)) (stringp tramp-version) (fboundp (quote tramp-inside-emacs)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote term/bobcat)) (featurep (quote term/cygwin)) (featurep (quote term/vt200)) (featurep (quote term/linux)) (featurep (quote term/vt100)) (featurep (quote term/AT386)) (featurep (quote term/news)) (featurep (quote term/lk201)) (featurep (quote term/w32console)) (fboundp (quote terminal-init-bobcat)) (fboundp (quote terminal-init-cygwin)) (fboundp (quote terminal-init-vt200)) (fboundp (quote terminal-init-linux)) (fboundp (quote terminal-init-vt100)) (fboundp (quote terminal-init-AT386)) (fboundp (quote terminal-init-news)) (fboundp (quote terminal-init-lk201)) (fboundp (quote terminal-init-w32console)) (boundp (quote lk201-function-map)) (boundp (quote w32-tty-standard-colors)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote meese)) (fboundp (quote protect-innocence-hook)) (featurep (quote ps-def)) (fboundp (quote ps-mark-active-p)) (fboundp (quote ps-face-foreground-name)) (fboundp (quote ps-face-background-name)) (featurep (quote ps-print-loaddefs)) (boundp (quote ps-multibyte-buffer)) (featurep (quote glyphless-mode)) (boundp (quote glyphless-mode-types)) (fboundp (quote glyphless-mode--setup)) (featurep (quote word-wrap-mode)) (boundp (quote word-wrap-whitespace-characters)) (boundp (quote word-wrap-mode--previous-state)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote sqlite)) (fboundp (quote with-sqlite-transaction)) (featurep (quote url-future)) (fboundp (quote make-url-future)) (fboundp (quote url-future-call)) (featurep (quote url-domsuf)) (boundp (quote url-domsuf-domains)) (fboundp (quote url-domsuf-cookie-allowed-p)) (featurep (quote vt100-led)) (boundp (quote led-state)) (fboundp (quote led-on)) (fboundp (quote led-off)) (fboundp (quote led-flash)) (fboundp (quote led-update)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote cham)) (featurep (quote czech)) (featurep (quote slovak)) (featurep (quote georgian)) (featurep (quote romanian)) (featurep (quote utf-8-lang)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (boundp (quote burmese-composable-pattern)) (featurep (quote tai-viet)) (featurep (quote lao)) (featurep (quote greek)) (featurep (quote ethiopic)) (featurep (quote philippine)) (featurep (quote korean)) (featurep (quote vietnamese)) (featurep (quote thai)) (boundp (quote tai-tham-composable-pattern)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote tai-viet-util)) (boundp (quote tai-viet-re)) (fboundp (quote tai-viet-compose-region)) (featurep (quote cyril-util)) (boundp (quote cyrillic-language-alist)) (fboundp (quote standard-display-cyrillic-translit)) (featurep (quote indonesian)) (featurep (quote korea-util)) (fboundp (quote setup-korean-environment-internal)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote china-util)) (fboundp (quote decode-hz-region)) (fboundp (quote encode-hz-region)) (featurep (quote cyrillic)) (featurep (quote hebrew)) (fboundp (quote hebrew-shape-gstring)) (featurep (quote japanese)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote viet-util)) (fboundp (quote viet-decode-viqr-region)) (fboundp (quote viet-encode-viqr-region)) (featurep (quote chinese)) (featurep (quote japan-util)) (fboundp (quote setup-japanese-environment-internal)) (fboundp (quote japanese-katakana)) (fboundp (quote japanese-hiragana)) (featurep (quote misc-lang)) (fboundp (quote arabic-shape-gstring)) (fboundp (quote egyptian-shape-grouping)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote studly)) (fboundp (quote studlify-region)) (fboundp (quote studlify-word)) (featurep (quote dissociate)) (fboundp (quote dissociated-press)) (featurep (quote makesum)) (fboundp (quote make-command-summary)) (fboundp (quote double-column)) (featurep (quote vt-control)) (boundp (quote vt-applications-keypad-p)) (boundp (quote vt-wide-p)) (fboundp (quote vt-wide)) (fboundp (quote vt-narrow)) (fboundp (quote vt-toggle-screen)) (featurep (quote flow-ctrl)) (boundp (quote flow-control-c-s-replacement)) (boundp (quote flow-control-c-q-replacement)) (fboundp (quote enable-flow-control)) (fboundp (quote enable-flow-control-on)) (featurep (quote talk)) (boundp (quote talk-display-alist)) (fboundp (quote talk-connect)) (fboundp (quote talk)) (fboundp (quote talk-add-display)) (fboundp (quote talk-disconnect)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote nxml-maint)) (fboundp (quote nxml-insert-target-repertoire-glyph-set)) (featurep (quote nxml-util)) (fboundp (quote nxml-make-namespace)) (fboundp (quote nxml-namespace-name)) (featurep (quote vc-filewise)) (fboundp (quote vc-master-name)) (fboundp (quote vc-filewise-registered)) (featurep (quote pgg-def)) (boundp (quote pgg-default-scheme)) (fboundp (quote pgg-truncate-key-identifier)) (featurep (quote autoconf)) (featurep (quote autoconf-mode)) (fboundp (quote autoconf-mode)) (fboundp (quote autoconf-current-defun-function)) (featurep (quote gssapi)) (fboundp (quote open-gssapi-stream)) (featurep (quote scroll-all)) (fboundp (quote scroll-all-function-all)) (fboundp (quote scroll-all-check-to-scroll)) (featurep (quote utf-7)) (fboundp (quote utf-7-decode)) (fboundp (quote utf-7-encode)) (featurep (quote rfc2368)) (fboundp (quote rfc2368-unhexify-string)) (fboundp (quote rfc2368-parse-mailto-url)) (featurep (quote timer-list)) (fboundp (quote list-timers)) (fboundp (quote timer-list-cancel)) (featurep (quote master)) (fboundp (quote master-set-slave)) (fboundp (quote master-says)) (featurep (quote helper)) (fboundp (quote Helper-help)) (fboundp (quote Helper-describe-function)))
# Persistent standalone REPL proof is intentionally count-only; the detailed
# surface proof above remains covered by the true-load replay.
VENDOR_REPL_PROOF_FORM := (and (= vendor-standalone-load-ok-count vendor-standalone-load-file-count) (= vendor-standalone-load-ok-count 306))
VENDOR_REPL_DETAIL_FORM ?= (concat "load-ok-count=" (number-to-string vendor-standalone-load-ok-count) "/" (number-to-string vendor-standalone-load-file-count) " loads=" vendor-repl-load-status " project-current=" (if (fboundp (quote project-current)) "t" "nil") " find-file=" (if (symbol-function (quote find-file)) "t" "nil") " save-buffer=" (if (symbol-function (quote save-buffer)) "t" "nil") " write-file=" (if (symbol-function (quote write-file)) "t" "nil") " forward-sexp=" (if (fboundp (quote forward-sexp)) "t" "nil") " map-y-or-n-p=" (if (fboundp (quote map-y-or-n-p)) "t" "nil") " ring=" (if (featurep (quote ring)) "t" "nil") " ring-ref=" (if (fboundp (quote ring-ref)) "t" "nil") " generator=" (if (featurep (quote generator)) "t" "nil") " avl-tree=" (if (featurep (quote avl-tree)) "t" "nil") " avl-tree-create=" (if (fboundp (quote avl-tree-create)) "t" "nil") " avl-tree-iter=" (if (fboundp (quote avl-tree-iter)) "t" "nil") " iso-transl-vars=" (if (and (boundp (quote iso-transl-char-map)) (boundp (quote iso-transl-language-alist))) "t" "nil") " cp51932=" (if (featurep (quote cp51932)) "t" "nil") " eucjp-ms=" (if (featurep (quote eucjp-ms)) "t" "nil") " fontset=" (if (featurep (quote fontset)) "t" "nil") " ja-dic-utl=" (if (featurep (quote ja-dic-utl)) "t" "nil") " format-spec=" (if (featurep (quote format-spec)) "t" "nil") " org-version=" (if (featurep (quote org-version)) "t" "nil") " org-macs=" (if (featurep (quote org-macs)) "t" "nil") " org-compat=" (if (featurep (quote org-compat)) "t" "nil") " org-fold-core=" (if (featurep (quote org-fold-core)) "t" "nil") " org-fold=" (if (featurep (quote org-fold)) "t" "nil") " org-duration=" (if (featurep (quote org-duration)) "t" "nil") " org=" (if (featurep (quote org)) "t" "nil") " org-agenda=" (if (featurep (quote org-agenda)) "t" "nil") " org-capture=" (if (featurep (quote org-capture)) "t" "nil") " org-refile=" (if (featurep (quote org-refile)) "t" "nil") " org-datetree=" (if (featurep (quote org-datetree)) "t" "nil") " org-archive=" (if (featurep (quote org-archive)) "t" "nil") " org-clock=" (if (featurep (quote org-clock)) "t" "nil") " ol=" (if (featurep (quote ol)) "t" "nil") " org-element-ast=" (if (featurep (quote org-element-ast)) "t" "nil") " org-footnote=" (if (featurep (quote org-footnote)) "t" "nil") " org-list=" (if (featurep (quote org-list)) "t" "nil") " org-list-to-lisp=" (if (fboundp (quote org-list-to-lisp)) "t" "nil") " org-entities=" (if (featurep (quote org-entities)) "t" "nil") " org-entity-get=" (if (fboundp (quote org-entity-get)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " org-macro=" (if (featurep (quote org-macro)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " org-faces=" (if (featurep (quote org-faces)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " oc=" (if (featurep (quote oc)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " org-keys=" (if (featurep (quote org-keys)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " org-cycle=" (if (featurep (quote org-cycle)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " org-inlinetask=" (if (featurep (quote org-inlinetask)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " ol-doi=" (if (featurep (quote ol-doi)) "t" "nil") " ol-info=" (if (featurep (quote ol-info)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " ol-man=" (if (featurep (quote ol-man)) "t" "nil") " ol-rmail=" (if (featurep (quote ol-rmail)) "t" "nil") " ol-mhe=" (if (featurep (quote ol-mhe)) "t" "nil") " ol-w3m=" (if (featurep (quote ol-w3m)) "t" "nil") " ol-irc=" (if (featurep (quote ol-irc)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " tempo=" (if (featurep (quote tempo)) "t" "nil") " org-tempo=" (if (featurep (quote org-tempo)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " inline=" (if (featurep (quote inline)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " easymenu=" (if (featurep (quote easymenu)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " let-alist=" (if (featurep (quote let-alist)) "t" "nil") " radix-tree=" (if (featurep (quote radix-tree)) "t" "nil") " text-property-search=" (if (featurep (quote text-property-search)) "t" "nil") " thunk=" (if (featurep (quote thunk)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " core-helpers=version/help-macro/env/fileloop")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " small-foundations=" (if (and (featurep (quote rmc)) (featurep (quote generate-lisp-file)) (featurep (quote obarray)) (featurep (quote soundex)) (featurep (quote cursor-sensor)) (featurep (quote indent-aux)) (featurep (quote display-fill-column-indicator))) "rmc/gen/ob/soundex/cursor/indent/fci" "missing"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " thingatpt=" (if (featurep (quote thingatpt)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " time-date=" (if (featurep (quote time-date)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " iso8601=" (if (featurep (quote iso8601)) "t" "nil") " parse-time=" (if (featurep (quote parse-time)) "t" "nil"))
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " recovered-arena-small=" (if (and (featurep (quote nnnil)) (featurep (quote po)) (featurep (quote cedet)) (fboundp (quote cc-block-intro-offset)) (featurep (quote cedet-cscope)) (featurep (quote metamail)) (featurep (quote string-edit)) (featurep (quote flymake-cc)) (featurep (quote external-completion)) (featurep (quote yank-media)) (featurep (quote cedet-idutils))) "t" "nil") " cyril-jis=load-count-only")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " unicode-case-data=load-only")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " small-utils=tabify/rot13/underline/widget/dos/mb-depth/mail/hmac")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " extra-foundations=compat/shorthands/dynamic-setting unicode-numeric=load-only")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " extra-ui-helpers=benchmark/password/double/chistory/scroll-lock/thread")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " mail-queue-helpers=qp/mailheader/yenc/flow-fill/uudecode/tq")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " mime-mail-helpers=mail-prsvr/mm-util/rfc2047/rfc2231/mail-parse/rfc6068")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " mail-utility-helpers=mail-utils/rfc822/ietf-drums-date/binhex")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " sasl-helpers=sasl/cram/digest/scram-rfc/scram-sha256")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " ntlm-helpers=md4/ntlm/sasl-ntlm")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " image-tramp-helpers=compface/tramp-uu/trampver")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " term-helpers=bobcat/cygwin/vt200/linux/vt100/AT386/news/lk201/w32console")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " ui-legacy-helpers=meese/ps-def/ps-print-loaddefs/glyphless/word-wrap")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " sqlite-url-helpers=sqlite/url-future/url-domsuf/vt100-led")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " language-helpers=khmer/cham/czech/slovak/georgian/sinhala/romanian/utf8")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " language-helpers-2=burmese/tai-viet/english/lao/greek/ethiopic/philippine/korean/vietnamese/thai")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " language-utils=tv/cyril/indonesian/korea")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " language-main=china/cyrillic/hebrew/japanese")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " language-more=viet/chinese/japan-util/misc-lang")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " misc-small=studly/dissociate/makesum/vt/flow/talk")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " utility-small=nxml/vc/pgg/autoconf/gssapi/scroll/utf7/rfc2368/timer/master/helper")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " non-org-small=rfc1843/nxml-enc/bibtex-style/dictionary-connection/m4-mode")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " play-term-small=cookie1/spook/yow/bruce/autoarg/tvi970/sun")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " misc-url-rng-small=subdirs/edt/rng/url-vars/url-privacy")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " ui-compat-small=edt-pc/w32/novice/page/cl-compat/elide/iimage/authors")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " utility-small-2=textsec/debug/calc-macs/kinsoku/latexenc/reposition/ansi-osc/morse")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " cedet-mh-epa-small=repl")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " misc-mode-net-small=repl")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " editing-ui-small=minibuf/visual/display/rfn")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " misc-helper-small=repl")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " standalone-small=true-load-only")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " helper-foundation-small=true-load-only")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " org-cite-babel-small=true-load-only")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " org-citation-backends=true-load-only")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " subword=repl")
VENDOR_REPL_DETAIL_FORM := (concat $(VENDOR_REPL_DETAIL_FORM) " font-core=global-font-lock-mode")
VENDOR_REPL_PROOF_FORM_ELISP = $(subst ",\",$(VENDOR_REPL_PROOF_FORM))
VENDOR_REPL_DETAIL_FORM_ELISP = $(subst ",\",$(VENDOR_REPL_DETAIL_FORM))
VENDOR_REPL_PROOF_FORM_FILE ?=
VENDOR_REPL_TIMEOUT ?= 900s
VENDOR_REPL_KEEP_TEMP ?= nil
VENDOR_REPL_TRACE_FORMS ?= nil
VENDOR_REPL_DIRECT_CHARACTER_LIMIT ?= 0
VENDOR_REPL_COALESCE_FILE_FORMS ?= nil
VENDOR_REPL_INTERNAL_TIMEOUT_SECONDS ?= nil
VENDOR_FAST_FILES ?= $(abspath $(VENDOR_FORM_WALK_FILE))
VENDOR_FAST_PROOF_FORM ?= (= vendor-standalone-load-ok-count vendor-standalone-load-file-count)
VENDOR_FAST_PROOF_FORM_FILE ?=
VENDOR_FAST_DETAIL_FORM ?= (concat "load-ok-count=" (number-to-string vendor-standalone-load-ok-count) "/" (number-to-string vendor-standalone-load-file-count))
VENDOR_FAST_PROOF_FORM_ELISP = $(subst ",\",$(VENDOR_FAST_PROOF_FORM))
VENDOR_FAST_DETAIL_FORM_ELISP = $(subst ",\",$(VENDOR_FAST_DETAIL_FORM))
NELISP_LOAD_PATH = -L $(NELISP_ROOT)/src \
	$(foreach d,$(wildcard $(NELISP_ROOT)/packages/*/src),-L $(d))
SRC_FILES = $(wildcard src/*.el)
# `generator.el' is vendored verbatim from GNU Emacs.  It loads correctly but
# host byte-compilation trips over upstream macro shapes, so `make compile'
# validates it through a load proof instead of compiling it.
SRC_BYTE_COMPILE_EXCLUDE = src/generator.el
SRC_BYTE_COMPILE_FILES = $(filter-out $(SRC_BYTE_COMPILE_EXCLUDE),$(SRC_FILES))
NEMACS_LIBRARY_PACKAGE_SMOKE_FEATURES = \
	emacs-foundation \
	emacs-text-core \
	emacs-buffer-core \
	emacs-editing \
	emacs-io \
	emacs-special-buffers \
	emacs-core \
	emacs-textmodes-stub
NEMACS_LIBRARY_PACKAGE_LAZY_SMOKE_FEATURES = \
	nelisp-coding-jis-tables \
	emacs-bookmark-ui \
	emacs-buffer-ui \
	emacs-dump \
	image-loader \
	nemacs-loaddefs \
	emacs-dired-min \
	files-standalone-buffer \
	emacs-isearch \
	emacs-replace \
	emacs-shell-command \
	emacs-vc \
	emacs-project \
	emacs-elisp-eval \
	emacs-elisp-mode \
	emacs-font-lock \
	emacs-ielm \
	lisp-mode \
	emacs-redisplay-core \
	emacs-syntax-table \
	emacs-tui-backend \
	emacs-tui-event \
	emacs-tui-terminfo \
	org \
	emacs-org-outline \
	emacs-org-todo \
	emacs-org-table
NEMACS_LIBRARY_PACKAGE_FRONTEND_SMOKE_TESTS = \
	test/nemacs-gtk-view-menu-test.el \
	test/nemacs-gtk-frontend-menu-test.el
NEMACS_LIBRARY_PACKAGE_GUI_BRIDGE_SMOKE_SELECTOR ?= (or \
	nemacs-gui-file-bridge-runtime-test/source-shape \
	nemacs-gui-file-bridge-runtime-test/package-scaffold-source-resolution \
	nemacs-gui-file-bridge-runtime-test/app-scaffold-source-resolution \
	nemacs-gui-file-bridge-runtime-test/scaffold-source-resolution-has-no-src-fallback \
	nemacs-gui-file-bridge-runtime-test/generated-image-includes-family-runtimes \
	nemacs-gui-file-bridge-runtime-test/source-shape-tier-1-ui-smoke-contract \
	nemacs-gui-file-bridge-runtime-test/source-shape-fileio-writeback-delegation \
	nemacs-gui-file-bridge-runtime-test/source-shape-bookmark-writeback-helper \
	nemacs-gui-file-bridge-runtime-test/source-shape-face-spans-contract \
	nemacs-gui-file-bridge-runtime-test/host-face-span-decision-path)
NEMACS_LIBRARY_PACKAGE_GUI_BRIDGE_STANDALONE_SMOKE_SELECTOR ?= (or \
	nemacs-gui-file-bridge-runtime-test/standalone-transport-dir-override \
	nemacs-gui-file-bridge-runtime-test/standalone-fileio-writeback-spec \
	nemacs-gui-file-bridge-runtime-test/standalone-bridge-find-file-writeback-helper \
	nemacs-gui-file-bridge-runtime-test/standalone-bridge-buffer-switch-writeback-helper \
	nemacs-gui-file-bridge-runtime-test/standalone-bridge-bookmark-writeback-helper)
TEST_FILES = $(wildcard test/*.el)
# Heavy integration ERTs spawn subprocesses and need NEMACS_NELISP_ROOT + a
# built reader; they have dedicated targets (gate5/gate6/vendor-nelc-cache[-set])
# and must stay out of the umbrella `make test'.
TEST_INTEGRATION_FILES = \
	test/nelisp-emacs-artifact-gate5-test.el \
	test/nelisp-emacs-artifact-gate6-test.el \
	test/emacs-server-client-test.el \
	test/nemacs-vendor-cache-test.el \
	test/nemacs-vendor-cache-set-test.el
TEST_UNIT_FILES = $(filter-out $(TEST_INTEGRATION_FILES),$(TEST_FILES))
TEST_FAST_FILES = \
	test/emacs-standalone-test.el \
	test/emacs-buffer-builtins-test.el \
	test/emacs-buffer-test.el \
	test/emacs-edit-builtins-test.el \
	test/emacs-fileio-builtins-test.el \
	test/files-test.el \
	test/emacs-fileio-test.el \
	test/emacs-keymap-builtins-test.el \
	test/emacs-keymap-test.el \
	test/emacs-minibuffer-builtins-test.el \
	test/emacs-minibuffer-test.el \
	test/emacs-command-loop-builtins-test.el \
	test/emacs-faces-builtins-test.el \
	test/emacs-dired-min-test.el \
	test/vendor-first-core-modes-test.el \
	test/emacs-help-test.el \
	test/emacs-info-test.el \
	test/emacs-shell-command-test.el \
	test/emacs-process-builtins-test.el \
	test/emacs-redisplay-builtins-test.el \
	test/emacs-redisplay-test.el \
	test/emacs-calc-test.el \
	test/emacs-shell-test.el \
	test/emacs-ielm-test.el \
	test/emacs-vc-test.el \
	test/emacs-tier3-facades-test.el

.PHONY: compile test test-fast soak gate-nemacs-complete gate5 gate6 elprop vendor-nelc-cache vendor-nelc-cache-set test-redisplay-core-smoke test-nemacs-gui-bridge test-nemacs-gui-bridge-gate test-nemacs-gui-bridge-slow test-nemacs-gui-bridge-slow-profile nemacs-gui-bridge-profile-summary nemacs-gui-bridge-run-shape test-nemacs-gui-bridge-select test-nemacs-server-client nemacs-library-gate nemacs-library-contract nemacs-library-consumer-smoke nemacs-library-package-smoke nemacs-library-package-path-smoke nemacs-library-package-consumer-smoke nemacs-library-package-lazy-smoke nemacs-library-package-load-path nemacs-library-package-frontend-smoke nemacs-library-package-gui-bridge-smoke nemacs-library-package-gui-bridge-standalone-smoke nemacs-library-package-manifest nemacs-library-package-deps nemacs-library-package-descriptors nemacs-library-package-guide nemacs-library-package-api nemacs-library-package-catalog nemacs-library-compat-api-policy nemacs-library-api-promotion-queue nemacs-library-package-layout nemacs-library-package-scaffold nemacs-library-app-scaffold nemacs-library-app-boundary nemacs-library-package-app-require-guard nemacs-library-package-metadata nemacs-library-package-install-smoke nemacs-library-package-archive nemacs-library-package-archive-smoke nemacs-library-package-archive-checksum nemacs-library-package-archive-index nemacs-library-package-index-smoke nemacs-library-package-publication-policy nemacs-library-package-release-key-policy nemacs-library-package-signature-policy nemacs-library-package-signature-release-sign nemacs-library-package-signature-release-verify nemacs-library-package-signature-release nemacs-library-package-release-bundle-manifest nemacs-library-package-release-bundle-smoke nemacs-library-package-release-publication-policy nemacs-library-package-release-publication-policy-run nemacs-library-package-release-bundle nemacs-library-package-release-rehearsal-key nemacs-library-package-release-rehearsal nemacs-library-package-release-config-check nemacs-library-package-release-ready nemacs-library-package-release-from-config nemacs-library-package-dependency-publication-policy nemacs-library-package-lazy-metadata nemacs-library-package-vendor-lock nemacs-library-package-vendor-release-verify nemacs-library-package-verify nemacs-runtime-image-input-inventory nemacs-gui-keymap-coverage gui-bridge-runtime-inventory nemacs-stub-fallback-skip-inventory nemacs-dirty-review-units nemacs-library-boundary-report nemacs-public-api-inventory nemacs-ownership-coverage verify-production-runtime-path doctor build-nelisp-bootstrap bake-image bake-runtime-image bake-interactive-runtime-image bake-vendor-core-runtime-image test-nelisp test-nelisp-runtime-image test-nelisp-interactive-runtime-image test-nelisp-vendor-core-runtime-image test-nelisp-ert profile-nelisp-bootstrap diagnose-vendor-form-walk diagnose-vendor-load-replay diagnose-vendor-repl-replay diagnose-vendor-form-walk-fast diagnose-vendor-load-replay-fast verify-nemacs-daily-driver verify-nelisp-standalone verify-vendor verify-vendor-inventory verify-vendor-class-a verify-vendor-core bench demo demo-phase2 clean nelisp nelisp-rebuild nelisp-clean help

help:
	@echo "Targets:"
	@echo "  make compile         byte-compile src/*.el except vendored load-proof files"
	@echo "  make test            run ERT under host emacs"
	@echo "  make test-fast       run fast host ERT gate for core daily-driver runtime"
	@echo "  make nemacs-library-gate  run reusable-library facade/boundary/dirty/compile checks"
	@echo "  make nemacs-library-contract  verify external consumer contract symbols"
	@echo "  make nemacs-library-consumer-smoke  prove facade loads from src without app/frontends"
	@echo "  make nemacs-library-package-smoke  prove package group loaders require independently"
	@echo "  make nemacs-library-package-manifest  write facade package manifest artifacts"
	@echo "  make nemacs-library-package-deps  write facade package dependency and migration queue artifacts"
	@echo "  make nemacs-library-package-descriptors  write draft package descriptor artifacts"
	@echo "  make nemacs-library-package-guide  write consumer package guide artifacts"
	@echo "  make nemacs-library-package-api  write package-scoped API inventory artifacts"
	@echo "  make nemacs-library-package-catalog  write consumer package API catalog artifacts"
	@echo "  make nemacs-library-compat-api-policy  verify stable API compat/prefixed policy"
	@echo "  make nemacs-library-api-promotion-queue  write package API promotion queue artifacts"
	@echo "  make nemacs-library-package-layout  write draft packages/ layout plan artifacts"
	@echo "  make nemacs-library-package-scaffold  generate experimental packages/ scaffold"
	@echo "  make nemacs-library-app-scaffold  generate experimental app/frontend scaffold"
	@echo "  make nemacs-library-app-boundary  verify app bootstrap files stay out of packages/ scaffold"
	@echo "  make nemacs-library-package-app-require-guard  verify packages do not require app/frontend features"
	@echo "  make nemacs-library-package-metadata  generate draft package archive metadata"
	@echo "  make nemacs-library-package-install-smoke  prove package-style install/load for each package"
	@echo "  make nemacs-library-package-archive  build draft package tar archives"
	@echo "  make nemacs-library-package-archive-smoke  prove install/load from generated package archives"
	@echo "  make nemacs-library-package-archive-checksum  prove generated package archives rebuild reproducibly"
	@echo "  make nemacs-library-package-archive-index  generate local package archive index"
	@echo "  make nemacs-library-package-index-smoke  prove install/load through package archive index"
	@echo "  make nemacs-library-package-publication-policy  verify package publication policy artifacts"
	@echo "  make nemacs-library-package-release-key-policy  verify release public key policy"
	@echo "  make nemacs-library-package-signature-policy  verify release signature policy artifact targets"
	@echo "  make nemacs-library-package-signature-release-sign  create release detached signatures"
	@echo "  make nemacs-library-package-signature-release-verify  verify release detached signatures strictly"
	@echo "  make nemacs-library-package-signature-release  create and verify release detached signatures"
	@echo "  make nemacs-library-package-release-bundle-manifest  retain draft release bundle files and manifest"
	@echo "  make nemacs-library-package-release-bundle-smoke  prove install/load from retained release bundle"
	@echo "  make nemacs-library-package-release-publication-policy  verify retained release bundle publishability"
	@echo "  make nemacs-library-package-release-bundle  create and smoke strict signed release bundle"
	@echo "  make nemacs-library-package-release-rehearsal  run strict signed bundle with a throwaway key"
	@echo "  make nemacs-library-package-release-ready  verify real release signing config and public key"
	@echo "  make nemacs-library-package-release-from-config  create strict signed bundle from local release config"
	@echo "  make nemacs-library-package-dependency-publication-policy  verify lazy/host/vendor dependency publication policy"
	@echo "  make nemacs-library-package-lazy-metadata  generate lazy companion dependency closure metadata"
	@echo "  make nemacs-library-package-vendor-lock  record vendored dependency HEAD/content locks"
	@echo "  make nemacs-library-package-vendor-release-verify  verify vendored dependency locks for release"
	@echo "  make nemacs-library-package-load-path  print packages/ scaffold -L args"
	@echo "  make nemacs-library-package-path-smoke  prove package loaders from packages/ scaffold"
	@echo "  make nemacs-library-package-consumer-smoke  prove nelisp-emacs facade from packages/ scaffold"
	@echo "  make nemacs-library-package-lazy-smoke  prove lazy package features from packages/ scaffold"
	@echo "  make nemacs-library-package-frontend-smoke  prove selected frontend smoke with package-backed libraries"
	@echo "  make nemacs-library-package-gui-bridge-smoke  prove selected GUI bridge host smoke with package-backed libraries"
	@echo "  make nemacs-library-package-gui-bridge-standalone-smoke  prove selected GUI bridge standalone smoke with package paths"
	@echo "  make nemacs-library-package-verify  verify descriptor/guide/API/catalog/promotion/layout extraction artifacts"
	@echo "  make gate-nemacs-complete  run completion gate for daily runtime readiness"
	@echo "  make gate5           prove vendor source replay == .nelc artifact load"
	@echo "  make vendor-nelc-cache-set prove vendor cache set cold/warm/invalidation"
	@echo "  make test-redisplay-core-smoke  run isolated lightweight redisplay core smoke"
	@echo "  make test-nemacs-gui-bridge  run standalone GUI file bridge ERT"
	@echo "  make test-nemacs-gui-bridge-gate  run GUI bridge gate without slow tail"
	@echo "  make test-nemacs-gui-bridge-slow  run only the GUI bridge slow tail"
	@echo "  make test-nemacs-gui-bridge-slow-profile  run slow tail with per-runtime timing"
	@echo "  make nemacs-gui-bridge-profile-summary  summarize NEMACS_GUI_BRIDGE_PROFILE log"
	@echo "  make nemacs-gui-bridge-run-shape  summarize nemacs-gui-file-bridge-run structure"
	@echo "  make test-nemacs-gui-bridge-select NEMACS_GUI_BRIDGE_TEST_SELECTOR=TEST  run selected GUI bridge ERT"
	@echo "  make test-nemacs-server-client  run standalone server/emacsclient round-trip ERT"
	@echo "  make nemacs-gui-keymap-coverage  write GUI keymap coverage TSV and summary artifacts"
	@echo "  make gui-bridge-runtime-inventory  write GUI bridge runtime symbol inventory"
	@echo "  make nemacs-stub-fallback-skip-inventory  write stub/fallback/skip inventory"
	@echo "  make nemacs-dirty-review-units  classify dirty worktree paths into review units"
	@echo "  make nemacs-library-boundary-report  write advisory library boundary report"
	@echo "  make nemacs-library-contract  verify external consumer contract symbols"
	@echo "  make nemacs-library-package-manifest  write facade package manifest artifacts"
	@echo "  make nemacs-library-package-deps  write facade package dependency and migration queue artifacts"
	@echo "  make nemacs-library-package-descriptors  write draft package descriptor artifacts"
	@echo "  make nemacs-library-package-guide  write consumer package guide artifacts"
	@echo "  make nemacs-library-package-api  write package-scoped API inventory artifacts"
	@echo "  make nemacs-library-package-catalog  write consumer package API catalog artifacts"
	@echo "  make nemacs-library-compat-api-policy  verify stable API compat/prefixed policy"
	@echo "  make nemacs-library-api-promotion-queue  write package API promotion queue artifacts"
	@echo "  make nemacs-library-package-layout  write draft packages/ layout plan artifacts"
	@echo "  make nemacs-library-package-scaffold  generate experimental packages/ scaffold"
	@echo "  make nemacs-library-app-scaffold  generate experimental app/frontend scaffold"
	@echo "  make nemacs-library-app-boundary  verify app bootstrap files stay out of packages/ scaffold"
	@echo "  make nemacs-library-package-app-require-guard  verify packages do not require app/frontend features"
	@echo "  make nemacs-library-package-metadata  generate draft package archive metadata"
	@echo "  make nemacs-library-package-install-smoke  prove package-style install/load for each package"
	@echo "  make nemacs-library-package-archive  build draft package tar archives"
	@echo "  make nemacs-library-package-archive-smoke  prove install/load from generated package archives"
	@echo "  make nemacs-library-package-archive-checksum  prove generated package archives rebuild reproducibly"
	@echo "  make nemacs-library-package-archive-index  generate local package archive index"
	@echo "  make nemacs-library-package-index-smoke  prove install/load through package archive index"
	@echo "  make nemacs-library-package-publication-policy  verify package publication policy artifacts"
	@echo "  make nemacs-library-package-release-key-policy  verify release public key policy"
	@echo "  make nemacs-library-package-signature-policy  verify release signature policy artifact targets"
	@echo "  make nemacs-library-package-signature-release-sign  create release detached signatures"
	@echo "  make nemacs-library-package-signature-release-verify  verify release detached signatures strictly"
	@echo "  make nemacs-library-package-signature-release  create and verify release detached signatures"
	@echo "  make nemacs-library-package-release-bundle-manifest  retain draft release bundle files and manifest"
	@echo "  make nemacs-library-package-release-bundle-smoke  prove install/load from retained release bundle"
	@echo "  make nemacs-library-package-release-publication-policy  verify retained release bundle publishability"
	@echo "  make nemacs-library-package-release-bundle  create and smoke strict signed release bundle"
	@echo "  make nemacs-library-package-release-rehearsal  run strict signed bundle with a throwaway key"
	@echo "  make nemacs-library-package-release-ready  verify real release signing config and public key"
	@echo "  make nemacs-library-package-release-from-config  create strict signed bundle from local release config"
	@echo "  make nemacs-library-package-dependency-publication-policy  verify lazy/host/vendor dependency publication policy"
	@echo "  make nemacs-library-package-lazy-metadata  generate lazy companion dependency closure metadata"
	@echo "  make nemacs-library-package-vendor-lock  record vendored dependency HEAD/content locks"
	@echo "  make nemacs-library-package-vendor-release-verify  verify vendored dependency locks for release"
	@echo "  make nemacs-library-package-load-path  print packages/ scaffold -L args"
	@echo "  make nemacs-library-package-path-smoke  prove package loaders from packages/ scaffold"
	@echo "  make nemacs-library-package-consumer-smoke  prove nelisp-emacs facade from packages/ scaffold"
	@echo "  make nemacs-library-package-lazy-smoke  prove lazy package features from packages/ scaffold"
	@echo "  make nemacs-library-package-frontend-smoke  prove selected frontend smoke with package-backed libraries"
	@echo "  make nemacs-library-package-gui-bridge-smoke  prove selected GUI bridge host smoke with package-backed libraries"
	@echo "  make nemacs-library-package-gui-bridge-standalone-smoke  prove selected GUI bridge standalone smoke with package paths"
	@echo "  make nemacs-library-package-verify  verify descriptor/guide/API/catalog/promotion/layout extraction artifacts"
	@echo "  make nemacs-runtime-image-input-inventory  write runtime-image input inventory"
	@echo "  make nemacs-public-api-inventory  write package-group public API inventory"
	@echo "  make nemacs-ownership-coverage  verify Doc 18 covers src/gui Elisp files"
	@echo "  make verify-production-runtime-path  prove production runtime modules are bootstrapped and scaffold-mapped"
	@echo "  make doctor          run host/NeLisp driver readiness checks"
	@echo "  make build-nelisp-bootstrap  generate build/nemacs-bootstrap.el and .repl"
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
	@echo "  make diagnose-vendor-repl-replay  load vendor files in persistent standalone REPL"
	@echo "  make diagnose-vendor-form-walk-fast  form-walk using existing bootstrap bundle"
	@echo "  make diagnose-vendor-load-replay-fast  load VENDOR_FAST_FILES using existing bootstrap bundle"
	@echo "  make diagnose-vendor-repl-replay-fast  REPL-load VENDOR_FAST_FILES using existing bootstrap REPL"
	@echo "  make verify-nemacs-daily-driver  run TUI daily-driver workflow smoke"
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
	@echo "  VENDOR_FORM_WALK_PRELOAD_FILES=$(VENDOR_FORM_WALK_PRELOAD_FILES)  files to load before diagnose-vendor-form-walk target forms"
	@echo "  VENDOR_FORM_WALK_NORMALIZE_FLOATS=$(VENDOR_FORM_WALK_NORMALIZE_FLOATS)  normalize floats in standalone diagnostic probes"
	@echo "  VENDOR_SOURCE_CACHE_DIR=$(VENDOR_SOURCE_CACHE_DIR)  host-side normalized source cache"
	@echo "  VENDOR_LOAD_PRELUDE=$(VENDOR_LOAD_PRELUDE)  standalone prelude for diagnose-vendor-load-replay"
	@echo "  VENDOR_LOAD_FILES=$(VENDOR_LOAD_FILES)  files for diagnose-vendor-load-replay"
	@echo "  VENDOR_LOAD_PROOF_FORM=$(VENDOR_LOAD_PROOF_FORM)  post-load proof for diagnose-vendor-load-replay"
	@echo "  VENDOR_REPL_PRELUDE=$(VENDOR_REPL_PRELUDE)  standalone prelude for diagnose-vendor-repl-replay"
	@echo "  VENDOR_REPL_FILES=$(VENDOR_REPL_FILES)  files for diagnose-vendor-repl-replay"
	@echo "  VENDOR_REPL_PROOF_FORM=$(VENDOR_REPL_PROOF_FORM)  post-load proof for diagnose-vendor-repl-replay"
	@echo "  VENDOR_REPL_PROOF_FORM_FILE=$(VENDOR_REPL_PROOF_FORM_FILE)  file containing post-load REPL proof form"
	@echo "  VENDOR_REPL_DETAIL_FORM=$(VENDOR_REPL_DETAIL_FORM)  diagnostic string form shown when REPL proof fails"
	@echo "  VENDOR_REPL_KEEP_TEMP=$(VENDOR_REPL_KEEP_TEMP)  keep generated REPL diagnostics when non-nil"
	@echo "  VENDOR_REPL_TRACE_FORMS=$(VENDOR_REPL_TRACE_FORMS)  record per-form REPL progress when non-nil"
	@echo "  VENDOR_REPL_DIRECT_CHARACTER_LIMIT=$(VENDOR_REPL_DIRECT_CHARACTER_LIMIT)  direct-emits normalized forms above this size"
	@echo "  VENDOR_REPL_COALESCE_FILE_FORMS=$(VENDOR_REPL_COALESCE_FILE_FORMS)  replay each file as one normalized progn when non-nil"
	@echo "  VENDOR_REPL_INTERNAL_TIMEOUT_SECONDS=$(VENDOR_REPL_INTERNAL_TIMEOUT_SECONDS)  optional in-Emacs timeout that preserves sentinel progress"
	@echo "  VENDOR_FAST_FILES=$(VENDOR_FAST_FILES)  small file set for diagnose-vendor-*-fast"
	@echo "  VENDOR_FAST_PROOF_FORM=$(VENDOR_FAST_PROOF_FORM)  fast load/REPL proof"
	@echo "  VENDOR_FAST_PROOF_FORM_FILE=$(VENDOR_FAST_PROOF_FORM_FILE)  file containing fast REPL proof form"
	@echo "  VENDOR_FAST_DETAIL_FORM=$(VENDOR_FAST_DETAIL_FORM)  fast REPL failure detail"

compile:
	$(EMACS) -L src $(NELISP_LOAD_PATH) \
		--eval '(setq native-comp-enable-subr-trampolines nil)' \
		-f batch-byte-compile $(SRC_BYTE_COMPILE_FILES)
	$(EMACS) -Q -L src -l src/generator.el \
		--eval '(unless (featurep (quote generator)) (kill-emacs 1))'

nemacs-library-gate: compile nemacs-ownership-coverage nemacs-public-api-inventory nemacs-library-contract nemacs-library-package-manifest nemacs-library-package-deps nemacs-library-package-descriptors nemacs-library-package-guide nemacs-library-package-api nemacs-library-package-catalog nemacs-library-compat-api-policy nemacs-library-api-promotion-queue nemacs-library-package-layout nemacs-library-package-verify nemacs-library-app-boundary nemacs-library-package-app-require-guard nemacs-library-package-metadata nemacs-library-package-install-smoke nemacs-library-package-archive nemacs-library-package-archive-checksum nemacs-library-package-archive-smoke nemacs-library-package-archive-index nemacs-library-package-index-smoke nemacs-library-package-publication-policy nemacs-library-package-release-key-policy nemacs-library-package-signature-policy nemacs-library-package-dependency-publication-policy nemacs-library-package-release-bundle-manifest nemacs-library-package-release-bundle-smoke nemacs-library-package-release-publication-policy nemacs-library-package-lazy-metadata nemacs-library-package-vendor-lock nemacs-runtime-image-input-inventory nemacs-library-boundary-report nemacs-dirty-review-units nemacs-library-package-smoke nemacs-library-consumer-smoke verify-production-runtime-path
	$(EMACS) -Q -L src -l test/nelisp-emacs-test.el \
		-f ert-run-tests-batch-and-exit
	@awk -F '\t' 'NR > 1 && ($$1 == "unowned" || $$1 == "stale") { count++ } END { if (count != 0) { printf "nemacs-library-gate: ownership coverage failures=%d\n", count; exit 1 } }' "$(NEMACS_OWNERSHIP_COVERAGE)"
	@awk -F '\t' 'NR > 1 && $$1 == "UNOWNED" { count++ } END { if (count != 0) { printf "nemacs-library-gate: public API UNOWNED rows=%d\n", count; exit 1 } }' "$(NEMACS_PUBLIC_API_INVENTORY)"
	@awk -F '\t' 'NR > 1 && $$8 == "app-or-frontend" { count++ } END { if (count != 0) { printf "nemacs-library-gate: package app/frontend dependency rows=%d\n", count; exit 1 } }' "$(NEMACS_LIBRARY_PACKAGE_DEPS)"
	@awk -F '\t' 'NR > 1 && $$8 == "unmanifested-reusable" { count++ } END { if (count != 0) { printf "nemacs-library-gate: unmanifested reusable dependency rows=%d\n", count; exit 1 } }' "$(NEMACS_LIBRARY_PACKAGE_DEPS)"
	@awk -F '\t' 'NR > 1 && $$8 == "lazy-unmanifested-reusable" { count++ } END { if (count != 0) { printf "nemacs-library-gate: lazy unmanifested reusable dependency rows=%d\n", count; exit 1 } }' "$(NEMACS_LIBRARY_PACKAGE_DEPS)"
	@awk -F '\t' 'NR > 1 && $$8 == "external-or-host" { count++ } END { if (count != 0) { printf "nemacs-library-gate: unknown external dependency rows=%d\n", count; exit 1 } }' "$(NEMACS_LIBRARY_PACKAGE_DEPS)"
	@awk 'NR > 1 { count++ } END { if (count != 0) { printf "nemacs-library-gate: boundary rows=%d\n", count; exit 1 } }' "$(NEMACS_LIBRARY_BOUNDARY_REPORT)"
	@echo "nemacs-library-gate: ok"

nemacs-library-consumer-smoke:
	$(EMACS) -Q -L src -l test/nelisp-emacs-consumer-smoke-test.el \
		-f ert-run-tests-batch-and-exit

nemacs-library-contract:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-contract-output "$(abspath $(NEMACS_LIBRARY_CONTRACT))")' \
		--eval '(setq nemacs-library-contract-summary-output "$(abspath $(NEMACS_LIBRARY_CONTRACT_SUMMARY))")' \
		-l scripts/nemacs-library-contract.el \
		-f nemacs-library-contract-batch

nemacs-library-package-smoke:
	@set -e; \
	for feature in $(NEMACS_LIBRARY_PACKAGE_SMOKE_FEATURES); do \
		echo "nemacs-library-package-smoke: $$feature"; \
		NEMACS_LIBRARY_PACKAGE_SMOKE_FEATURE="$$feature" \
			$(EMACS) -Q -L src -L scripts \
			-l scripts/nemacs-library-package-smoke.el \
			-f nemacs-library-package-smoke-batch; \
	done

nemacs-library-package-path-smoke: nemacs-library-package-scaffold
	@set -e; \
	for feature in $(NEMACS_LIBRARY_PACKAGE_SMOKE_FEATURES); do \
		echo "nemacs-library-package-path-smoke: $$feature"; \
		NEMACS_LIBRARY_PACKAGE_SMOKE_FEATURE="$$feature" \
			$(EMACS) -Q $(NEMACS_LIBRARY_PACKAGE_LOAD_PATH) -L scripts \
			-l scripts/nemacs-library-package-smoke.el \
			-f nemacs-library-package-smoke-batch; \
	done

nemacs-library-package-consumer-smoke: nemacs-library-package-scaffold
	$(EMACS) -Q $(NEMACS_LIBRARY_PACKAGE_HOST_ERT_PRELUDE) \
		$(NEMACS_LIBRARY_PACKAGE_LOAD_PATH) \
		-l test/nelisp-emacs-consumer-smoke-test.el \
		-f ert-run-tests-batch-and-exit

nemacs-library-package-lazy-smoke: nemacs-library-package-scaffold
	@set -e; \
	for feature in $(NEMACS_LIBRARY_PACKAGE_LAZY_SMOKE_FEATURES); do \
		echo "nemacs-library-package-lazy-smoke: $$feature"; \
		$(EMACS) -Q $(NEMACS_LIBRARY_PACKAGE_LOAD_PATH) \
			--eval "(require '$$feature)" \
			--eval "(unless (featurep '$$feature) (kill-emacs 1))"; \
	done

nemacs-library-package-load-path: nemacs-library-package-scaffold
	@sh scripts/nemacs-library-package-load-path.sh

nemacs-library-package-frontend-smoke: nemacs-library-package-scaffold nemacs-library-app-scaffold
	@set -e; \
	for test_file in $(NEMACS_LIBRARY_PACKAGE_FRONTEND_SMOKE_TESTS); do \
		echo "nemacs-library-package-frontend-smoke: $$test_file"; \
		$(EMACS) -Q $(NEMACS_LIBRARY_PACKAGE_HOST_ERT_PRELUDE) \
			$(NEMACS_LIBRARY_PACKAGE_APP_LOAD_PATH) \
			-l "$$test_file" \
			-f ert-run-tests-batch-and-exit; \
	done

nemacs-library-package-gui-bridge-smoke: nemacs-library-package-scaffold nemacs-library-app-scaffold
	NEMACS_GUI_BRIDGE_PACKAGE_SCAFFOLD_ROOT="$(abspath packages)" \
		NEMACS_GUI_BRIDGE_APP_SCAFFOLD_ROOT="$(abspath packages/nelisp-emacs-app-gui)" \
		$(EMACS) -Q $(NEMACS_LIBRARY_PACKAGE_HOST_ERT_PRELUDE) \
		$(NEMACS_LIBRARY_PACKAGE_APP_LOAD_PATH) -L test -L scripts \
		-l test/nemacs-gui-file-bridge-runtime-test.el \
		--eval '(ert-run-tests-batch-and-exit (quote $(NEMACS_LIBRARY_PACKAGE_GUI_BRIDGE_SMOKE_SELECTOR)))'

nemacs-library-package-gui-bridge-standalone-smoke: nemacs-library-package-scaffold nemacs-library-app-scaffold
	test -x "$(NELISP_BIN)"
	NEMACS_RUN_GUI_BRIDGE=1 \
		NEMACS_GUI_BRIDGE_NELISP="$(abspath $(NELISP_BIN))" \
		NEMACS_GUI_BRIDGE_PACKAGE_SCAFFOLD_ROOT="$(abspath packages)" \
		NEMACS_GUI_BRIDGE_APP_SCAFFOLD_ROOT="$(abspath packages/nelisp-emacs-app-gui)" \
		$(EMACS) -Q $(NEMACS_LIBRARY_PACKAGE_HOST_ERT_PRELUDE) \
		$(NEMACS_LIBRARY_PACKAGE_APP_LOAD_PATH) -L test -L scripts \
		-l test/nemacs-gui-file-bridge-runtime-test.el \
		--eval '(ert-run-tests-batch-and-exit (quote $(NEMACS_LIBRARY_PACKAGE_GUI_BRIDGE_STANDALONE_SMOKE_SELECTOR)))'

nemacs-library-package-manifest:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-package-manifest-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_MANIFEST))")' \
		--eval '(setq nemacs-library-package-manifest-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_MANIFEST_SUMMARY))")' \
		-l scripts/nemacs-library-package-manifest.el \
		-f nemacs-library-package-manifest-batch

nemacs-library-package-deps:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-package-deps-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_DEPS))")' \
		--eval '(setq nemacs-library-package-deps-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_DEPS_SUMMARY))")' \
		--eval '(setq nemacs-library-package-deps-migration-queue-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_MIGRATION_QUEUE))")' \
		--eval '(setq nemacs-library-package-deps-migration-queue-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_MIGRATION_QUEUE_SUMMARY))")' \
		-l scripts/nemacs-library-package-deps.el \
		-f nemacs-library-package-deps-batch

nemacs-library-package-descriptors:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-package-descriptors-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_DESCRIPTORS))")' \
		--eval '(setq nemacs-library-package-descriptors-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_DESCRIPTORS_SUMMARY))")' \
		-l scripts/nemacs-library-package-descriptors.el \
		-f nemacs-library-package-descriptors-batch

nemacs-library-package-guide:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-package-guide-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_GUIDE))")' \
		--eval '(setq nemacs-library-package-guide-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_GUIDE_SUMMARY))")' \
		-l scripts/nemacs-library-package-guide.el \
		-f nemacs-library-package-guide-batch

nemacs-library-package-api:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-package-api-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_API))")' \
		--eval '(setq nemacs-library-package-api-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_API_SUMMARY))")' \
		-l scripts/nemacs-library-package-api.el \
		-f nemacs-library-package-api-batch

nemacs-library-package-catalog:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-package-catalog-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_CATALOG))")' \
		--eval '(setq nemacs-library-package-catalog-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_CATALOG_SUMMARY))")' \
		-l scripts/nemacs-library-package-catalog.el \
		-f nemacs-library-package-catalog-batch

nemacs-library-compat-api-policy:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-compat-api-policy-output "$(abspath $(NEMACS_LIBRARY_COMPAT_API_POLICY))")' \
		--eval '(setq nemacs-library-compat-api-policy-summary-output "$(abspath $(NEMACS_LIBRARY_COMPAT_API_POLICY_SUMMARY))")' \
		-l scripts/nemacs-library-compat-api-policy.el \
		-f nemacs-library-compat-api-policy-batch

nemacs-library-api-promotion-queue:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-api-promotion-queue-output "$(abspath $(NEMACS_LIBRARY_API_PROMOTION_QUEUE))")' \
		--eval '(setq nemacs-library-api-promotion-queue-summary-output "$(abspath $(NEMACS_LIBRARY_API_PROMOTION_QUEUE_SUMMARY))")' \
		-l scripts/nemacs-library-api-promotion-queue.el \
		-f nemacs-library-api-promotion-queue-batch

nemacs-library-package-layout:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-package-layout-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_LAYOUT))")' \
		--eval '(setq nemacs-library-package-layout-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_LAYOUT_SUMMARY))")' \
		-l scripts/nemacs-library-package-layout.el \
		-f nemacs-library-package-layout-batch

nemacs-library-package-scaffold: nemacs-library-package-layout nemacs-library-package-guide
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-package-scaffold-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_SCAFFOLD))")' \
		--eval '(setq nemacs-library-package-scaffold-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_SCAFFOLD_SUMMARY))")' \
		-l scripts/nemacs-library-package-scaffold.el \
		-f nemacs-library-package-scaffold-batch

nemacs-library-app-scaffold:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-app-scaffold-output "$(abspath $(NEMACS_LIBRARY_APP_SCAFFOLD))")' \
		--eval '(setq nemacs-library-app-scaffold-summary-output "$(abspath $(NEMACS_LIBRARY_APP_SCAFFOLD_SUMMARY))")' \
		-l scripts/nemacs-library-app-scaffold.el \
		-f nemacs-library-app-scaffold-batch

nemacs-library-app-boundary: nemacs-library-package-scaffold nemacs-library-app-scaffold
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-app-boundary-package-scaffold "$(abspath $(NEMACS_LIBRARY_PACKAGE_SCAFFOLD))")' \
		--eval '(setq nemacs-library-app-boundary-app-scaffold "$(abspath $(NEMACS_LIBRARY_APP_SCAFFOLD))")' \
		--eval '(setq nemacs-library-app-boundary-output "$(abspath $(NEMACS_LIBRARY_APP_BOUNDARY))")' \
		--eval '(setq nemacs-library-app-boundary-summary-output "$(abspath $(NEMACS_LIBRARY_APP_BOUNDARY_SUMMARY))")' \
		-l scripts/nemacs-library-app-boundary.el \
		-f nemacs-library-app-boundary-batch

nemacs-library-package-app-require-guard: nemacs-library-package-scaffold nemacs-library-app-scaffold
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-app-require-guard-package-scaffold "$(abspath $(NEMACS_LIBRARY_PACKAGE_SCAFFOLD))")' \
		--eval '(setq nemacs-library-package-app-require-guard-app-scaffold "$(abspath $(NEMACS_LIBRARY_APP_SCAFFOLD))")' \
		--eval '(setq nemacs-library-package-app-require-guard-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_APP_REQUIRE_GUARD))")' \
		--eval '(setq nemacs-library-package-app-require-guard-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_APP_REQUIRE_GUARD_SUMMARY))")' \
		-l scripts/nemacs-library-package-app-require-guard.el \
		-f nemacs-library-package-app-require-guard-batch

nemacs-library-package-metadata: nemacs-library-package-scaffold
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-package-metadata-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA))")' \
		--eval '(setq nemacs-library-package-metadata-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA_SUMMARY))")' \
		-l scripts/nemacs-library-package-metadata.el \
		-f nemacs-library-package-metadata-batch

nemacs-library-package-install-smoke: nemacs-library-package-metadata
	mkdir -p "$(dir $(NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE))"
	@printf 'package_id\tstatus\tloader_feature\tdependency_closure\tinstall_dirs\tmetadata_file\tmember_features\tsource_leaks\n' > "$(NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE)"
	@set -e; \
	for package in $$(awk -F '\t' 'NR > 1 { print $$1 }' "$(NEMACS_LIBRARY_PACKAGE_METADATA)"); do \
		echo "nemacs-library-package-install-smoke: $$package"; \
		output="$(abspath $(BUILD_DIR))/nemacs-library-package-install-smoke/$$package.tsv"; \
		NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE_PACKAGE="$$package" \
			$(EMACS) -Q -L scripts \
			--eval '(setq nemacs-library-package-install-smoke-metadata "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA))")' \
			--eval '(setq nemacs-library-package-install-smoke-scaffold "$(abspath $(NEMACS_LIBRARY_PACKAGE_SCAFFOLD))")' \
			--eval '(setq nemacs-library-package-install-smoke-install-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE_ROOT))")' \
			--eval "(setq nemacs-library-package-install-smoke-output \"$$output\")" \
			-l scripts/nemacs-library-package-install-smoke.el \
			-f nemacs-library-package-install-smoke-batch; \
		awk 'NR > 1 { print }' "$$output" >> "$(NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE)"; \
	done
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-install-smoke-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE))")' \
		--eval '(setq nemacs-library-package-install-smoke-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_INSTALL_SMOKE_SUMMARY))")' \
		-l scripts/nemacs-library-package-install-smoke.el \
		-f nemacs-library-package-install-smoke-summary-batch

nemacs-library-package-archive: nemacs-library-package-metadata
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-archive-metadata "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA))")' \
		--eval '(setq nemacs-library-package-archive-scaffold "$(abspath $(NEMACS_LIBRARY_PACKAGE_SCAFFOLD))")' \
		--eval '(setq nemacs-library-package-archive-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_ROOT))")' \
		--eval '(setq nemacs-library-package-archive-staging-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_STAGING_ROOT))")' \
		--eval '(setq nemacs-library-package-archive-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE))")' \
		--eval '(setq nemacs-library-package-archive-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_SUMMARY))")' \
		-l scripts/nemacs-library-package-archive.el \
		-f nemacs-library-package-archive-batch

nemacs-library-package-archive-checksum: nemacs-library-package-archive
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-archive-checksum-metadata "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA))")' \
		--eval '(setq nemacs-library-package-archive-checksum-scaffold "$(abspath $(NEMACS_LIBRARY_PACKAGE_SCAFFOLD))")' \
		--eval '(setq nemacs-library-package-archive-checksum-archives "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE))")' \
		--eval '(setq nemacs-library-package-archive-checksum-rebuild-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM_ROOT))")' \
		--eval '(setq nemacs-library-package-archive-checksum-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM))")' \
		--eval '(setq nemacs-library-package-archive-checksum-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM_SUMMARY))")' \
		-l scripts/nemacs-library-package-archive-checksum.el \
		-f nemacs-library-package-archive-checksum-batch

nemacs-library-package-archive-smoke: nemacs-library-package-archive
	mkdir -p "$(dir $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE))"
	@printf 'package_id\tstatus\tloader_feature\tdependency_closure\tarchives\tpackage_user_dir\tmember_features\tsource_leaks\n' > "$(NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE)"
	@set -e; \
	for package in $$(awk -F '\t' 'NR > 1 { print $$1 }' "$(NEMACS_LIBRARY_PACKAGE_METADATA)"); do \
		echo "nemacs-library-package-archive-smoke: $$package"; \
		output="$(abspath $(BUILD_DIR))/nemacs-library-package-archive-smoke/$$package.tsv"; \
		NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE_PACKAGE="$$package" \
			$(EMACS) -Q -L scripts \
			--eval '(setq nemacs-library-package-archive-smoke-metadata "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA))")' \
			--eval '(setq nemacs-library-package-archive-smoke-archives "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE))")' \
			--eval '(setq nemacs-library-package-archive-smoke-install-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE_ROOT))")' \
			--eval "(setq nemacs-library-package-archive-smoke-output \"$$output\")" \
			-l scripts/nemacs-library-package-archive-smoke.el \
			-f nemacs-library-package-archive-smoke-batch; \
		awk 'NR > 1 { print }' "$$output" >> "$(NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE)"; \
	done
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-archive-smoke-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE))")' \
		--eval '(setq nemacs-library-package-archive-smoke-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_SMOKE_SUMMARY))")' \
		-l scripts/nemacs-library-package-archive-smoke.el \
		-f nemacs-library-package-archive-smoke-summary-batch

nemacs-library-package-archive-index: nemacs-library-package-archive
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-archive-index-metadata "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA))")' \
		--eval '(setq nemacs-library-package-archive-index-archives "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE))")' \
		--eval '(setq nemacs-library-package-archive-index-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_ROOT))")' \
		--eval '(setq nemacs-library-package-archive-index-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_INDEX))")' \
		--eval '(setq nemacs-library-package-archive-index-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_INDEX_SUMMARY))")' \
		-l scripts/nemacs-library-package-archive-index.el \
		-f nemacs-library-package-archive-index-batch

nemacs-library-package-index-smoke: nemacs-library-package-archive-index
	mkdir -p "$(dir $(NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE))"
	@printf 'package_id\tstatus\tloader_feature\tdeclared_dependencies\tinstalled_dependencies\tarchive_location\tpackage_user_dir\tmember_features\tsource_leaks\n' > "$(NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE)"
	@set -e; \
	for package in $$(awk -F '\t' 'NR > 1 { print $$1 }' "$(NEMACS_LIBRARY_PACKAGE_METADATA)"); do \
		echo "nemacs-library-package-index-smoke: $$package"; \
		output="$(abspath $(BUILD_DIR))/nemacs-library-package-index-smoke/$$package.tsv"; \
		NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE_PACKAGE="$$package" \
			$(EMACS) -Q -L scripts \
			--eval '(setq nemacs-library-package-index-smoke-metadata "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA))")' \
			--eval '(setq nemacs-library-package-index-smoke-archive-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_ROOT))")' \
			--eval '(setq nemacs-library-package-index-smoke-install-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE_ROOT))")' \
			--eval "(setq nemacs-library-package-index-smoke-output \"$$output\")" \
			-l scripts/nemacs-library-package-index-smoke.el \
			-f nemacs-library-package-index-smoke-batch; \
		awk 'NR > 1 { print }' "$$output" >> "$(NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE)"; \
	done
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-index-smoke-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE))")' \
		--eval '(setq nemacs-library-package-index-smoke-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_INDEX_SMOKE_SUMMARY))")' \
		-l scripts/nemacs-library-package-index-smoke.el \
		-f nemacs-library-package-index-smoke-summary-batch

nemacs-library-package-publication-policy: nemacs-library-package-archive-checksum nemacs-library-package-archive-index
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-publication-policy-metadata "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA))")' \
		--eval '(setq nemacs-library-package-publication-policy-checksum "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM))")' \
		--eval '(setq nemacs-library-package-publication-policy-index "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_INDEX))")' \
		--eval '(setq nemacs-library-package-publication-policy-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_PUBLICATION_POLICY))")' \
		--eval '(setq nemacs-library-package-publication-policy-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_PUBLICATION_POLICY_SUMMARY))")' \
		-l scripts/nemacs-library-package-publication-policy.el \
		-f nemacs-library-package-publication-policy-batch

nemacs-library-package-release-key-policy:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-release-key-policy-public-key-file "$(abspath $(NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_FILE))")' \
		--eval '(setq nemacs-library-package-release-key-policy-key-fingerprint "$(NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT)")' \
		--eval '(setq nemacs-library-package-release-key-policy-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_KEY_POLICY))")' \
		--eval '(setq nemacs-library-package-release-key-policy-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_KEY_POLICY_SUMMARY))")' \
		--eval '(setq nemacs-library-package-release-key-policy-strict $(NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_STRICT_ELISP))' \
		--eval '(setq nemacs-library-package-release-key-policy-gpg-program "$(NEMACS_LIBRARY_RELEASE_GPG_PROGRAM)")' \
		-l scripts/nemacs-library-package-release-key-policy.el \
		-f nemacs-library-package-release-key-policy-batch

nemacs-library-package-signature-policy: nemacs-library-package-publication-policy nemacs-library-package-release-key-policy
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-signature-policy-checksum "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM))")' \
		--eval '(setq nemacs-library-package-signature-policy-index "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_INDEX))")' \
		--eval '(setq nemacs-library-package-signature-policy-archive-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_ROOT))")' \
		--eval '(setq nemacs-library-package-signature-policy-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_POLICY))")' \
		--eval '(setq nemacs-library-package-signature-policy-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_POLICY_SUMMARY))")' \
		--eval '(setq nemacs-library-package-signature-policy-release-strict $(NEMACS_LIBRARY_RELEASE_SIGNATURE_STRICT_ELISP))' \
		--eval '(setq nemacs-library-package-signature-policy-key-fingerprint "$(NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT)")' \
		--eval '(setq nemacs-library-package-signature-policy-public-key-file "$(abspath $(NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_FILE))")' \
		--eval '(setq nemacs-library-package-signature-policy-signature-suffix "$(NEMACS_LIBRARY_RELEASE_SIGNATURE_SUFFIX)")' \
		--eval '(setq nemacs-library-package-signature-policy-gpg-program "$(NEMACS_LIBRARY_RELEASE_GPG_PROGRAM)")' \
		-l scripts/nemacs-library-package-signature-policy.el \
		-f nemacs-library-package-signature-policy-batch

nemacs-library-package-signature-release-sign: nemacs-library-package-signature-policy
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-signature-release-sign-policy "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_POLICY))")' \
		--eval '(setq nemacs-library-package-signature-release-sign-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SIGN))")' \
		--eval '(setq nemacs-library-package-signature-release-sign-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SIGN_SUMMARY))")' \
		--eval '(setq nemacs-library-package-signature-release-sign-key-fingerprint "$(NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT)")' \
		--eval '(setq nemacs-library-package-signature-release-sign-gpg-program "$(NEMACS_LIBRARY_RELEASE_GPG_PROGRAM)")' \
		--eval '(setq nemacs-library-package-signature-release-sign-armor $(NEMACS_LIBRARY_RELEASE_SIGNATURE_ARMOR_ELISP))' \
		-l scripts/nemacs-library-package-signature-release-sign.el \
		-f nemacs-library-package-signature-release-sign-batch

nemacs-library-package-signature-release-verify:
	$(MAKE) nemacs-library-package-release-key-policy \
		NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_STRICT=1
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-signature-policy-checksum "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM))")' \
		--eval '(setq nemacs-library-package-signature-policy-index "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_INDEX))")' \
		--eval '(setq nemacs-library-package-signature-policy-archive-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_ROOT))")' \
		--eval '(setq nemacs-library-package-signature-policy-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE))")' \
		--eval '(setq nemacs-library-package-signature-policy-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SUMMARY))")' \
		--eval '(setq nemacs-library-package-signature-policy-release-strict t)' \
		--eval '(setq nemacs-library-package-signature-policy-key-fingerprint "$(NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT)")' \
		--eval '(setq nemacs-library-package-signature-policy-public-key-file "$(abspath $(NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_FILE))")' \
		--eval '(setq nemacs-library-package-signature-policy-signature-suffix "$(NEMACS_LIBRARY_RELEASE_SIGNATURE_SUFFIX)")' \
		--eval '(setq nemacs-library-package-signature-policy-gpg-program "$(NEMACS_LIBRARY_RELEASE_GPG_PROGRAM)")' \
		-l scripts/nemacs-library-package-signature-policy.el \
		-f nemacs-library-package-signature-policy-batch

nemacs-library-package-signature-release: nemacs-library-package-signature-release-sign nemacs-library-package-signature-release-verify

nemacs-library-package-release-bundle-manifest:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-release-bundle-manifest-release-key-policy "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_KEY_POLICY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-release-key-policy-summary "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_KEY_POLICY_SUMMARY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-release-public-key-file "$(abspath $(NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_FILE))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-signature-policy "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_POLICY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-signature-policy-summary "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_POLICY_SUMMARY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-archive-checksum "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-archive-checksum-summary "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM_SUMMARY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-archive-index "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_INDEX))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-archive-index-summary "$(abspath $(NEMACS_LIBRARY_PACKAGE_ARCHIVE_INDEX_SUMMARY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-publication-policy "$(abspath $(NEMACS_LIBRARY_PACKAGE_PUBLICATION_POLICY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-publication-policy-summary "$(abspath $(NEMACS_LIBRARY_PACKAGE_PUBLICATION_POLICY_SUMMARY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-dependency-policy "$(abspath $(NEMACS_LIBRARY_PACKAGE_DEPENDENCY_PUBLICATION_POLICY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-dependency-policy-summary "$(abspath $(NEMACS_LIBRARY_PACKAGE_DEPENDENCY_PUBLICATION_POLICY_SUMMARY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-release-sign "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SIGN))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-release-sign-summary "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SIGN_SUMMARY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-release-verify "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-release-verify-summary "$(abspath $(NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SUMMARY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-bundle-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_ROOT))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_MANIFEST))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_MANIFEST_SUMMARY))")' \
		--eval '(setq nemacs-library-package-release-bundle-manifest-strict-release $(NEMACS_LIBRARY_RELEASE_BUNDLE_STRICT_ELISP))' \
		-l scripts/nemacs-library-package-release-bundle-manifest.el \
		-f nemacs-library-package-release-bundle-manifest-batch

nemacs-library-package-release-bundle-smoke: nemacs-library-package-release-bundle-manifest
	mkdir -p "$(dir $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE))"
	@printf 'package_id\tstatus\tloader_feature\tdeclared_dependencies\tinstalled_dependencies\tbundle_archive_location\tpackage_user_dir\tmember_features\tmanifest_retained\tmanifest_pending\tmanifest_archive_artifacts\tsource_leaks\n' > "$(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE)"
	@set -e; \
	for package in $$(awk -F '\t' 'NR > 1 { print $$1 }' "$(NEMACS_LIBRARY_PACKAGE_METADATA)"); do \
		echo "nemacs-library-package-release-bundle-smoke: $$package"; \
		output="$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_WORK_ROOT))/$$package.tsv"; \
		mkdir -p "$$(dirname "$$output")"; \
		NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_PACKAGE="$$package" \
			$(EMACS) -Q -L scripts \
			--eval '(setq nemacs-library-package-release-bundle-smoke-metadata "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA))")' \
			--eval '(setq nemacs-library-package-release-bundle-smoke-manifest "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_MANIFEST))")' \
			--eval '(setq nemacs-library-package-release-bundle-smoke-bundle-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_ROOT))")' \
			--eval '(setq nemacs-library-package-release-bundle-smoke-install-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_ROOT))")' \
			--eval "(setq nemacs-library-package-release-bundle-smoke-output \"$$output\")" \
			-l scripts/nemacs-library-package-release-bundle-smoke.el \
			-f nemacs-library-package-release-bundle-smoke-batch; \
		awk 'NR > 1 { print }' "$$output" >> "$(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE)"; \
	done
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-release-bundle-smoke-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE))")' \
		--eval '(setq nemacs-library-package-release-bundle-smoke-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_SUMMARY))")' \
		-l scripts/nemacs-library-package-release-bundle-smoke.el \
		-f nemacs-library-package-release-bundle-smoke-summary-batch

nemacs-library-package-release-publication-policy: nemacs-library-package-release-bundle-smoke
	$(MAKE) nemacs-library-package-release-publication-policy-run

nemacs-library-package-release-publication-policy-run:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-release-publication-policy-manifest "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_MANIFEST))")' \
		--eval '(setq nemacs-library-package-release-publication-policy-smoke "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE))")' \
		--eval '(setq nemacs-library-package-release-publication-policy-bundle-root "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_ROOT))")' \
		--eval '(setq nemacs-library-package-release-publication-policy-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_PUBLICATION_POLICY))")' \
		--eval '(setq nemacs-library-package-release-publication-policy-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_PUBLICATION_POLICY_SUMMARY))")' \
		--eval '(setq nemacs-library-package-release-publication-policy-strict-release $(NEMACS_LIBRARY_RELEASE_PUBLICATION_STRICT_ELISP))' \
		-l scripts/nemacs-library-package-release-publication-policy.el \
		-f nemacs-library-package-release-publication-policy-batch

nemacs-library-package-release-bundle: nemacs-library-package-dependency-publication-policy nemacs-library-package-signature-release
	$(MAKE) nemacs-library-package-release-bundle-manifest \
		NEMACS_LIBRARY_RELEASE_BUNDLE_STRICT=1 \
		NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_STRICT=1 \
		NEMACS_LIBRARY_RELEASE_SIGNATURE_STRICT=1
	$(MAKE) nemacs-library-package-release-bundle-smoke \
		NEMACS_LIBRARY_RELEASE_BUNDLE_STRICT=1 \
		NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_STRICT=1 \
		NEMACS_LIBRARY_RELEASE_SIGNATURE_STRICT=1
	$(MAKE) nemacs-library-package-release-publication-policy-run \
		NEMACS_LIBRARY_RELEASE_PUBLICATION_STRICT=1 \
		NEMACS_LIBRARY_RELEASE_BUNDLE_STRICT=1 \
		NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_STRICT=1 \
		NEMACS_LIBRARY_RELEASE_SIGNATURE_STRICT=1

nemacs-library-package-release-rehearsal-key:
	scripts/nemacs-library-package-release-rehearsal-key.sh \
		"$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_GNUPGHOME))" \
		"$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_PUBLIC_KEY))" \
		"$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_KEY))" \
		"$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_KEY_SUMMARY))" \
		"$(NEMACS_LIBRARY_RELEASE_GPG_PROGRAM)" \
		"$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_KEY_UID)"

nemacs-library-package-release-rehearsal: nemacs-library-package-release-rehearsal-key
	@set -e; \
	fingerprint="$$(awk -F '\t' 'NR > 1 && $$1 == "fingerprint" && $$2 == "ok" { print $$3; exit }' "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_KEY)")"; \
	if [ -z "$$fingerprint" ]; then \
		echo "nemacs-library-package-release-rehearsal: missing rehearsal key fingerprint" >&2; \
		exit 1; \
	fi; \
	GNUPGHOME="$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_GNUPGHOME))" \
		$(MAKE) nemacs-library-package-release-bundle \
		NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_FILE="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_PUBLIC_KEY)" \
		NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT="$$fingerprint" \
		NEMACS_LIBRARY_PACKAGE_ARCHIVE="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/archive.tsv" \
		NEMACS_LIBRARY_PACKAGE_ARCHIVE_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/archive.org" \
		NEMACS_LIBRARY_PACKAGE_ARCHIVE_ROOT="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/archives" \
		NEMACS_LIBRARY_PACKAGE_ARCHIVE_STAGING_ROOT="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/archive-staging" \
		NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/archive-checksum.tsv" \
		NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/archive-checksum.org" \
		NEMACS_LIBRARY_PACKAGE_ARCHIVE_CHECKSUM_ROOT="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/archive-checksum" \
		NEMACS_LIBRARY_PACKAGE_ARCHIVE_INDEX="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/archive-index.tsv" \
		NEMACS_LIBRARY_PACKAGE_ARCHIVE_INDEX_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/archive-index.org" \
		NEMACS_LIBRARY_PACKAGE_PUBLICATION_POLICY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/publication-policy.tsv" \
		NEMACS_LIBRARY_PACKAGE_PUBLICATION_POLICY_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/publication-policy.org" \
		NEMACS_LIBRARY_PACKAGE_RELEASE_KEY_POLICY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-key-policy.tsv" \
		NEMACS_LIBRARY_PACKAGE_RELEASE_KEY_POLICY_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-key-policy.org" \
		NEMACS_LIBRARY_PACKAGE_SIGNATURE_POLICY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/signature-policy.tsv" \
		NEMACS_LIBRARY_PACKAGE_SIGNATURE_POLICY_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/signature-policy.org" \
		NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SIGN="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/signature-release-sign.tsv" \
		NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SIGN_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/signature-release-sign.org" \
		NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/signature-release.tsv" \
		NEMACS_LIBRARY_PACKAGE_SIGNATURE_RELEASE_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/signature-release.org" \
		NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_MANIFEST="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-bundle-manifest.tsv" \
		NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_MANIFEST_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-bundle-manifest.org" \
		NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_ROOT="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/bundle" \
		NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-bundle-smoke.tsv" \
		NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-bundle-smoke.org" \
		NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_ROOT="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/smoke-install" \
		NEMACS_LIBRARY_PACKAGE_RELEASE_BUNDLE_SMOKE_WORK_ROOT="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-bundle-smoke-work" \
		NEMACS_LIBRARY_PACKAGE_RELEASE_PUBLICATION_POLICY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-publication-policy.tsv" \
		NEMACS_LIBRARY_PACKAGE_RELEASE_PUBLICATION_POLICY_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-publication-policy.org"; \
	key_ok="$$(awk -F '\t' 'NR > 1 && $$8 == "ok" { count++ } END { print count + 0 }' "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-key-policy.tsv")"; \
	sign_fail="$$(awk -F '\t' 'NR > 1 && $$9 != "ok" { count++ } END { print count + 0 }' "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/signature-release-sign.tsv")"; \
	verify_fail="$$(awk -F '\t' 'NR > 1 && $$8 != "ok" { count++ } END { print count + 0 }' "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/signature-release.tsv")"; \
	retained="$$(awk -F '\t' 'NR > 1 && $$8 == "yes" { count++ } END { print count + 0 }' "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-bundle-manifest.tsv")"; \
	pending="$$(awk -F '\t' 'NR > 1 && $$10 == "pending" { count++ } END { print count + 0 }' "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-bundle-manifest.tsv")"; \
	bundle_fail="$$(awk -F '\t' 'NR > 1 && $$10 == "fail" { count++ } END { print count + 0 }' "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-bundle-manifest.tsv")"; \
	publication_fail="$$(awk -F '\t' 'NR > 1 && $$4 == "fail" { count++ } END { print count + 0 }' "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-publication-policy.tsv")"; \
	smoke_ok="$$(awk -F '\t' 'NR > 1 && $$2 == "ok" { count++ } END { print count + 0 }' "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-bundle-smoke.tsv")"; \
	smoke_fail="$$(awk -F '\t' 'NR > 1 && $$2 != "ok" { count++ } END { print count + 0 }' "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)/release-bundle-smoke.tsv")"; \
	mkdir -p "$(dir $(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL))"; \
	{ \
		printf 'check\tvalue\n'; \
		printf 'fingerprint\t%s\n' "$$fingerprint"; \
		printf 'release-key-policy-ok\t%s\n' "$$key_ok"; \
		printf 'signature-release-sign-failures\t%s\n' "$$sign_fail"; \
		printf 'signature-release-verify-failures\t%s\n' "$$verify_fail"; \
		printf 'bundle-retained-files\t%s\n' "$$retained"; \
		printf 'bundle-pending-files\t%s\n' "$$pending"; \
		printf 'bundle-failures\t%s\n' "$$bundle_fail"; \
		printf 'publication-policy-failures\t%s\n' "$$publication_fail"; \
		printf 'bundle-smoke-ok\t%s\n' "$$smoke_ok"; \
		printf 'bundle-smoke-failures\t%s\n' "$$smoke_fail"; \
	} > "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL)"; \
	{ \
		printf '#+TITLE: nemacs library package release rehearsal\n\n'; \
		printf '* Summary\n\n'; \
		printf -- '- fingerprint: =%s=\n' "$$fingerprint"; \
		printf -- '- release key policy ok: %s\n' "$$key_ok"; \
		printf -- '- signing failures: %s\n' "$$sign_fail"; \
		printf -- '- signature verification failures: %s\n' "$$verify_fail"; \
		printf -- '- bundle retained files: %s\n' "$$retained"; \
		printf -- '- bundle pending files: %s\n' "$$pending"; \
		printf -- '- bundle failures: %s\n' "$$bundle_fail"; \
		printf -- '- publication policy failures: %s\n' "$$publication_fail"; \
		printf -- '- bundle smoke ok: %s\n' "$$smoke_ok"; \
		printf -- '- bundle smoke failures: %s\n\n' "$$smoke_fail"; \
		printf '* Notes\n\n'; \
		printf -- '- This rehearsal uses a throwaway key generated under =%s=.\n' "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_ROOT)"; \
		printf -- '- It proves the strict signed release workflow, not public trust in the key.\n'; \
	} > "$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_SUMMARY)"; \
	echo "nemacs-library-package-release-rehearsal: fingerprint=$$fingerprint key-ok=$$key_ok sign-failures=$$sign_fail verify-failures=$$verify_fail bundle-failures=$$bundle_fail publication-failures=$$publication_fail smoke-failures=$$smoke_fail summary=$(NEMACS_LIBRARY_PACKAGE_RELEASE_REHEARSAL_SUMMARY)"

nemacs-library-package-release-config-check:
	mkdir -p "$(BUILD_DIR)"
	scripts/nemacs-library-package-release-config-check.sh \
		"$(abspath $(NEMACS_LIBRARY_RELEASE_CONFIG))" \
		"$(abspath $(NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_FILE))" \
		"$(NEMACS_LIBRARY_RELEASE_SIGNING_KEY_FINGERPRINT)" \
		"$(NEMACS_LIBRARY_RELEASE_GNUPGHOME)" \
		"$(NEMACS_LIBRARY_RELEASE_GPG_PROGRAM)" \
		"$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_CONFIG_CHECK))" \
		"$(abspath $(NEMACS_LIBRARY_PACKAGE_RELEASE_CONFIG_CHECK_SUMMARY))"

nemacs-library-package-release-ready: nemacs-library-package-release-config-check
	$(MAKE) nemacs-library-package-release-key-policy \
		NEMACS_LIBRARY_RELEASE_PUBLIC_KEY_STRICT=1

nemacs-library-package-release-from-config:
	$(MAKE) nemacs-library-package-release-ready
	$(MAKE) nemacs-library-package-release-bundle

nemacs-library-package-dependency-publication-policy: nemacs-library-package-metadata nemacs-library-package-scaffold nemacs-library-package-deps
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-dependency-publication-policy-metadata "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA))")' \
		--eval '(setq nemacs-library-package-dependency-publication-policy-scaffold "$(abspath $(NEMACS_LIBRARY_PACKAGE_SCAFFOLD))")' \
		--eval '(setq nemacs-library-package-dependency-publication-policy-deps "$(abspath $(NEMACS_LIBRARY_PACKAGE_DEPS))")' \
		--eval '(setq nemacs-library-package-dependency-publication-policy-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_DEPENDENCY_PUBLICATION_POLICY))")' \
		--eval '(setq nemacs-library-package-dependency-publication-policy-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_DEPENDENCY_PUBLICATION_POLICY_SUMMARY))")' \
		-l scripts/nemacs-library-package-dependency-publication-policy.el \
		-f nemacs-library-package-dependency-publication-policy-batch

nemacs-library-package-lazy-metadata: nemacs-library-package-metadata nemacs-library-package-scaffold nemacs-library-package-deps
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-package-lazy-metadata-metadata "$(abspath $(NEMACS_LIBRARY_PACKAGE_METADATA))")' \
		--eval '(setq nemacs-library-package-lazy-metadata-scaffold "$(abspath $(NEMACS_LIBRARY_PACKAGE_SCAFFOLD))")' \
		--eval '(setq nemacs-library-package-lazy-metadata-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_LAZY_METADATA))")' \
		--eval '(setq nemacs-library-package-lazy-metadata-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_LAZY_METADATA_SUMMARY))")' \
		-l scripts/nemacs-library-package-lazy-metadata.el \
		-f nemacs-library-package-lazy-metadata-batch

nemacs-library-package-vendor-lock: nemacs-library-package-dependency-publication-policy
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-package-vendor-lock-dependency-policy "$(abspath $(NEMACS_LIBRARY_PACKAGE_DEPENDENCY_PUBLICATION_POLICY))")' \
		--eval '(setq nemacs-library-package-vendor-lock-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_VENDOR_LOCK))")' \
		--eval '(setq nemacs-library-package-vendor-lock-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_VENDOR_LOCK_SUMMARY))")' \
		--eval '(setq nemacs-library-package-vendor-lock-release-strict $(NEMACS_LIBRARY_VENDOR_RELEASE_STRICT_ELISP))' \
		-l scripts/nemacs-library-package-vendor-lock.el \
		-f nemacs-library-package-vendor-lock-batch

nemacs-library-package-vendor-release-verify:
	$(MAKE) nemacs-library-package-vendor-lock \
		NEMACS_LIBRARY_VENDOR_RELEASE_STRICT=1 \
		NEMACS_LIBRARY_PACKAGE_VENDOR_LOCK="$(NEMACS_LIBRARY_PACKAGE_VENDOR_RELEASE_LOCK)" \
		NEMACS_LIBRARY_PACKAGE_VENDOR_LOCK_SUMMARY="$(NEMACS_LIBRARY_PACKAGE_VENDOR_RELEASE_LOCK_SUMMARY)"

nemacs-library-package-verify:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L src -L scripts \
		--eval '(setq nemacs-library-package-verify-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_VERIFY))")' \
		--eval '(setq nemacs-library-package-verify-summary-output "$(abspath $(NEMACS_LIBRARY_PACKAGE_VERIFY_SUMMARY))")' \
		-l scripts/nemacs-library-package-verify.el \
		-f nemacs-library-package-verify-batch

nemacs-runtime-image-input-inventory: build-nelisp-bootstrap nemacs-library-package-scaffold nemacs-library-app-scaffold
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-runtime-image-input-inventory-output "$(abspath $(NEMACS_RUNTIME_IMAGE_INPUT_INVENTORY))")' \
		--eval '(setq nemacs-runtime-image-input-inventory-summary-output "$(abspath $(NEMACS_RUNTIME_IMAGE_INPUT_SUMMARY))")' \
		--eval '(setq nemacs-runtime-image-input-inventory-package-scaffold "$(abspath $(NEMACS_LIBRARY_PACKAGE_SCAFFOLD))")' \
		--eval '(setq nemacs-runtime-image-input-inventory-app-scaffold "$(abspath $(NEMACS_LIBRARY_APP_SCAFFOLD))")' \
		-l scripts/nemacs-runtime-image-input-inventory.el \
		-f nemacs-runtime-image-input-inventory-batch

test:
	$(EMACS) --eval '(setq load-prefer-newer t)' \
		-L src -L test -L demo -L scripts $(NELISP_LOAD_PATH) \
		$(foreach t,$(TEST_UNIT_FILES),-l $(t)) \
		-f ert-run-tests-batch-and-exit

test-fast:
	$(EMACS) --eval '(setq load-prefer-newer t)' \
		-L src -L test -L demo -L scripts $(NELISP_LOAD_PATH) \
		$(foreach t,$(TEST_FAST_FILES),-l $(t)) \
		-f ert-run-tests-batch-and-exit

gate-nemacs-complete: test-fast nemacs-gui-keymap-coverage gui-bridge-runtime-inventory nemacs-stub-fallback-skip-inventory nemacs-dirty-review-units verify-nemacs-daily-driver test-nemacs-gui-bridge-gate
	@echo "gate-nemacs-complete: ok"

test-nemacs-gui-bridge:
	test -x "$(NELISP_BIN)"
	NEMACS_RUN_GUI_BRIDGE=1 NEMACS_GUI_BRIDGE_NELISP="$(abspath $(NELISP_BIN))" \
		$(EMACS) -L src -L test -L scripts $(NELISP_LOAD_PATH) \
		-l test/nemacs-gui-file-bridge-runtime-test.el \
		-f ert-run-tests-batch-and-exit

NEMACS_GUI_BRIDGE_TEST_SELECTOR ?= t
NEMACS_GUI_BRIDGE_SLOW_TESTS := \
	nemacs-gui-file-bridge-runtime-test/standalone-save-and-transform \
	nemacs-gui-file-bridge-runtime-test/standalone-goto-line \
	nemacs-gui-file-bridge-runtime-test/standalone-large-org-file \
	nemacs-gui-file-bridge-runtime-test/standalone-minibuffer-owned-help \
	nemacs-gui-file-bridge-runtime-test/standalone-tab-transport
NEMACS_GUI_BRIDGE_GATE_SELECTOR ?= (not (or $(NEMACS_GUI_BRIDGE_SLOW_TESTS)))
NEMACS_GUI_BRIDGE_SLOW_SELECTOR ?= (or $(NEMACS_GUI_BRIDGE_SLOW_TESTS))

test-nemacs-gui-bridge-gate:
	test -x "$(NELISP_BIN)"
	NEMACS_RUN_GUI_BRIDGE=1 NEMACS_GUI_BRIDGE_NELISP="$(abspath $(NELISP_BIN))" \
		$(EMACS) -L src -L test -L scripts $(NELISP_LOAD_PATH) \
		-l test/nemacs-gui-file-bridge-runtime-test.el \
		--eval '(ert-run-tests-batch-and-exit (quote $(NEMACS_GUI_BRIDGE_GATE_SELECTOR)))'

test-nemacs-gui-bridge-slow:
	test -x "$(NELISP_BIN)"
	NEMACS_RUN_GUI_BRIDGE=1 NEMACS_GUI_BRIDGE_NELISP="$(abspath $(NELISP_BIN))" \
		$(EMACS) -L src -L test -L scripts $(NELISP_LOAD_PATH) \
		-l test/nemacs-gui-file-bridge-runtime-test.el \
		--eval '(ert-run-tests-batch-and-exit (quote $(NEMACS_GUI_BRIDGE_SLOW_SELECTOR)))'

test-nemacs-gui-bridge-slow-profile:
	test -x "$(NELISP_BIN)"
	NEMACS_GUI_BRIDGE_PROFILE=1 NEMACS_RUN_GUI_BRIDGE=1 NEMACS_GUI_BRIDGE_NELISP="$(abspath $(NELISP_BIN))" \
		$(EMACS) -L src -L test -L scripts $(NELISP_LOAD_PATH) \
		-l test/nemacs-gui-file-bridge-runtime-test.el \
		--eval '(ert-run-tests-batch-and-exit (quote $(NEMACS_GUI_BRIDGE_SLOW_SELECTOR)))'

nemacs-gui-bridge-profile-summary:
	mkdir -p "$(BUILD_DIR)"
	test -f "$(NEMACS_GUI_BRIDGE_PROFILE_LOG)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-gui-bridge-profile-summary-input "$(abspath $(NEMACS_GUI_BRIDGE_PROFILE_LOG))")' \
		--eval '(setq nemacs-gui-bridge-profile-summary-output "$(abspath $(NEMACS_GUI_BRIDGE_PROFILE_SUMMARY))")' \
		-l scripts/nemacs-gui-bridge-profile-summary.el \
		-f nemacs-gui-bridge-profile-summary-batch

nemacs-gui-bridge-run-shape:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-gui-bridge-run-shape-output "$(abspath $(NEMACS_GUI_BRIDGE_RUN_SHAPE))")' \
		-l scripts/nemacs-gui-bridge-run-shape.el \
		-f nemacs-gui-bridge-run-shape-batch

test-nemacs-gui-bridge-select:
	test -x "$(NELISP_BIN)"
	NEMACS_RUN_GUI_BRIDGE=1 NEMACS_GUI_BRIDGE_NELISP="$(abspath $(NELISP_BIN))" \
		$(EMACS) -L src -L test -L scripts $(NELISP_LOAD_PATH) \
		-l test/nemacs-gui-file-bridge-runtime-test.el \
		--eval '(ert-run-tests-batch-and-exit (quote $(NEMACS_GUI_BRIDGE_TEST_SELECTOR)))'

test-nemacs-server-client:
	test -x "$(NELISP_BIN)"
	timeout $(NEMACS_SERVER_CLIENT_TIMEOUT) env NELISP="$(abspath $(NELISP_BIN))" \
		$(EMACS) --eval '(setq load-prefer-newer t)' \
		-L src -L test -L scripts $(NELISP_LOAD_PATH) \
		-l test/emacs-server-client-test.el \
		-f ert-run-tests-batch-and-exit

verify-production-runtime-path: build-nelisp-bootstrap nemacs-library-package-scaffold nemacs-library-app-scaffold
	$(EMACS) -Q -L scripts \
		--eval '(setq verify-production-runtime-path-bootstrap "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))")' \
		--eval '(setq verify-production-runtime-path-main "$(abspath src/nemacs-main.el)")' \
		--eval '(setq verify-production-runtime-path-package-scaffold "$(abspath $(NEMACS_LIBRARY_PACKAGE_SCAFFOLD))")' \
		--eval '(setq verify-production-runtime-path-app-scaffold "$(abspath $(NEMACS_LIBRARY_APP_SCAFFOLD))")' \
		--eval '(setq verify-production-runtime-path-summary-output "$(abspath $(NEMACS_PRODUCTION_RUNTIME_PATH_SUMMARY))")' \
		-l scripts/verify-production-runtime-path.el \
		-f verify-production-runtime-path-batch

verify-nemacs-daily-driver: verify-production-runtime-path
	scripts/verify-nemacs-tui.sh

SOAK_ITER ?= 20
soak:
	@$(EMACS) -L src $(NELISP_LOAD_PATH) --eval "(require 'standalone-soak)" --eval '(let ((r (standalone-soak-run $(SOAK_ITER))) (p (standalone-soak-process)) (s (standalone-soak-project-scan "src"))) (princ (standalone-soak-report-string r)) (terpri) (princ (format "process: ran=%s ok=%s\n" (plist-get p :ran) (plist-get p :ok))) (princ (format "project-scan src: files=%s dirs=%s\n" (plist-get s :files) (plist-get s :dirs))) (kill-emacs (if (and (= 0 (plist-get r :errors)) (or (not (plist-get p :ran)) (plist-get p :ok))) 0 1)))'

nemacs-gui-keymap-coverage:
	mkdir -p "$(BUILD_DIR)"
	@$(EMACS) -Q -L scripts \
		-l scripts/nemacs-gui-keymap-coverage.el \
		> "$(NEMACS_GUI_KEYMAP_COVERAGE_TSV)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-gui-keymap-coverage-summary-input "$(abspath $(NEMACS_GUI_KEYMAP_COVERAGE_TSV))")' \
		--eval '(setq nemacs-gui-keymap-coverage-summary-output "$(abspath $(NEMACS_GUI_KEYMAP_COVERAGE_SUMMARY))")' \
		--eval '(setq nemacs-gui-keymap-coverage-missing-output "$(abspath $(NEMACS_GUI_KEYMAP_COVERAGE_MISSING_TSV))")' \
		--eval '(setq nemacs-gui-keymap-coverage-command-missing-output "$(abspath $(NEMACS_GUI_KEYMAP_COVERAGE_COMMAND_MISSING_TSV))")' \
		--eval '(setq nemacs-gui-keymap-coverage-different-output "$(abspath $(NEMACS_GUI_KEYMAP_COVERAGE_DIFFERENT_TSV))")' \
		-l scripts/nemacs-gui-keymap-coverage-summary.el \
		-f nemacs-gui-keymap-coverage-summary-batch

gui-bridge-runtime-inventory:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-gui-bridge-runtime-inventory-output "$(abspath $(NEMACS_GUI_BRIDGE_RUNTIME_INVENTORY))")' \
		-l scripts/nemacs-gui-bridge-runtime-inventory.el \
		-f nemacs-gui-bridge-runtime-inventory-batch

nemacs-stub-fallback-skip-inventory:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-stub-fallback-skip-inventory-output "$(abspath $(NEMACS_STUB_FALLBACK_SKIP_INVENTORY))")' \
		--eval '(setq nemacs-stub-fallback-skip-inventory-summary-output "$(abspath $(NEMACS_STUB_FALLBACK_SKIP_SUMMARY))")' \
		-l scripts/nemacs-stub-fallback-skip-inventory.el \
		-f nemacs-stub-fallback-skip-inventory-batch

nemacs-dirty-review-units:
	scripts/nemacs-dirty-review-units.sh "$(NEMACS_DIRTY_REVIEW_UNITS)"

nemacs-library-boundary-report:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-library-boundary-report-output "$(abspath $(NEMACS_LIBRARY_BOUNDARY_REPORT))")' \
		--eval '(setq nemacs-library-boundary-report-summary-output "$(abspath $(NEMACS_LIBRARY_BOUNDARY_SUMMARY))")' \
		-l scripts/nemacs-library-boundary-report.el \
		-f nemacs-library-boundary-report-batch

nemacs-public-api-inventory:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-public-api-inventory-output "$(abspath $(NEMACS_PUBLIC_API_INVENTORY))")' \
		--eval '(setq nemacs-public-api-inventory-summary-output "$(abspath $(NEMACS_PUBLIC_API_SUMMARY))")' \
		-l scripts/nemacs-public-api-inventory.el \
		-f nemacs-public-api-inventory-batch

nemacs-ownership-coverage:
	mkdir -p "$(BUILD_DIR)"
	$(EMACS) -Q -L scripts \
		--eval '(setq nemacs-ownership-coverage-output "$(abspath $(NEMACS_OWNERSHIP_COVERAGE))")' \
		--eval '(setq nemacs-ownership-coverage-summary-output "$(abspath $(NEMACS_OWNERSHIP_COVERAGE_SUMMARY))")' \
		-l scripts/nemacs-ownership-coverage.el \
		-f nemacs-ownership-coverage-batch

gate5:
	NEMACS_NELISP_ROOT="$(abspath $(NELISP_ROOT))" $(EMACS) -Q -L scripts -L test \
		-L $(NELISP_ROOT)/src \
		-L $(NELISP_ROOT)/lisp \
		-l scripts/nemacs-artifact-gate5.el \
		-l test/nelisp-emacs-artifact-gate5-test.el \
		-f ert-run-tests-batch-and-exit

gate6:
	NEMACS_NELISP_ROOT="$(abspath $(NELISP_ROOT))" $(EMACS) -Q -L scripts -L test \
		$(NELISP_LOAD_PATH) \
		-L $(NELISP_ROOT)/lisp \
		-l scripts/nemacs-artifact-gate6.el \
		-l test/nelisp-emacs-artifact-gate6-test.el \
		-f ert-run-tests-batch-and-exit

vendor-nelc-cache:
	NEMACS_NELISP_ROOT="$(abspath $(NELISP_ROOT))" $(EMACS) -Q -L scripts -L test \
		-L $(NELISP_ROOT)/lisp \
		-L $(NELISP_ROOT)/src \
		-l scripts/nemacs-vendor-cache.el \
		-l test/nemacs-vendor-cache-test.el \
		-f ert-run-tests-batch-and-exit

vendor-nelc-cache-set:
	NEMACS_NELISP_ROOT="$(abspath $(NELISP_ROOT))" $(EMACS) -Q -L scripts -L test \
		-L $(NELISP_ROOT)/lisp \
		-L $(NELISP_ROOT)/src \
		-l scripts/nemacs-vendor-cache-set.el \
		-l test/nemacs-vendor-cache-set-test.el \
		-f ert-run-tests-batch-and-exit

test-redisplay-core-smoke:
	$(EMACS) -L src -L scripts \
		-l scripts/emacs-redisplay-core-smoke.el \
		-f ert-run-tests-batch-and-exit

# Regression smoke for the pure-elisp bidirectional subprocess layer in
# scripts/nemacs-runtime-process-preload.el — the interactive `make-process'
# + `process-send-string' + `accept-process-output' pattern that the portable
# IMAP engine (anvil-wl-imap.el) drives.  Runs on the NeLisp standalone
# reader (NOT host Emacs); asserts a live child round-trips a sent line back
# through its `:filter', and that a self-exiting child fires its sentinel.
test-nemacs-process-bidi-smoke: standalone-reader-passthrough
	test -x "$(NELISP_BIN)"
	@out="$$(timeout $(NELISP_BOOT_TIMEOUT) env \
		NEMACS_PROCESS_PRELOAD="$(abspath scripts/nemacs-runtime-process-preload.el)" \
		"$(NELISP_BIN)" --load "$(abspath test/nemacs-process-bidi-smoke.el)" 2>&1)"; \
	printf '%s\n' "$$out"; \
	if printf '%s\n' "$$out" | grep -q '^ROUNDTRIP=PASS$$' && \
	   printf '%s\n' "$$out" | grep -q '^EXIT-SENTINEL=PASS$$'; then \
	  echo "[test-nemacs-process-bidi-smoke] PASS"; \
	else \
	  echo "[test-nemacs-process-bidi-smoke] FAIL"; exit 1; \
	fi

# Ensure the standalone reader binary exists (build it if missing) without
# rebuilding when present, so the smoke can run cheaply.
standalone-reader-passthrough:
	@test -x "$(NELISP_BIN)" || $(MAKE) -C "$(NELISP_ROOT)" standalone-reader

doctor:
	NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		NEMACS_NELISP="$(abspath $(NELISP_BIN))" \
		NEMACS_NELISP_STACK="$(NELISP_STACK_LIMIT)" \
		./bin/nemacs --doctor

build-nelisp-bootstrap: $(NEMACS_BOOTSTRAP_BUNDLE)

$(NEMACS_BOOTSTRAP_BUNDLE): scripts/build-nelisp-bootstrap.el $(SRC_FILES)
	$(EMACS) -L src -L scripts $(NELISP_LOAD_PATH) \
		--eval '(setq nelisp-bootstrap-output-file "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))")' \
		--eval '(setq nelisp-bootstrap-repl-output-file "$(abspath $(NEMACS_BOOTSTRAP_REPL))")' \
		-l scripts/build-nelisp-bootstrap.el \
		-f nelisp-bootstrap-build-batch

bake-image: nemacs-library-package-scaffold nemacs-library-app-scaffold
	$(EMACS) -Q $(NEMACS_LIBRARY_PACKAGE_APP_LOAD_PATH) $(NELISP_LOAD_PATH) \
		--eval '(setq image-baker-output-file "$(abspath $(NEMACS_IMAGE))")' \
		-l image-baker \
		-f image-baker-bake-batch

bake-runtime-image: $(NEMACS_RUNTIME_IMAGE)

$(NEMACS_RUNTIME_IMAGE): $(NEMACS_BOOTSTRAP_BUNDLE) $(NEMACS_RUNTIME_PRELOAD) $(NEMACS_RUNTIME_PROCESS_PRELOAD) $(NEMACS_RUNTIME_FRAME_TAB_PRELOAD) nemacs-library-package-scaffold nemacs-library-app-scaffold
	test -x "$(NELISP_BIN)"
	mkdir -p "$(dir $(NEMACS_RUNTIME_IMAGE))"
	ulimit -s "$(NELISP_STACK_LIMIT)" 2>/dev/null || true; \
	timeout $(NEMACS_RUNTIME_BAKE_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		"$(NELISP_BIN)" dump-runtime-image "$(abspath $(NEMACS_RUNTIME_IMAGE)).tmp" \
		'(progn (load "$(abspath $(NEMACS_RUNTIME_PROCESS_PRELOAD))" nil (quote no-message) t t) (load "$(abspath $(NEMACS_RUNTIME_FRAME_TAB_PRELOAD))" nil (quote no-message) t t) (load "$(abspath $(NEMACS_RUNTIME_PRELOAD))" nil (quote no-message) t t) (nemacs-runtime-image-preload-batch "$(abspath .)" "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))"))'
	mv "$(NEMACS_RUNTIME_IMAGE).tmp" "$(NEMACS_RUNTIME_IMAGE)"

bake-interactive-runtime-image: $(NEMACS_INTERACTIVE_RUNTIME_IMAGE)

$(NEMACS_INTERACTIVE_RUNTIME_IMAGE): $(NEMACS_BOOTSTRAP_BUNDLE) $(NEMACS_RUNTIME_PRELOAD) $(NEMACS_RUNTIME_PROCESS_PRELOAD) $(NEMACS_RUNTIME_FRAME_TAB_PRELOAD) nemacs-library-package-scaffold nemacs-library-app-scaffold
	test -x "$(NELISP_BIN)"
	mkdir -p "$(dir $(NEMACS_INTERACTIVE_RUNTIME_IMAGE))"
	ulimit -s "$(NELISP_STACK_LIMIT)" 2>/dev/null || true; \
	timeout $(NEMACS_RUNTIME_BAKE_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		"$(NELISP_BIN)" dump-runtime-image "$(abspath $(NEMACS_INTERACTIVE_RUNTIME_IMAGE)).tmp" \
		'(progn (load "$(abspath $(NEMACS_RUNTIME_PROCESS_PRELOAD))" nil (quote no-message) t t) (load "$(abspath $(NEMACS_RUNTIME_FRAME_TAB_PRELOAD))" nil (quote no-message) t t) (load "$(abspath $(NEMACS_RUNTIME_PRELOAD))" nil (quote no-message) t t) (nemacs-runtime-image-preload-interactive "$(abspath .)" "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))"))'
	mv "$(NEMACS_INTERACTIVE_RUNTIME_IMAGE).tmp" "$(NEMACS_INTERACTIVE_RUNTIME_IMAGE)"

bake-vendor-core-runtime-image: nemacs-library-package-scaffold nemacs-library-app-scaffold
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
	  nelisp|nelisp-standalone-reader) \
	    tmp=$$(mktemp "$${TMPDIR:-/tmp}/nemacs-standalone-smoke.XXXXXX.el"); \
	    printf '%s\n' '(+ 40 2)' > "$$tmp"; \
	    set +e; timeout $(NELISP_BOOT_TIMEOUT) "$(NELISP_BIN)" --load "$$tmp"; rc=$$?; set -e; \
	    rm -f "$$tmp"; \
	    if [ "$$rc" -eq 42 ]; then echo "STANDALONE-READER=ok exit=42"; else echo "STANDALONE-READER=fail exit=$$rc expected=42"; exit 1; fi; \
	    out=$$(timeout $(NELISP_BOOT_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
	      NEMACS_NELISP="$(abspath $(NELISP_BIN))" \
	      NEMACS_NELISP_STACK="$(NELISP_STACK_LIMIT)" \
	      NEMACS_RUNTIME_IMAGE= \
	      ./bin/nemacs --driver=nelisp --batch --no-banner \
	      --eval '(if (and (fboundp (quote nemacs-batch-main)) (featurep (quote nemacs-main))) (nelisp--write-stdout-bytes "NEMACS-STANDALONE-BOOT=ok\n") (nelisp--write-stdout-bytes "NEMACS-STANDALONE-BOOT=fail\n"))'); \
	    printf '%s\n' "$$out"; \
	    printf '%s\n' "$$out" | grep -q '^NEMACS-STANDALONE-BOOT=ok$$' ;; \
	  *) \
	    timeout $(NELISP_BOOT_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
	      NEMACS_NELISP="$(abspath $(NELISP_BIN))" \
	      NEMACS_NELISP_STACK="$(NELISP_STACK_LIMIT)" \
	      NEMACS_RUNTIME_IMAGE= \
	      ./bin/nemacs --driver=nelisp --batch --no-banner \
	      --eval '(if (fboundp (quote nelisp--write-stdout-bytes)) (nelisp--write-stdout-bytes "BOOT=t\n") (princ "BOOT=t\n"))' ;; \
	esac

test-nelisp-runtime-image: bake-runtime-image
	test -x "$(NELISP_BIN)"
	out=$$(timeout $(NEMACS_RUNTIME_REPLAY_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		"$(NELISP_BIN)" exec-runtime-image "$(abspath $(NEMACS_RUNTIME_IMAGE))" \
		'(nelisp--write-stdout-bytes "BOOT=t\n")'); \
	printf '%s\n' "$$out"; \
	printf '%s\n' "$$out" | grep -q '^BOOT=t$$'

test-nelisp-interactive-runtime-image: bake-interactive-runtime-image
	test -x "$(NELISP_BIN)"
	out=$$(timeout $(NEMACS_INTERACTIVE_RUNTIME_REPLAY_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		"$(NELISP_BIN)" exec-runtime-image "$(abspath $(NEMACS_INTERACTIVE_RUNTIME_IMAGE))" \
		'(nelisp--write-stdout-bytes "TUI=t\n")'); \
	printf '%s\n' "$$out"; \
	printf '%s\n' "$$out" | grep -q '^TUI=t$$'

test-nelisp-vendor-core-runtime-image: bake-vendor-core-runtime-image
	test -x "$(NELISP_BIN)"
	out=$$(timeout $(NEMACS_VENDOR_CORE_RUNTIME_REPLAY_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		"$(NELISP_BIN)" exec-runtime-image "$(abspath $(NEMACS_VENDOR_CORE_RUNTIME_IMAGE))" \
		'(nelisp--write-stdout-bytes "VENDOR-CORE=t\n")'); \
	printf '%s\n' "$$out"; \
	printf '%s\n' "$$out" | grep -q '^VENDOR-CORE=t$$'

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
		--eval '(setq standalone-bootstrap-profile-prelude "$(VENDOR_LOAD_PRELUDE)")' \
		--eval '(setq standalone-bootstrap-profile-repo-root "$(abspath .)")' \
		--eval '(setq standalone-bootstrap-profile-limit "$(NELISP_BOOT_PROFILE_LIMIT)")' \
		--eval '(setq standalone-bootstrap-profile-timeout "$(NELISP_BOOT_PROFILE_TIMEOUT)")' \
		-l standalone-bootstrap-profile \
		-f standalone-bootstrap-profile-batch

diagnose-vendor-form-walk: build-nelisp-bootstrap
	test -x "$(NELISP_BIN)"
	timeout $(VENDOR_FORM_WALK_TIMEOUT) $(EMACS) -Q -L scripts \
		--eval '(setq vendor-form-standalone-reader "$(abspath $(NELISP_BIN))")' \
		--eval '(setq vendor-form-standalone-bootstrap "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))")' \
		--eval '(setq vendor-form-standalone-prelude "$(VENDOR_FORM_WALK_PRELUDE)")' \
		--eval '(setq vendor-form-standalone-file "$(VENDOR_FORM_WALK_FILE)")' \
		--eval '(setq vendor-form-standalone-preload-files "$(VENDOR_FORM_WALK_PRELOAD_FILES)")' \
		--eval '(setq vendor-form-standalone-start-index $(VENDOR_FORM_WALK_START_INDEX))' \
		--eval '(setq vendor-form-standalone-limit $(VENDOR_FORM_WALK_LIMIT))' \
		--eval '(setq vendor-form-standalone-print-every $(VENDOR_FORM_WALK_PRINT_EVERY))' \
		--eval '(setq vendor-form-standalone-normalize-floats $(VENDOR_FORM_WALK_NORMALIZE_FLOATS))' \
		--eval '(setq vendor-form-standalone-repo-root "$(abspath .)")' \
		--eval '(setq standalone-source-normalize-cache-directory "$(VENDOR_SOURCE_CACHE_DIR)")' \
		-l vendor-form-standalone-walk \
		-f vendor-form-standalone-batch

diagnose-vendor-form-walk-fast:
	test -x "$(NELISP_BIN)"
	test -r "$(NEMACS_BOOTSTRAP_BUNDLE)" || { echo "missing $(NEMACS_BOOTSTRAP_BUNDLE); run make build-nelisp-bootstrap once"; exit 1; }
	timeout $(VENDOR_FORM_WALK_TIMEOUT) $(EMACS) -Q -L scripts \
		--eval '(setq vendor-form-standalone-reader "$(abspath $(NELISP_BIN))")' \
		--eval '(setq vendor-form-standalone-bootstrap "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))")' \
		--eval '(setq vendor-form-standalone-prelude "$(VENDOR_FORM_WALK_PRELUDE)")' \
		--eval '(setq vendor-form-standalone-file "$(VENDOR_FORM_WALK_FILE)")' \
		--eval '(setq vendor-form-standalone-preload-files "$(VENDOR_FORM_WALK_PRELOAD_FILES)")' \
		--eval '(setq vendor-form-standalone-start-index $(VENDOR_FORM_WALK_START_INDEX))' \
		--eval '(setq vendor-form-standalone-limit $(VENDOR_FORM_WALK_LIMIT))' \
		--eval '(setq vendor-form-standalone-print-every $(VENDOR_FORM_WALK_PRINT_EVERY))' \
		--eval '(setq vendor-form-standalone-normalize-floats $(VENDOR_FORM_WALK_NORMALIZE_FLOATS))' \
		--eval '(setq vendor-form-standalone-repo-root "$(abspath .)")' \
		--eval '(setq standalone-source-normalize-cache-directory "$(VENDOR_SOURCE_CACHE_DIR)")' \
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
		--eval '(setq standalone-source-normalize-cache-directory "$(VENDOR_SOURCE_CACHE_DIR)")' \
		-l vendor-load-standalone-replay \
		-f vendor-load-standalone-batch

diagnose-vendor-load-replay-fast:
	test -x "$(NELISP_BIN)"
	test -r "$(NEMACS_BOOTSTRAP_BUNDLE)" || { echo "missing $(NEMACS_BOOTSTRAP_BUNDLE); run make build-nelisp-bootstrap once"; exit 1; }
	timeout $(VENDOR_LOAD_TIMEOUT) $(EMACS) -Q -L scripts \
		--eval '(setq vendor-load-standalone-reader "$(abspath $(NELISP_BIN))")' \
		--eval '(setq vendor-load-standalone-prelude "$(VENDOR_LOAD_PRELUDE)")' \
		--eval '(setq vendor-load-standalone-bootstrap "$(abspath $(NEMACS_BOOTSTRAP_BUNDLE))")' \
		--eval '(setq vendor-load-standalone-files "$(VENDOR_FAST_FILES)")' \
		--eval '(setq vendor-load-standalone-proof-form "$(VENDOR_FAST_PROOF_FORM)")' \
		--eval '(setq vendor-load-standalone-repo-root "$(abspath .)")' \
		--eval '(setq standalone-source-normalize-cache-directory "$(VENDOR_SOURCE_CACHE_DIR)")' \
		-l vendor-load-standalone-replay \
		-f vendor-load-standalone-batch

diagnose-vendor-repl-replay: build-nelisp-bootstrap
	test -x "$(NELISP_BIN)"
	timeout $(VENDOR_REPL_TIMEOUT) $(EMACS) -Q -L scripts \
		--eval '(setq vendor-repl-standalone-reader "$(abspath $(NELISP_BIN))")' \
		--eval '(setq vendor-repl-standalone-bootstrap-repl "$(abspath $(NEMACS_BOOTSTRAP_REPL))")' \
		--eval '(setq vendor-repl-standalone-prelude "$(VENDOR_REPL_PRELUDE)")' \
		--eval '(setq vendor-repl-standalone-files "$(VENDOR_REPL_FILES)")' \
		--eval '(setq vendor-repl-standalone-proof-form "$(VENDOR_REPL_PROOF_FORM_ELISP)")' \
		--eval '(setq vendor-repl-standalone-proof-form-file "$(if $(VENDOR_REPL_PROOF_FORM_FILE),$(abspath $(VENDOR_REPL_PROOF_FORM_FILE)),)")' \
		--eval '(setq vendor-repl-standalone-detail-form "$(VENDOR_REPL_DETAIL_FORM_ELISP)")' \
		--eval '(setq vendor-repl-standalone-repo-root "$(abspath .)")' \
		--eval '(setq vendor-repl-standalone-keep-temp $(VENDOR_REPL_KEEP_TEMP))' \
		--eval '(setq vendor-repl-standalone-trace-forms $(VENDOR_REPL_TRACE_FORMS))' \
		--eval '(setq vendor-repl-standalone-direct-character-limit $(VENDOR_REPL_DIRECT_CHARACTER_LIMIT))' \
		--eval '(setq vendor-repl-standalone-coalesce-file-forms $(VENDOR_REPL_COALESCE_FILE_FORMS))' \
		--eval '(setq vendor-repl-standalone-internal-timeout-seconds $(VENDOR_REPL_INTERNAL_TIMEOUT_SECONDS))' \
		--eval '(setq standalone-source-normalize-cache-directory "$(VENDOR_SOURCE_CACHE_DIR)")' \
		-l vendor-repl-standalone-replay \
		-f vendor-repl-standalone-batch

diagnose-vendor-repl-replay-fast:
	test -x "$(NELISP_BIN)"
	test -r "$(NEMACS_BOOTSTRAP_REPL)" || { echo "missing $(NEMACS_BOOTSTRAP_REPL); run make build-nelisp-bootstrap once"; exit 1; }
	timeout $(VENDOR_REPL_TIMEOUT) $(EMACS) -Q -L scripts \
		--eval '(setq vendor-repl-standalone-reader "$(abspath $(NELISP_BIN))")' \
		--eval '(setq vendor-repl-standalone-bootstrap-repl "$(abspath $(NEMACS_BOOTSTRAP_REPL))")' \
		--eval '(setq vendor-repl-standalone-prelude "$(VENDOR_REPL_PRELUDE)")' \
		--eval '(setq vendor-repl-standalone-files "$(VENDOR_FAST_FILES)")' \
		--eval '(setq vendor-repl-standalone-proof-form "$(VENDOR_FAST_PROOF_FORM_ELISP)")' \
		--eval '(setq vendor-repl-standalone-proof-form-file "$(if $(VENDOR_FAST_PROOF_FORM_FILE),$(abspath $(VENDOR_FAST_PROOF_FORM_FILE)),)")' \
		--eval '(setq vendor-repl-standalone-detail-form "$(VENDOR_FAST_DETAIL_FORM_ELISP)")' \
		--eval '(setq vendor-repl-standalone-repo-root "$(abspath .)")' \
		--eval '(setq vendor-repl-standalone-keep-temp $(VENDOR_REPL_KEEP_TEMP))' \
		--eval '(setq vendor-repl-standalone-trace-forms $(VENDOR_REPL_TRACE_FORMS))' \
		--eval '(setq vendor-repl-standalone-direct-character-limit $(VENDOR_REPL_DIRECT_CHARACTER_LIMIT))' \
		--eval '(setq vendor-repl-standalone-coalesce-file-forms $(VENDOR_REPL_COALESCE_FILE_FORMS))' \
		--eval '(setq vendor-repl-standalone-internal-timeout-seconds $(VENDOR_REPL_INTERNAL_TIMEOUT_SECONDS))' \
		--eval '(setq standalone-source-normalize-cache-directory "$(VENDOR_SOURCE_CACHE_DIR)")' \
		-l vendor-repl-standalone-replay \
		-f vendor-repl-standalone-batch

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
	test -r "$(NEMACS_BOOTSTRAP_REPL)"
	ulimit -s "$(NELISP_STACK_LIMIT)" 2>/dev/null || true; \
	timeout $(NELISP_VENDOR_CORE_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		NELISP_ROOT="$(abspath $(NELISP_ROOT))" \
		NELISP_BIN="$(abspath $(NELISP_BIN))" \
		REPO_ROOT="$(abspath .)" \
		NEMACS_BOOTSTRAP_REPL="$(abspath $(NEMACS_BOOTSTRAP_REPL))" \
		VENDOR_CORE_MODULES="$(VENDOR_CORE_MODULES)" \
		VENDOR_CORE_LIMIT="$(VENDOR_CORE_LIMIT)" \
		VENDOR_CORE_STRICT_ELISP="$(VENDOR_CORE_STRICT_ELISP)" \
		./scripts/verify-vendor-core-repl.sh

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
# Pure-Elisp NeLisp checkouts provide target/nelisp.
nelisp:
	bin/build-nelisp

nelisp-rebuild:
	bin/build-nelisp --rebuild

nelisp-clean:
	rm -rf $(VENDOR_NELISP)

elprop:
	bin/elprop-run

clean:
	find . -name "*.elc" -delete
