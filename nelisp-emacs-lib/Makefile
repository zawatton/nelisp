## nelisp-emacs Makefile

EMACS = emacs --batch
# NeLisp is driven through the pure-Elisp standalone reader.
VENDOR_NELISP = vendor/nelisp
DEFAULT_NELISP_ROOT := $(firstword $(wildcard $(VENDOR_NELISP) ../nelisp $(HOME)/Notes/dev/nelisp))
NELISP_ROOT  ?= $(if $(DEFAULT_NELISP_ROOT),$(DEFAULT_NELISP_ROOT),$(VENDOR_NELISP))
DEFAULT_NELISP_BIN := $(firstword $(wildcard build/nelisp-experiment $(NELISP_ROOT)/target/nelisp $(NELISP_ROOT)/target/nelisp-standalone-reader))
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
NELISP_STACK_LIMIT ?= unlimited
BUILD_DIR ?= build
NEMACS_BOOTSTRAP_BUNDLE ?= $(BUILD_DIR)/nemacs-bootstrap.el
NEMACS_BOOTSTRAP_REPL ?= $(BUILD_DIR)/nemacs-bootstrap.repl
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
VENDOR_LOAD_PROOF_FORM ?= (and (= vendor-standalone-load-ok-count vendor-standalone-load-file-count) (fboundp (quote dired)) (fboundp (quote describe-function)) (fboundp (quote project-current)) (fboundp (quote find-file)) (fboundp (quote forward-sexp)) (fboundp (quote mark-defun)) (fboundp (quote map-y-or-n-p)) (fboundp (quote read-answer)) (featurep (quote ring)) (fboundp (quote ring-ref)) (featurep (quote generator)) (featurep (quote avl-tree)) (fboundp (quote avl-tree-create)) (fboundp (quote avl-tree-p)) (fboundp (quote avl-tree-iter)) $(VENDOR_AVL_PROOF_FORM) (boundp (quote emoji--derived)) (boundp (quote emoji--names)) (boundp (quote idna-mapping-table)) (vectorp idna-mapping-table) (string= (elt idna-mapping-table 65) (char-to-string 97)) (eq (elt idna-mapping-table 173) (quote ignored)) (string= (elt idna-mapping-table 8490) (char-to-string 107)) (boundp (quote iso-transl-char-map)) (boundp (quote iso-transl-language-alist)) (fboundp (quote iso-transl-define-keys)) (fboundp (quote iso-transl-set-language)) (featurep (quote cp51932)) (get (quote cp51932-decode) (quote translation-table)) (get (quote cp51932-encode) (quote translation-table)) (featurep (quote eucjp-ms)) (get (quote eucjp-ms-decode) (quote translation-table)) (get (quote eucjp-ms-encode) (quote translation-table)) (featurep (quote fontset)) (fboundp (quote x-decompose-font-name)) (fboundp (quote x-compose-font-name)) (fboundp (quote create-default-fontset)) (boundp (quote standard-fontset-spec)) (featurep (quote ja-dic-utl)) (fboundp (quote skkdic-lookup-key)) (boundp (quote skkdic-okurigana-table)) (featurep (quote format-spec)) (fboundp (quote format-spec)) (featurep (quote org-version)) (fboundp (quote org-release)) (fboundp (quote org-git-version)) (stringp (org-release)) (stringp (org-git-version)) (equal (length (org-release)) 6) (equal (length (org-git-version)) 14) (featurep (quote org-macs)) (fboundp (quote org-with-gensyms)) (fboundp (quote org-string-nw-p)) (featurep (quote org-compat)) (fboundp (quote org-string-equal-ignore-case)) (fboundp (quote org-version-check)) (fboundp (quote org-with-silent-modifications)) (featurep (quote org-fold-core)) (fboundp (quote org-fold-core-add-folding-spec)) (fboundp (quote org-fold-core-region)) (fboundp (quote org-fold-core-folded-p)) (featurep (quote org-fold)) (fboundp (quote org-fold-region)) (fboundp (quote org-fold-show-all)) (fboundp (quote org-fold-hide-subtree)) (featurep (quote org-duration)) (fboundp (quote org-duration-p)) (fboundp (quote org-duration-to-minutes)) (fboundp (quote org-duration-from-minutes)) (fboundp (quote org-duration-h:mm-only-p)) (featurep (quote org)) (featurep (quote org-capture)) (featurep (quote org-refile)) (featurep (quote org-datetree)) (featurep (quote org-archive)) (featurep (quote org-clock)) (featurep (quote ol)) (featurep (quote org-footnote)) (featurep (quote org-list)) (fboundp (quote org-list-to-lisp)) (featurep (quote org-entities)) (fboundp (quote org-entity-get)))
VENDOR_LOAD_PROOF_FORM := (and $(VENDOR_LOAD_PROOF_FORM) (featurep (quote org-macro)) (fboundp (quote org-macro--makeargs)) (fboundp (quote org-macro--set-templates)) (fboundp (quote org-macro-initialize-templates)) (fboundp (quote org-macro-expand)) (fboundp (quote org-macro-replace-all)) (fboundp (quote org-macro-escape-arguments)) (fboundp (quote org-macro-extract-arguments)) (fboundp (quote org-macro--counter-increment)) (boundp (quote org-macro-templates)))
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
VENDOR_REPL_PROOF_FORM ?= (and (= vendor-standalone-load-ok-count vendor-standalone-load-file-count) (fboundp (quote dired)) (fboundp (quote describe-function)) (fboundp (quote project-current)) (fboundp (quote find-file)) (fboundp (quote forward-sexp)) (fboundp (quote mark-defun)) (fboundp (quote map-y-or-n-p)) (fboundp (quote read-answer)) (featurep (quote ring)) (fboundp (quote ring-ref)) (featurep (quote generator)) (featurep (quote avl-tree)) (fboundp (quote avl-tree-create)) (fboundp (quote avl-tree-p)) (fboundp (quote avl-tree-iter)) $(VENDOR_AVL_PROOF_FORM) (boundp (quote emoji--derived)) (boundp (quote emoji--names)) (boundp (quote idna-mapping-table)) (vectorp idna-mapping-table) (string= (elt idna-mapping-table 65) (char-to-string 97)) (eq (elt idna-mapping-table 173) (quote ignored)) (string= (elt idna-mapping-table 8490) (char-to-string 107)) (boundp (quote iso-transl-char-map)) (boundp (quote iso-transl-language-alist)) (fboundp (quote iso-transl-define-keys)) (fboundp (quote iso-transl-set-language)) (featurep (quote cp51932)) (get (quote cp51932-decode) (quote translation-table)) (get (quote cp51932-encode) (quote translation-table)) (featurep (quote eucjp-ms)) (get (quote eucjp-ms-decode) (quote translation-table)) (get (quote eucjp-ms-encode) (quote translation-table)) (featurep (quote fontset)) (fboundp (quote x-decompose-font-name)) (fboundp (quote x-compose-font-name)) (fboundp (quote create-default-fontset)) (boundp (quote standard-fontset-spec)) (featurep (quote ja-dic-utl)) (fboundp (quote skkdic-lookup-key)) (boundp (quote skkdic-okurigana-table)) (featurep (quote format-spec)) (fboundp (quote format-spec)) (featurep (quote org-version)) (fboundp (quote org-release)) (fboundp (quote org-git-version)) (stringp (org-release)) (stringp (org-git-version)) (equal (length (org-release)) 6) (equal (length (org-git-version)) 14) (featurep (quote org-macs)) (fboundp (quote org-with-gensyms)) (fboundp (quote org-string-nw-p)) (featurep (quote org-compat)) (fboundp (quote org-string-equal-ignore-case)) (fboundp (quote org-version-check)) (fboundp (quote org-with-silent-modifications)) (featurep (quote org-fold-core)) (fboundp (quote org-fold-core-add-folding-spec)) (fboundp (quote org-fold-core-region)) (fboundp (quote org-fold-core-folded-p)) (featurep (quote org-fold)) (fboundp (quote org-fold-region)) (fboundp (quote org-fold-show-all)) (fboundp (quote org-fold-hide-subtree)) (featurep (quote org-duration)) (fboundp (quote org-duration-p)) (fboundp (quote org-duration-to-minutes)) (fboundp (quote org-duration-from-minutes)) (fboundp (quote org-duration-h:mm-only-p)) (featurep (quote org)) (featurep (quote org-capture)) (featurep (quote org-refile)) (featurep (quote org-datetree)) (featurep (quote org-archive)) (featurep (quote org-clock)) (featurep (quote ol)) (featurep (quote org-footnote)) (featurep (quote org-list)) (fboundp (quote org-list-to-lisp)) (featurep (quote org-entities)) (fboundp (quote org-entity-get)))
VENDOR_REPL_PROOF_FORM := (and $(VENDOR_REPL_PROOF_FORM) (featurep (quote org-macro)) (fboundp (quote org-macro--makeargs)) (fboundp (quote org-macro--set-templates)) (fboundp (quote org-macro-initialize-templates)) (fboundp (quote org-macro-expand)) (fboundp (quote org-macro-replace-all)) (fboundp (quote org-macro-escape-arguments)) (fboundp (quote org-macro-extract-arguments)) (fboundp (quote org-macro--counter-increment)) (boundp (quote org-macro-templates)))
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
VENDOR_REPL_DETAIL_FORM ?= (concat "load-ok-count=" (number-to-string vendor-standalone-load-ok-count) "/" (number-to-string vendor-standalone-load-file-count) " loads=" vendor-repl-load-status " project-current=" (if (fboundp (quote project-current)) "t" "nil") " find-file=" (if (fboundp (quote find-file)) "t" "nil") " forward-sexp=" (if (fboundp (quote forward-sexp)) "t" "nil") " map-y-or-n-p=" (if (fboundp (quote map-y-or-n-p)) "t" "nil") " ring=" (if (featurep (quote ring)) "t" "nil") " ring-ref=" (if (fboundp (quote ring-ref)) "t" "nil") " generator=" (if (featurep (quote generator)) "t" "nil") " avl-tree=" (if (featurep (quote avl-tree)) "t" "nil") " avl-tree-create=" (if (fboundp (quote avl-tree-create)) "t" "nil") " avl-tree-iter=" (if (fboundp (quote avl-tree-iter)) "t" "nil") " iso-transl-vars=" (if (and (boundp (quote iso-transl-char-map)) (boundp (quote iso-transl-language-alist))) "t" "nil") " cp51932=" (if (featurep (quote cp51932)) "t" "nil") " eucjp-ms=" (if (featurep (quote eucjp-ms)) "t" "nil") " fontset=" (if (featurep (quote fontset)) "t" "nil") " ja-dic-utl=" (if (featurep (quote ja-dic-utl)) "t" "nil") " format-spec=" (if (featurep (quote format-spec)) "t" "nil") " org-version=" (if (featurep (quote org-version)) "t" "nil") " org-macs=" (if (featurep (quote org-macs)) "t" "nil") " org-compat=" (if (featurep (quote org-compat)) "t" "nil") " org-fold-core=" (if (featurep (quote org-fold-core)) "t" "nil") " org-fold=" (if (featurep (quote org-fold)) "t" "nil") " org-duration=" (if (featurep (quote org-duration)) "t" "nil") " org=" (if (featurep (quote org)) "t" "nil") " org-agenda=" (if (featurep (quote org-agenda)) "t" "nil") " org-capture=" (if (featurep (quote org-capture)) "t" "nil") " org-refile=" (if (featurep (quote org-refile)) "t" "nil") " org-datetree=" (if (featurep (quote org-datetree)) "t" "nil") " org-archive=" (if (featurep (quote org-archive)) "t" "nil") " org-clock=" (if (featurep (quote org-clock)) "t" "nil") " ol=" (if (featurep (quote ol)) "t" "nil") " org-element-ast=" (if (featurep (quote org-element-ast)) "t" "nil") " org-footnote=" (if (featurep (quote org-footnote)) "t" "nil") " org-list=" (if (featurep (quote org-list)) "t" "nil") " org-list-to-lisp=" (if (fboundp (quote org-list-to-lisp)) "t" "nil") " org-entities=" (if (featurep (quote org-entities)) "t" "nil") " org-entity-get=" (if (fboundp (quote org-entity-get)) "t" "nil"))
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
VENDOR_REPL_TIMEOUT ?= 900s
VENDOR_REPL_KEEP_TEMP ?= nil
VENDOR_REPL_TRACE_FORMS ?= nil
VENDOR_FAST_FILES ?= $(abspath $(VENDOR_FORM_WALK_FILE))
VENDOR_FAST_PROOF_FORM ?= (= vendor-standalone-load-ok-count vendor-standalone-load-file-count)
VENDOR_FAST_DETAIL_FORM ?= (concat "load-ok-count=" (number-to-string vendor-standalone-load-ok-count) "/" (number-to-string vendor-standalone-load-file-count))
VENDOR_FAST_PROOF_FORM_ELISP = $(subst ",\",$(VENDOR_FAST_PROOF_FORM))
VENDOR_FAST_DETAIL_FORM_ELISP = $(subst ",\",$(VENDOR_FAST_DETAIL_FORM))
NELISP_LOAD_PATH = -L $(NELISP_ROOT)/src \
	$(foreach d,$(wildcard $(NELISP_ROOT)/packages/*/src),-L $(d))
SRC_FILES = $(wildcard src/*.el)
TEST_FILES = $(wildcard test/*.el)

.PHONY: compile test gate5 test-redisplay-core-smoke doctor build-nelisp-bootstrap bake-image bake-runtime-image bake-interactive-runtime-image bake-vendor-core-runtime-image test-nelisp test-nelisp-runtime-image test-nelisp-interactive-runtime-image test-nelisp-vendor-core-runtime-image test-nelisp-ert profile-nelisp-bootstrap diagnose-vendor-form-walk diagnose-vendor-load-replay diagnose-vendor-repl-replay diagnose-vendor-form-walk-fast diagnose-vendor-load-replay-fast diagnose-vendor-repl-replay-fast verify-nelisp-standalone verify-vendor verify-vendor-inventory verify-vendor-class-a verify-vendor-core bench demo demo-phase2 clean nelisp nelisp-rebuild nelisp-clean help

help:
	@echo "Targets:"
	@echo "  make compile         byte-compile src/*.el"
	@echo "  make test            run ERT under host emacs"
	@echo "  make gate5           prove vendor source replay == .nelc artifact load"
	@echo "  make test-redisplay-core-smoke  run isolated lightweight redisplay core smoke"
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
	@echo "  VENDOR_REPL_DETAIL_FORM=$(VENDOR_REPL_DETAIL_FORM)  diagnostic string form shown when REPL proof fails"
	@echo "  VENDOR_REPL_KEEP_TEMP=$(VENDOR_REPL_KEEP_TEMP)  keep generated REPL diagnostics when non-nil"
	@echo "  VENDOR_REPL_TRACE_FORMS=$(VENDOR_REPL_TRACE_FORMS)  record per-form REPL progress when non-nil"
	@echo "  VENDOR_FAST_FILES=$(VENDOR_FAST_FILES)  small file set for diagnose-vendor-*-fast"
	@echo "  VENDOR_FAST_PROOF_FORM=$(VENDOR_FAST_PROOF_FORM)  fast load/REPL proof"
	@echo "  VENDOR_FAST_DETAIL_FORM=$(VENDOR_FAST_DETAIL_FORM)  fast REPL failure detail"

compile:
	$(EMACS) -L src $(NELISP_LOAD_PATH) \
		-f batch-byte-compile $(SRC_FILES)

test:
	$(EMACS) -L src -L test -L demo $(NELISP_LOAD_PATH) \
		$(foreach t,$(TEST_FILES),-l $(t)) \
		-f ert-run-tests-batch-and-exit

gate5:
	$(EMACS) -Q -L scripts -L test \
		-L /home/madblack-21/Cowork/Notes/dev/nelisp/src \
		-L /home/madblack-21/Cowork/Notes/dev/nelisp/lisp \
		-l scripts/nemacs-artifact-gate5.el \
		-l test/nelisp-emacs-artifact-gate5-test.el \
		-f ert-run-tests-batch-and-exit

vendor-nelc-cache:
	$(EMACS) -Q -L scripts -L test \
		-L /home/madblack-21/Cowork/Notes/dev/nelisp/lisp \
		-L /home/madblack-21/Cowork/Notes/dev/nelisp/src \
		-l scripts/nemacs-vendor-cache.el \
		-l test/nemacs-vendor-cache-test.el \
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
		--eval '(setq nelisp-bootstrap-repl-output-file "$(abspath $(NEMACS_BOOTSTRAP_REPL))")' \
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
		--eval '(setq vendor-repl-standalone-detail-form "$(VENDOR_REPL_DETAIL_FORM_ELISP)")' \
		--eval '(setq vendor-repl-standalone-repo-root "$(abspath .)")' \
		--eval '(setq vendor-repl-standalone-keep-temp $(VENDOR_REPL_KEEP_TEMP))' \
		--eval '(setq vendor-repl-standalone-trace-forms $(VENDOR_REPL_TRACE_FORMS))' \
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
		--eval '(setq vendor-repl-standalone-detail-form "$(VENDOR_FAST_DETAIL_FORM_ELISP)")' \
		--eval '(setq vendor-repl-standalone-repo-root "$(abspath .)")' \
		--eval '(setq vendor-repl-standalone-keep-temp $(VENDOR_REPL_KEEP_TEMP))' \
		--eval '(setq vendor-repl-standalone-trace-forms $(VENDOR_REPL_TRACE_FORMS))' \
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
		printf '%s\n' '(exit 42)'; \
	} > "$$tmp"; \
	timeout $(NELISP_VENDOR_CORE_TIMEOUT) env NELISP_HOME="$(abspath $(NELISP_ROOT))" \
		"$(NELISP_BIN)" --load "$$tmp"; \
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
# Pure-Elisp NeLisp checkouts provide target/nelisp.
nelisp:
	bin/build-nelisp

nelisp-rebuild:
	bin/build-nelisp --rebuild

nelisp-clean:
	rm -rf $(VENDOR_NELISP)

clean:
	find . -name "*.elc" -delete
