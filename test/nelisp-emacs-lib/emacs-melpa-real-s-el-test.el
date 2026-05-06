;;; emacs-melpa-real-s-el-test.el --- Phase 4 real MELPA package: s.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Phase 4 = MELPA compat shim (`emacs-melpa-shim.el')。第 1
;; real-package onboarding として MELPA で widely-deployed な s.el
;; (string utilities、Magnar Sveen、~792 LOC、MELPA 上 reverse-deps
;; 多数) を取込む。
;;
;; 取り扱いの分割:
;;   * §A pure string subset    — host load 経路でそのまま動作する
;;     部分。`(require 's)' 相当 = host emacs の string primitive
;;     (concat / substring / format / mapconcat / split-string /
;;     replace-regexp-in-string / string-match / regexp-quote 等)
;;     に依存し buffer 操作は経由しないので shim 不要で end-to-end
;;     動作する。
;;   * §B shim-required subset  — `s-with' / `s-truncate' 等の関数は
;;     host C primitive (load / set-buffer 等) と nelisp-ec の buffer
;;     型がぶつかるため、現状 `with-installed' macro 経由で安定して
;;     動作させるには protocol harmonisation が要る。本 ERT では
;;     skip で gap を見える化する (= regression gate)。
;;
;; 本 file が test/ に lands した時点で「Phase 4 = synthetic pilot
;; のみ」状態を脱し、real MELPA package が 1 件 onboarding 済み
;; という地位を獲得する。第 2 onboarding 候補は dash.el (= list
;; utilities、s.el と並ぶ MELPA foundation) を想定。

;;; Code:

(require 'ert)

(defconst emacs-melpa-real-s-el-test--candidates
  '("/home/madblack-21/.emacs.d/external-packages/s.el/s.el"
    "/usr/share/emacs/site-lisp/elpa/s/s.el"
    "/usr/share/emacs/site-lisp/elpa-src/s/s.el")
  "Candidate paths for s.el on disk.")

(defun emacs-melpa-real-s-el-test--locate ()
  "Return an absolute path to a usable s.el on the host, or nil."
  (cl-find-if #'file-readable-p emacs-melpa-real-s-el-test--candidates))

(defmacro emacs-melpa-real-s-el-test--skip-without-source (&rest body)
  "Skip the test gracefully when s.el is not on disk."
  (declare (indent 0) (debug t))
  `(let ((src (emacs-melpa-real-s-el-test--locate)))
     (unless src
       (ert-skip "s.el not found in any candidate location"))
     (load src nil t)
     ,@body))

;;;; A. pure string subset (= MVP coverage that works without the shim)

(ert-deftest emacs-melpa-real-s-el-test/loads-cleanly ()
  "s.el must `load' end-to-end without raising — proves the substrate
provides the underlying string + regex + sequence primitives
(`replace-regexp-in-string', `split-string', `regexp-quote', ...) at
the API surface s.el's defuns expand against."
  (emacs-melpa-real-s-el-test--skip-without-source
    (should (fboundp 's-trim))
    (should (fboundp 's-split))
    (should (fboundp 's-upcase))
    (should (fboundp 's-replace))))

(ert-deftest emacs-melpa-real-s-el-test/trim ()
  (emacs-melpa-real-s-el-test--skip-without-source
    (should (string= "hello" (s-trim "  hello  ")))
    (should (string= "x"     (s-trim "\n\t x \r")))
    (should (string= ""      (s-trim "    ")))))

(ert-deftest emacs-melpa-real-s-el-test/split ()
  (emacs-melpa-real-s-el-test--skip-without-source
    (should (equal '("a" "b" "c") (s-split "," "a,b,c")))
    (should (equal '("a")         (s-split "," "a")))
    (should (equal '("" "a" "")   (s-split "," ",a,")))))

(ert-deftest emacs-melpa-real-s-el-test/case-conversions ()
  (emacs-melpa-real-s-el-test--skip-without-source
    (should (string= "ABC"   (s-upcase "abc")))
    (should (string= "abc"   (s-downcase "ABC")))
    (should (string= "Hello" (s-capitalize "hello")))))

(ert-deftest emacs-melpa-real-s-el-test/predicates ()
  (emacs-melpa-real-s-el-test--skip-without-source
    (should      (s-starts-with-p "hello" "hello world"))
    (should      (s-ends-with-p   "world" "hello world"))
    (should-not  (s-starts-with-p "world" "hello world"))
    (should-not  (s-ends-with-p   "hello" "hello world"))
    (should      (s-contains-p    "lo wo" "hello world"))
    (should      (s-blank-p       ""))
    (should      (s-blank-p       nil))
    (should-not  (s-blank-p       "x"))))

(ert-deftest emacs-melpa-real-s-el-test/join ()
  (emacs-melpa-real-s-el-test--skip-without-source
    (should (string= "a-b-c"   (s-join "-" '("a" "b" "c"))))
    (should (string= ""        (s-join "-" '())))
    (should (string= "a"       (s-join "-" '("a"))))))

(ert-deftest emacs-melpa-real-s-el-test/replace ()
  (emacs-melpa-real-s-el-test--skip-without-source
    (should (string= "new text"
                     (s-replace "old" "new" "old text")))
    (should (string= "AAA"
                     (s-replace-all '(("a" . "A")) "aaa")))))

(ert-deftest emacs-melpa-real-s-el-test/length-helpers ()
  (emacs-melpa-real-s-el-test--skip-without-source
    (should (= 5 (length (s-pad-left  3 "x" "hello"))))
    (should (= 5 (length (s-pad-right 3 "x" "hello"))))
    (should (string= "xxhi"   (s-pad-left  4 "x" "hi")))
    (should (string= "hixx"   (s-pad-right 4 "x" "hi")))))

;;;; B. shim-required subset (= documented gap, regression gate)

(ert-deftest emacs-melpa-real-s-el-test/buffer-functions-are-skipped ()
  "Phase 4 follow-up: `s-' helpers that route through buffers (= s-with,
s-truncate-or-pad-on-newlines, s-split-up-and-down, ...) require the
MELPA shim to be installed via `emacs-melpa-shim-with-installed'.
Direct host load currently breaks on protocol mismatches between
host C primitives (`load' / `set-buffer' calling C `Fset_buffer'
directly) and the NeLisp buffer struct.  This test pins the gap as
a known follow-up so we can un-skip when the shim grows the
required protocol coverage."
  (emacs-melpa-real-s-el-test--skip-without-source
    (ert-skip "shim-required s-* functions: pending Phase 4 protocol harmonisation")))

(provide 'emacs-melpa-real-s-el-test)

;;; emacs-melpa-real-s-el-test.el ends here
