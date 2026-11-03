;;; nelisp-alloc-classes-test.el --- Doc 239 S1 exact size classes (structural)  -*- lexical-binding: t; -*-

;;; Commentary:

;; Structural checks of the large free-list size classes in
;; scripts/nelisp-standalone-build.el.  Nothing here runs a NeLisp image.
;; NELISP_A239_BUILD overrides the build script path (used for the negative
;; control against the pristine seed script).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defconst nelisp-alloc-classes-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory of this test file, captured at load time.")

(defconst nelisp-alloc-classes-test--script
  (or (getenv "NELISP_A239_BUILD")
      (expand-file-name "../scripts/nelisp-standalone-build.el"
                        nelisp-alloc-classes-test--dir))
  "Build script under test, captured at load time.")

(defvar nelisp-alloc-classes-test--text nil)

(defun nelisp-alloc-classes-test--text ()
  (or nelisp-alloc-classes-test--text
      (setq nelisp-alloc-classes-test--text
            (with-temp-buffer
              (insert-file-contents nelisp-alloc-classes-test--script)
              (buffer-string)))))

(defun nelisp-alloc-classes-test--count (regexp)
  (let ((n 0) (pos 0) (s (nelisp-alloc-classes-test--text)))
    (while (string-match regexp s pos)
      (setq n (1+ n) pos (match-end 0)))
    n))

(defun nelisp-alloc-classes-test--defun (name)
  "Return the source text of the first (defun NAME ...) form."
  (let* ((s (nelisp-alloc-classes-test--text))
         (start (string-match (concat "(defun " (regexp-quote name) " ") s)))
    (should start)
    (with-temp-buffer
      (insert (substring s start))
      (goto-char (point-min))
      (let ((form (read (current-buffer))))
        (buffer-substring (point-min) (point))
        (prin1-to-string form)))))

(defun nelisp-alloc-classes-test--class-fn ()
  "Evaluate the class function's arithmetic in Emacs."
  (let ((form (car (read-from-string
                    (nelisp-alloc-classes-test--defun "nl_freelist_large_bin")))))
    (eval `(lambda ,(nth 2 form) ,@(nthcdr 3 form)) t)))

(ert-deftest nelisp-alloc-classes-mapping ()
  (should (= (nelisp-alloc-classes-test--count "(defun nl_freelist_large_bin ") 1))
  (let ((f (nelisp-alloc-classes-test--class-fn)))
    ;; 480 and 488..2048: one class per block total, consecutive.
    (let ((prev nil))
      (cl-loop for bt from 488 to 2048 by 8
               do (let ((c (funcall f bt)))
                    (when prev (should (= c (1+ prev))))
                    (setq prev c))))
    (should (= (funcall f 488) (1+ (funcall f 480))))
    (should (= (funcall f 2048) 196))
    (should (/= (funcall f 504) (funcall f 512)))
    ;; Above 2048 the old coarse bins, renumbered after the new classes.
    (should (= (funcall f 2049) 197))
    (should (= (funcall f 4096) (+ 197 (/ (- 4096 2049) 32))))
    (should (= (funcall f 4097) 261))
    (should (= (funcall f 8192) (+ 261 (/ (- 8192 4097) 64))))
    (should (= (funcall f 8193) 325))
    (should (= (funcall f 32768) (+ 325 (/ (- 32768 8193) 1024))))
    (should (= (funcall f 32769) 349))
    (should (= (funcall f 262144) 349))
    (should (= (funcall f 262145) 350))
    (should (= (funcall f 100000000) 350))))

(ert-deftest nelisp-alloc-classes-producers-use-class-function ()
  ;; Every place that picks a head from a block total goes through the one
  ;; class function; no second copy of the arithmetic exists.
  (should (= (nelisp-alloc-classes-test--count "(defun nl_freelist_large_bin ") 1))
  (should (= (nelisp-alloc-classes-test--count "(- bt 480)") 1))
  (should (>= (nelisp-alloc-classes-test--count
               "(nl_freelist_large_head (nl_freelist_large_bin") 3))
  (should (>= (nelisp-alloc-classes-test--count "(nl_freelist_large_bin ") 10))
  (dolist (fn '("nl_freelist_split_tail" "nl_gc_free_block" "nl_gc_relink_free_one"))
    (should (string-match-p "nl_freelist_large_bin"
                            (nelisp-alloc-classes-test--defun fn))))
  ;; Every head lookup either takes a class from the function or an index.
  (should (string-match-p "nl_freelist_large_bin"
                          (nelisp-alloc-classes-test--defun "nl_freelist_scan_large"))))

(ert-deftest nelisp-alloc-classes-request-skips-want+8 ()
  (let ((scan (nelisp-alloc-classes-test--defun "nl_freelist_scan_large"))
        (first (nelisp-alloc-classes-test--defun "nl_freelist_large_first"))
        (fast (nelisp-alloc-classes-test--defun "nl_alloc_large_fast")))
    ;; Exact class of WANT, then classes at WANT+16 and up; never WANT+8.
    (should (string-match-p "(nl_freelist_large_bin want)" scan))
    (should (string-match-p "(nl_freelist_large_bin (\\+ want 16))" scan))
    (should-not (string-match-p "(\\+ want 8)" scan))
    (should (string-match-p "(nl_freelist_large_bin (\\+ want 16))" first))
    (should-not (string-match-p "(\\+ want 8)" first))
    ;; The inline allocator uses the same first-class rule for its bump test.
    (should (string-match-p "nl_freelist_large_first want" fast))
    (should-not (string-match-p "nl_freelist_large_next bin" fast))))

(ert-deftest nelisp-alloc-classes-bss-and-mask-consistent ()
  (let* ((heads 351) (bytes (* heads 8)))
    ;; The highest class index the class function can return.
    (should (= (1- heads) (funcall (nelisp-alloc-classes-test--class-fn) most-positive-fixnum)))
    ;; BSS: base size sums the head table, the head symbol subtracts it.
    (should (string-match-p (format "64 192 64 40 8 64 %d\n" bytes)
                            (nelisp-alloc-classes-test--text)))
    (should (string-match-p
             (format "(- (nelisp-standalone--driver-bss-base-size) %d)" bytes)
             (nelisp-alloc-classes-test--text)))
    ;; Mask: 51 bits per word, 7 words = 56 bytes, within the 64-byte slot.
    (let ((words (ceiling heads 51)))
      (should (= words 7))
      (should (<= (* words 8) 64))
      (should (string-match-p (format "(< group %d)" words)
                              (nelisp-alloc-classes-test--defun "nl_freelist_large_next")))
      (let ((clr (nelisp-alloc-classes-test--defun "nl_freelist_large_mask_clear_all")))
        (should (= (nelisp-alloc-classes-test--count-in clr "ptr-write-u64") words))
        (should (string-match-p (format "mask) %d 0" (* 8 (1- words))) clr))))
    ;; Every class-range loop uses the new bound; no stale 203/204.
    (should (= (nelisp-alloc-classes-test--count "(> n 350)") 4))
    (should (= (nelisp-alloc-classes-test--count "(> n 203)") 0))
    (should (= (nelisp-alloc-classes-test--count "(< index 351)") 1))
    (should (= (nelisp-alloc-classes-test--count "(< index 204)") 0))))

(defun nelisp-alloc-classes-test--count-in (text needle)
  (let ((n 0) (pos 0))
    (while (string-match (regexp-quote needle) text pos)
      (setq n (1+ n) pos (match-end 0)))
    n))

(provide 'nelisp-alloc-classes-test)
;;; nelisp-alloc-classes-test.el ends here
