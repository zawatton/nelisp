;;; nelisp-pe-write-dll-test.el --- ERT tests for the PE32+ DLL writer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Stage S1: header flags, export directory, base relocations and the
;; static-linker DLL path.  Parses the emitted image directly; the wine
;; end-to-end proof lives in tools/ai/pe-dll/wine-e2e.sh.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (root (and this (expand-file-name ".." (file-name-directory this)))))
  (when root
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (add-to-list 'load-path (expand-file-name "tools/ai/pe-dll" root))))

(require 'nelisp-pe-write)
(require 'nelisp-static-linker)
(require 'pe-dll-sample)

(defun nelisp-pe-dll-test--u (bytes off n)
  (let ((v 0)) (dotimes (i n) (setq v (logior v (ash (aref bytes (+ off i)) (* 8 i))))) v))

(defun nelisp-pe-dll-test--cstr (bytes off)
  (let ((e off)) (while (/= 0 (aref bytes e)) (cl-incf e))
       (substring bytes off e)))

(defun nelisp-pe-dll-test--parse (bytes)
  "Return plist (:opt OFF :sections ((NAME VA VSIZE RAWPTR RAWSIZE)...))."
  (let* ((pe (nelisp-pe-dll-test--u bytes #x3c 4))
         (nsec (nelisp-pe-dll-test--u bytes (+ pe 6) 2))
         (opt (+ pe 24))
         (sec (+ opt (nelisp-pe-dll-test--u bytes (+ pe 20) 2))))
    (list :pe pe :opt opt
          :sections (cl-loop for i below nsec
                             for o = (+ sec (* 40 i))
                             collect (list (nelisp-pe-dll-test--cstr (substring bytes o (+ o 8)) 0)
                                           (nelisp-pe-dll-test--u bytes (+ o 12) 4)
                                           (nelisp-pe-dll-test--u bytes (+ o 8) 4)
                                           (nelisp-pe-dll-test--u bytes (+ o 20) 4)
                                           (nelisp-pe-dll-test--u bytes (+ o 16) 4))))))

(defun nelisp-pe-dll-test--rva->off (parsed rva)
  (cl-loop for s in (plist-get parsed :sections)
           when (and (>= rva (nth 1 s)) (< rva (+ (nth 1 s) (max (nth 2 s) (nth 4 s)))))
           return (+ (nth 3 s) (- rva (nth 1 s)))
           finally (error "RVA %x unmapped" rva)))

(defun nelisp-pe-dll-test--datadir (bytes parsed n)
  (let ((o (+ (plist-get parsed :opt) 112 (* 8 n))))
    (cons (nelisp-pe-dll-test--u bytes o 4) (nelisp-pe-dll-test--u bytes (+ o 4) 4))))

(defun nelisp-pe-dll-test--exports (bytes parsed)
  "Return (DLLNAME BASE ((NAME ORDINAL RVA)...)) in name-table order."
  (let* ((dd (nelisp-pe-dll-test--datadir bytes parsed 0))
         (d (nelisp-pe-dll-test--rva->off parsed (car dd)))
         (u (lambda (o n) (nelisp-pe-dll-test--u bytes (+ d o) n)))
         (nn (funcall u 24 4))
         (eat (nelisp-pe-dll-test--rva->off parsed (funcall u 28 4)))
         (npt (nelisp-pe-dll-test--rva->off parsed (funcall u 32 4)))
         (ord (nelisp-pe-dll-test--rva->off parsed (funcall u 36 4))))
    (list (nelisp-pe-dll-test--cstr bytes (nelisp-pe-dll-test--rva->off parsed (funcall u 12 4)))
          (funcall u 16 4)
          (cl-loop for i below nn
                   for idx = (nelisp-pe-dll-test--u bytes (+ ord (* 2 i)) 2)
                   collect (list (nelisp-pe-dll-test--cstr
                                  bytes (nelisp-pe-dll-test--rva->off
                                         parsed (nelisp-pe-dll-test--u bytes (+ npt (* 4 i)) 4)))
                                 idx
                                 (nelisp-pe-dll-test--u bytes (+ eat (* 4 idx)) 4))))))

(defun nelisp-pe-dll-test--reloc-rvas (bytes parsed)
  (let* ((dd (nelisp-pe-dll-test--datadir bytes parsed 5))
         (o (nelisp-pe-dll-test--rva->off parsed (car dd)))
         (end (+ o (cdr dd))) out)
    (while (< o end)
      (let ((page (nelisp-pe-dll-test--u bytes o 4))
            (size (nelisp-pe-dll-test--u bytes (+ o 4) 4)))
        (should (= 0 (mod size 4)))
        (cl-loop for e from (+ o 8) below (+ o size) by 2
                 for v = (nelisp-pe-dll-test--u bytes e 2)
                 do (pcase (ash v -12)
                      (10 (push (+ page (logand v #xfff)) out))
                      (0 nil)
                      (_ (ert-fail "unexpected reloc type"))))
        (setq o (+ o size))))
    (sort out #'<)))

(defun nelisp-pe-dll-test--build (fn)
  (let* ((dir (make-temp-file "pedll-ert" t))
         (path (expand-file-name "t.dll" dir)))
    (unwind-protect
        (progn (funcall fn path)
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally path)
                 (buffer-string)))
      (delete-directory dir t))))

(defun nelisp-pe-dll-test--sample ()
  (nelisp-pe-dll-test--build #'nelisp-pe-dll-sample-build-dll))

(ert-deftest nelisp-pe-write-dll-header-flags ()
  (let* ((b (nelisp-pe-dll-test--sample))
         (p (nelisp-pe-dll-test--parse b))
         (pe (plist-get p :pe)) (opt (plist-get p :opt)))
    (should (equal (substring b pe (+ pe 4)) "PE\0\0"))
    (let ((ch (nelisp-pe-dll-test--u b (+ pe 22) 2)))
      (should (/= 0 (logand ch #x2000)))   ; IMAGE_FILE_DLL
      (should (/= 0 (logand ch #x0002)))   ; EXECUTABLE_IMAGE
      (should (= 0 (logand ch #x0001))))   ; relocs not stripped
    (should (= #x20b (nelisp-pe-dll-test--u b opt 2)))
    (should (= #x180000000 (nelisp-pe-dll-test--u b (+ opt 24) 8)))
    (should (= #x160 (nelisp-pe-dll-test--u b (+ opt 70) 2))) ; HEVA|DYN|NX
    (should (equal (mapcar #'car (plist-get p :sections))
                   '(".text" ".data" ".edata" ".reloc")))))

(ert-deftest nelisp-pe-write-dll-export-directory ()
  (let* ((b (nelisp-pe-dll-test--sample))
         (p (nelisp-pe-dll-test--parse b))
         (ex (nelisp-pe-dll-test--exports b p))
         (names (mapcar #'car (nth 2 ex))))
    (should (equal (nth 0 ex) "pedllsample.dll"))
    (should (= 1 (nth 1 ex)))
    (should (equal names (sort (copy-sequence names) #'string<)))
    ;; Spec order via_ptr, add2, answer => ordinal indexes 0, 1, 2.
    (should (equal (nth 2 ex)
                   '(("add2" 1 #x1000) ("answer" 2 #x1010) ("via_ptr" 0 #x1020))))))

(ert-deftest nelisp-pe-write-dll-sample-abs64-reloc-coverage ()
  "Every abs64 site of the linked sample appears as a DIR64 fixup."
  (let* ((b (nelisp-pe-dll-test--sample))
         (p (nelisp-pe-dll-test--parse b))
         (data (assoc ".data" (plist-get p :sections)))
         (fix (nelisp-pe-dll-test--reloc-rvas b p)))
    (should (equal fix (list (nth 1 data))))
    ;; The slot holds the absolute VA of `answer' at the preferred base.
    (should (= (+ #x180000000 #x1010)
               (nelisp-pe-dll-test--u b (nth 3 data) 8)))))

(ert-deftest nelisp-pe-write-dll-reloc-blocks-multi-page ()
  (let* ((rvas '(#x1008 #x1ff8 #x2000 #x2010 #x2018 #x5000 #x1008)) ; dup + odd count
         (b (nelisp-pe-dll-test--build
             (lambda (path)
               (nelisp-pe-write-dll-binary
                path (list :text (unibyte-string #xc3) :abs64-relocs rvas))))))
    (should (equal (nelisp-pe-dll-test--reloc-rvas b (nelisp-pe-dll-test--parse b))
                   (sort (delete-dups (copy-sequence rvas)) #'<)))))

(ert-deftest nelisp-pe-write-dll-default-dllmain-and-imports ()
  (let* ((b (nelisp-pe-dll-test--build
             (lambda (path)
               (nelisp-pe-write-dll-binary
                path (list :text (unibyte-string #xc3)
                           :imports '("ExitProcess")
                           :exports '(("f" . 0)))))))
         (p (nelisp-pe-dll-test--parse b))
         (entry (nelisp-pe-dll-test--u b (+ (plist-get p :opt) 16) 4))
         (eo (nelisp-pe-dll-test--rva->off p entry))
         (imp (nelisp-pe-dll-test--datadir b p 1)))
    (should (equal (substring b eo (+ eo 6)) (unibyte-string #xb8 1 0 0 0 #xc3)))
    (should (> (car imp) 0))
    ;; No abs64 relocs => no .reloc section.
    (should-not (assoc ".reloc" (plist-get p :sections)))
    (should (equal (mapcar #'car (nth 2 (nelisp-pe-dll-test--exports b p))) '("f")))))

(ert-deftest nelisp-pe-write-dll-no-reloc-hook-and-exe-unchanged ()
  (let ((b (nelisp-pe-dll-test--build
            (lambda (path)
              (nelisp-link-units-pe32-dll
               path (list (nelisp-pe-dll-sample--unit)) '("add2") nil
               (list :no-reloc t))))))
    (should (= 0 (car (nelisp-pe-dll-test--datadir b (nelisp-pe-dll-test--parse b) 5)))))
  ;; EXE output keeps IMAGE_FILE_DLL clear and DllCharacteristics = NX only.
  (let* ((f (make-temp-file "pedll-exe")) b)
    (unwind-protect
        (progn (nelisp-pe-write-exe-binary f 'minimal-exit-42)
               (with-temp-buffer (set-buffer-multibyte nil)
                                 (insert-file-contents-literally f)
                                 (setq b (buffer-string))))
      (delete-file f))
    (let ((p (nelisp-pe-dll-test--parse b)))
      (should (= 0 (logand #x2000 (nelisp-pe-dll-test--u b (+ (plist-get p :pe) 22) 2))))
      (should (= #x100 (nelisp-pe-dll-test--u b (+ (plist-get p :opt) 70) 2))))))

(provide 'nelisp-pe-write-dll-test)
;;; nelisp-pe-write-dll-test.el ends here
