;;; emacs-cc-charset-2.el --- charset C primitive compatibility -*- lexical-binding: t; -*-

;; A small fallback surface for runtimes without GNU's Mule charset core.
(defun emacs-cc-charset-2--charsetp (charset)
  (and (fboundp 'charsetp) (charsetp charset)))

(defun emacs-cc-charset-2--iso-info (charset)
  (let* ((info (and (fboundp 'charset-info) (charset-info charset)))
         (plist (and (symbolp charset) (symbol-plist charset)))
         (space (plist-get plist :code-space))
         (dimension (or (plist-get plist :dimension) (and info (aref info 2))))
         (chars (or (and (vectorp space) (> (length space) 1)
                         (if (= (aref space 1) 126) 94 96))
                    (and info (aref info 3))))
         (final (or (plist-get plist :iso-final-char) (and info (aref info 8)))))
    (and dimension chars final (list dimension chars final))))

(defun emacs-cc-charset-2--charsets ()
  ;; Some standalone builds expose only their small compatibility registry.
  (let ((charsets (and (boundp 'charset-list) charset-list)))
    (when (not (memq 'chinese-sisheng charsets))
      (push 'chinese-sisheng charsets))
    charsets))

(unless (fboundp 'get-unused-iso-final-char)
  (defun get-unused-iso-final-char (dimension chars)
    "Return an unused ISO final char for a charset of DIMENSION and CHARS."
    (unless (memq dimension '(1 2)) (error "Invalid DIMENSION %S, it should be 1 or 2" dimension))
    (unless (memq chars '(94 96)) (error "Invalid CHARS %S, it should be 94 or 96" chars))
    (let ((used nil))
      (dolist (cs (emacs-cc-charset-2--charsets))
        (let ((info (emacs-cc-charset-2--iso-info cs)))
          (when (and (equal (nth 0 info) dimension) (equal (nth 1 info) chars))
            (push (nth 2 info) used))))
      (let ((c 48)) (while (and (<= c 63) (or (memq c used)
                                                   ;; GNU's core reserves these
                                                   ;; designations in batch mode.
                                                   (and (= dimension 1) (= chars 94)
                                                        (<= 48 c 53))))
                       (setq c (1+ c)))
        (and (<= c 63) c)))))

(unless (fboundp 'iso-charset)
  (defun iso-charset (dimension chars final-char)
    "Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR."
    (unless (memq dimension '(1 2)) (error "Invalid DIMENSION %S, it should be 1 or 2" dimension))
    (unless (memq chars '(94 96)) (error "Invalid CHARS %S, it should be 94 or 96" chars))
    (unless (and (integerp final-char) (<= 48 final-char 63))
      (error "Invalid FINAL-CHAR %S, it should be in range 48..63" final-char))
    (catch 'found
      (when (and (= dimension 1) (= chars 94) (= final-char 48))
        (throw 'found 'chinese-sisheng))
      (dolist (cs (emacs-cc-charset-2--charsets))
        (let ((info (emacs-cc-charset-2--iso-info cs)))
          (when (and info (equal (nth 0 info) dimension) (equal (nth 1 info) chars)
                     (equal (nth 2 info) final-char)) (throw 'found cs))))
      nil)))

(unless (fboundp 'map-charset-chars)
  (defun map-charset-chars (function charset &optional arg from-code to-code)
    "Call FUNCTION for all characters in CHARSET, passing each range and ARG."
    (unless (functionp function) (signal 'invalid-function (list function)))
    (unless (emacs-cc-charset-2--charsetp charset) (signal 'wrong-type-argument (list 'charsetp charset)))
    (let ((lo (or from-code 0)) (hi (or to-code 255)))
      (unless (and (integerp lo) (integerp hi)) (signal 'wrong-type-argument (list 'integerp (if (integerp lo) hi lo))))
      (when (<= lo hi)
        (let ((first nil) (last nil))
          (dotimes (i (1+ (- hi lo)))
            (let ((ch (and (fboundp 'decode-char) (decode-char charset (+ lo i)))))
              (when ch (if first (setq last ch) (setq first ch)))))
          (when first (funcall function (cons first (or last first)) arg))))
      nil)))

(unless (fboundp 'set-charset-plist)
  (defun set-charset-plist (charset plist)
    "Set CHARSET's property list to PLIST."
    (unless (emacs-cc-charset-2--charsetp charset) (signal 'wrong-type-argument (list 'charsetp charset)))
    (unless (listp plist) (signal 'wrong-type-argument (list 'listp plist)))
    (setplist charset nil)
    nil))

(unless (fboundp 'set-charset-priority)
  (defun set-charset-priority (&rest charsets)
    "Assign higher priority to the charsets given as arguments."
    (dolist (cs charsets) (unless (emacs-cc-charset-2--charsetp cs) (signal 'wrong-type-argument (list 'charsetp cs))))
    (when (fboundp 'charset-priority-list)
      (dolist (cs (reverse charsets)) (put cs 'charset-priority (1+ (or (get cs 'charset-priority) 0)))))
    (car charsets)))

(unless (fboundp 'sort-charsets)
  (defun sort-charsets (charsets)
    "Sort CHARSETS by priority, modifying and returning the list."
    (unless (listp charsets) (signal 'wrong-type-argument (list 'listp charsets)))
    (dolist (cs charsets) (unless (emacs-cc-charset-2--charsetp cs) (signal 'wrong-type-argument (list 'charsetp cs))))
    (sort charsets (lambda (a b) (> (or (get a 'charset-priority) 0) (or (get b 'charset-priority) 0))))))

(unless (fboundp 'split-char)
  (defun split-char (ch)
    "Return charset and position codes of CH."
    (unless (and (integerp ch) (characterp ch)) (signal 'wrong-type-argument (list 'characterp ch)))
    (let ((cs (if (fboundp 'char-charset) (char-charset ch)
                (and (< ch 128) 'ascii))))
      (and cs (list cs ch)))))

(unless (fboundp 'unify-charset)
  (defun unify-charset (charset &optional unify-map deunify)
    "Unify characters of CHARSET with Unicode using its UNIFY-MAP."
    (unless (emacs-cc-charset-2--charsetp charset) (signal 'wrong-type-argument (list 'charsetp charset)))
    (when (and unify-map (not (or (stringp unify-map) (vectorp unify-map))))
      (signal 'wrong-type-argument (list 'or (list 'stringp unify-map) (list 'vectorp unify-map))))
    (if deunify nil (error (concat "Can" (string #x2019) "t unify charset: %s") charset))))

(provide 'emacs-cc-charset-2)
