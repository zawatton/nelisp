;;; calc-ext.el --- Tier 3 Calc extension facade  -*- lexical-binding: t; -*-

;;; Commentary:

;; `calc.el' in this tree deliberately provides a small calculator surface.
;; The GNU Calc `calc-ext.el' assumes that the full Calc implementation has
;; already initialized hundreds of variables, so loading that vendor file
;; after the facade is not a compatible fallback.  Keep the companion feature
;; loadable for packages such as evil-collection and install the bindings that
;; the local scalar calculator can actually execute.

;;; Code:

(require 'calc)
(require 'emacs-tier3-facades)

;; GNU Calc creates this map while loading `calc'.  The standalone calculator
;; creates it during installation; create it here as well for host facade
;; users that only require `calc' and `calc-ext'.
(defvar calc-mode-map nil
  "The keymap exposed by the Calc facade.")
(unless (keymapp calc-mode-map)
  (setq calc-mode-map (make-sparse-keymap)))

(defconst calc-ext--scalar-bindings
  '(("+" . calc-plus)
    ("-" . calc-minus)
    ("*" . calc-times)
    ("/" . calc-divide))
  "Bindings implemented by the local scalar calculator.")

(defun calc-shift-prefix (&optional _argument)
  "Signal that GNU Calc letter-prefix mode is outside this facade."
  (interactive "P")
  (emacs-tier3-facades--unsupported 'calc 'calc-shift-prefix))

(defun calc-init-prefixes ()
  "Install the prefix-independent bindings supported by the facade."
  (dolist (binding calc-ext--scalar-bindings)
    (define-key calc-mode-map (car binding) (cdr binding)))
  nil)

(defun calc-init-extensions ()
  "Initialize the extension bindings supported by the facade."
  (calc-init-prefixes)
  (define-key calc-mode-map "mS" #'calc-shift-prefix)
  nil)

;; A few small packages require `calc-ext' only for its integer helpers.  Keep
;; that useful compatibility slice available without importing the full GNU
;; Calc object system.  These definitions intentionally cover scalar integer
;; inputs; non-scalar Calc expressions remain outside this facade.
(unless (fboundp 'math-read-radix)
  (defun math-read-radix (string radix)
    "Read an integer STRING in RADIX, or return nil for invalid input."
    (let ((text (upcase (or string "")))
          (index 0)
          (value 0)
          digit)
      (while (and (< index (length text))
                  (setq digit
                        (let ((char (aref text index)))
                          (cond ((and (>= char ?0) (<= char ?9))
                                 (- char ?0))
                                ((and (>= char ?A) (<= char ?Z))
                                 (+ 10 (- char ?A))))))
                  (< digit radix))
        (setq value (+ (* value radix) digit)
              index (1+ index)))
      (and (= index (length text)) value))))

(unless (fboundp 'math-add)
  (defun math-add (left right)
    "Add scalar integer LEFT and RIGHT."
    (+ left right)))

(unless (fboundp 'math-mul)
  (defun math-mul (left right)
    "Multiply scalar integer LEFT and RIGHT."
    (* left right)))

(unless (fboundp 'math-power-of-2)
  (defun math-power-of-2 (power)
    "Return 2 raised to the non-negative integer POWER."
    (if (and (integerp power) (>= power 0))
        (ash 1 power)
      (error "Argument must be a natural number"))))

(unless (fboundp 'math-clip)
  (defun math-clip (integer width)
    "Return the low WIDTH bits of scalar integer INTEGER."
    (if (and (integerp integer) (integerp width) (>= width 0))
        (if (= width 0)
            integer
          (logand integer (1- (ash 1 width))))
      (error "Arguments must be integers"))))

(unless (fboundp 'math-idivmod)
  (defun math-idivmod (dividend divisor)
    "Return the integer quotient and remainder of DIVIDEND and DIVISOR."
    (if (= divisor 0)
        (error "Division by zero")
      (cons (/ dividend divisor) (% dividend divisor)))))

(unless (fboundp 'math-fixnum)
  (defun math-fixnum (integer)
    "Return scalar INTEGER unchanged."
    (if (integerp integer)
        integer
      (error "%s is not a supported number format" integer))))

(unless (fboundp 'calcFunc-random)
  (defun calcFunc-random (limit)
    "Return a random scalar integer below LIMIT."
    (if (and (integerp limit) (> limit 0))
        (random limit)
      (error "Argument must be a positive integer"))))

;; Vendor `calc-ext' performs this setup as a load-time side effect, and
;; evil-collection relies on that contract before it adds its own bindings.
(calc-init-extensions)

(provide 'calc-ext)

;;; calc-ext.el ends here
