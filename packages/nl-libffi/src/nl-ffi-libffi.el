;;; nl-ffi-libffi.el --- Callback-free SysV libffi adapter -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Independent of any display/editor. Native objects are created only at run time.
(require 'nl-ffi)
(require 'nl-ffi-memory)

(defvar nl-ffi-libffi--cache nil)
(defvar nl-ffi-libffi--types nil)

(defun nl-ffi-libffi-scalar (name signature &rest values)
  "Invoke the existing scalar provider with NAME, SIGNATURE and VALUES."
  (apply #'nl-ffi-compat-call nil name signature values))

(defun nl-ffi-libffi-u32 (p offset)
  "Read a little-endian u32 without tagged u64 arithmetic."
  (+ (ptr-read-u8 p offset) (ash (ptr-read-u8 p (+ offset 1)) 8)
     (ash (ptr-read-u8 p (+ offset 2)) 16) (ash (ptr-read-u8 p (+ offset 3)) 24)))

(defun nl-ffi-libffi-u16 (p offset)
  (+ (ptr-read-u8 p offset) (ash (ptr-read-u8 p (1+ offset)) 8)))

(defun nl-ffi-libffi--write-double (p offset value)
  "Pack a finite double through u32 halves; avoid the tagged u64 defect."
  (unless (numberp value) (error "libffi: double requires a number"))
  (let ((x (abs (float value))) (exponent 0) (sign (if (< value 0) 2147483648 0)))
    (unless (and (= x x) (<= x 1.7976931348623157e308))
      (error "libffi: nonfinite double unsupported"))
    (if (= x 0.0)
        (progn (ptr-write-u32 p offset 0) (ptr-write-u32 p (+ offset 4) sign))
      (while (>= x 2.0) (setq x (/ x 2.0) exponent (1+ exponent)))
      (while (< x 1.0) (setq x (* x 2.0) exponent (1- exponent)))
      (unless (and (> exponent -1023) (< exponent 1024))
        (error "libffi: nonfinite/subnormal double unsupported"))
      (let* ((fraction (* (- x 1.0) 1048576.0))
             (hi (floor fraction)) (lo (floor (* (- fraction hi) 4294967296.0))))
        (ptr-write-u32 p offset lo)
        (ptr-write-u32 p (+ offset 4) (+ sign (* (+ exponent 1023) 1048576) hi))))))

(defun nl-ffi-libffi--type (type owners)
  "Resolve TYPE, retaining aggregate owners in OWNERS (a mutable cell)."
  (if (and (consp type) (eq (car type) :struct))
      (let* ((members (cdr type))
             (o (nl-ffi-memory-allocate 24)) (p (nl-ffi-memory-address o))
             (e (nl-ffi-memory-allocate (* 8 (1+ (length members)))))
             (ep (nl-ffi-memory-address e)) (i 0))
        (setcdr owners (cons o (cons e (cdr owners))))
        (unless (and members (<= (length members) 16) (not (memq :void members)))
          (error "libffi: invalid aggregate members"))
        (ptr-write-u8 p 10 13)
        (ptr-write-u64 p 16 ep)
        (dolist (member members)
          (ptr-write-u64 ep (* i 8) (nl-ffi-libffi--type member owners))
          (setq i (1+ i)))
        p)
    (or (cdr (assq type nl-ffi-libffi--types))
        (let* ((names '((:void . "void") (:pointer . "pointer") (:double . "double")
                        (:uint8 . "uint8") (:sint8 . "sint8")
                        (:uint16 . "uint16") (:sint16 . "sint16")
                        (:uint32 . "uint32") (:sint32 . "sint32")
                        (:uint64 . "uint64") (:sint64 . "sint64")))
               (name (cdr (assq type names))))
          (unless name (error "libffi: unsupported type %S" type))
          (let ((p (nl-ffi--dlsym (nl-ffi-library-handle "libffi.so.8")
                                 (concat "ffi_type_" name))))
            (unless (> p 0) (error "libffi: missing type %s" name))
            (unless (eq type :void)
              (let ((size (cond ((memq type '(:uint8 :sint8)) 1)
                                ((memq type '(:uint16 :sint16)) 2)
                                ((memq type '(:uint32 :sint32)) 4) (t 8))))
                (unless (and (= (ptr-read-u64 p 0) size) (= (nl-ffi-libffi-u16 p 8) size))
                  (error "libffi: unexpected scalar ABI for %S" type))))
            (push (cons type p) nl-ffi-libffi--types)
            p)))))

(defun nl-ffi-libffi-prepare (library name result arguments)
  "Return an immutable cached CIF for the fixed, bounded signature.
Structs are declared as (:struct TYPE...). No varargs or callbacks.
The cache retains the library handle, CIF, types and aggregate descriptors."
  (unless (nl-ffi-memory--supported-p) (error "libffi: requires Linux x86-64"))
  (when (eq result :double) (error "libffi: double results unsupported; use the scalar provider"))
  (unless (<= (length arguments) 16) (error "libffi: arity exceeds 16"))
  (let* ((key (list library name result arguments)) (old (assoc key nl-ffi-libffi--cache)))
    (if old (cdr old)
      (ffi:library "libffi.so.8")
      (ffi:library library)
      (let* ((owners (list nil)) (complete nil)
             (c (nl-ffi-memory-allocate 32))
             (a (nl-ffi-memory-allocate (* 8 (length arguments))))
             (cp (nl-ffi-memory-address c)) (ap (nl-ffi-memory-address a))
             (handle (nl-ffi-library-handle library))
             (fn (nl-ffi--dlsym handle name)) (i 0) (rt nil))
        (setcdr owners (list c a))
        (unwind-protect
            (progn
              (unless (> fn 0) (error "libffi: unresolved %s" name))
              (setq rt (nl-ffi-libffi--type result owners))
              (dolist (arg arguments)
                (ptr-write-u64 ap (* i 8) (nl-ffi-libffi--type arg owners))
                (setq i (1+ i)))
              (unless (= 0 (nl-ffi-libffi-scalar
                            "ffi_prep_cif" [:sint32 :pointer :sint32 :uint32 :pointer :pointer]
                            cp 2 (length arguments) rt ap))
                (error "libffi: CIF rejected %s" name))
              ;; Installed SysV ABI: ffi_cif32, ffi_type24; check initialized sizes.
              (when (consp result)
                (unless (and (> (ptr-read-u64 rt 0) 0) (<= (ptr-read-u64 rt 0) 32)
                             (<= (nl-ffi-libffi-u16 rt 8) 8))
                  (error "libffi: unexpected aggregate ABI")))
              (let ((entry (vector cp fn result arguments (cdr owners) handle rt)))
                (push (cons key entry) nl-ffi-libffi--cache)
                (setq complete t)
                entry))
          (unless complete (dolist (o (cdr owners)) (nl-ffi-memory-release o))))))))

(defun nl-ffi-libffi-call (library name result arguments &rest values)
  "Call NAME with explicitly typed VALUES, including real by-value structs.
Aggregate arguments are pointers to native cells; aggregate results are byte vectors.
Full-width unsigned integers outside the runtime fixnum range are refused."
  (unless (= (length arguments) (length values)) (error "libffi: argument count"))
  (let* ((cif (nl-ffi-libffi-prepare library name result arguments))
         (n (length values)) (owners nil) (argv nil) (cells nil) (ret nil))
    (unwind-protect
        (progn
          (dolist (size (list (* 8 n) (* 8 n) 32))
            (push (nl-ffi-memory-allocate size) owners))
          (setq ret (nl-ffi-memory-address (nth 0 owners))
                cells (nl-ffi-memory-address (nth 1 owners))
                argv (nl-ffi-memory-address (nth 2 owners)))
          (let ((i 0) (types arguments))
            (dolist (value values)
              (let ((type (pop types)) (offset (* 8 i)))
                (if (consp type)
                    (ptr-write-u64 argv offset value)
                  (ptr-write-u64 argv offset (+ cells offset))
                  (if (eq type :double)
                      (nl-ffi-libffi--write-double cells offset value)
                    (unless (integerp value) (error "libffi: integer/pointer expected"))
                    (when (and (eq type :uint64) (or (< value 0) (> value 1152921504606846975)))
                      (error "libffi: uint64 outside fixnum range"))
                    (if (memq type '(:pointer :sint64 :uint64))
                        (ptr-write-u64 cells offset value)
                      (ptr-write-u32 cells offset (logand value 4294967295))))))
              (setq i (1+ i))))
          (nl-ffi-libffi-scalar "ffi_call" [:void :pointer :pointer :pointer :pointer]
                               (aref cif 0) (aref cif 1) ret argv)
          (cond
           ((consp result)
            (let* ((size (ptr-read-u64 (aref cif 6) 0)) (bytes (make-vector size 0)))
              (dotimes (i size) (aset bytes i (ptr-read-u8 ret i))) bytes))
           ((eq result :void) nil)
           ((memq result '(:pointer :uint64 :sint64)) (ptr-read-u64 ret 0))
           (t
            (let* ((bits (if (memq result '(:uint8 :sint8)) 8
                           (if (memq result '(:uint16 :sint16)) 16 32)))
                   (v (logand (nl-ffi-libffi-u32 ret 0) (1- (ash 1 bits)))))
              (if (and (memq result '(:sint8 :sint16 :sint32)) (>= v (ash 1 (1- bits))))
                  (- v (ash 1 bits)) v)))))
      (dolist (o owners) (nl-ffi-memory-release o)))))

(defun nl-ffi-libffi-release ()
  "Release prepared CIF owners after all native consumers have been destroyed."
  (dolist (entry nl-ffi-libffi--cache)
    (dolist (o (aref (cdr entry) 4)) (nl-ffi-memory-release o)))
  (setq nl-ffi-libffi--cache nil nl-ffi-libffi--types nil))

(provide 'nl-ffi-libffi)
