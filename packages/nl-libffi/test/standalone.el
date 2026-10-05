;;; standalone.el --- Actual native libffi ABI/lifetime acceptance -*- lexical-binding: t; -*-
(require 'nl-ffi-libffi)
(let ((canvas 0) (cr 0) (bounds nil) (complete nil))
  (unwind-protect
      (progn
        (unless (= -1 (nl-ffi-libffi-call "libcairo.so.2" "cairo_format_stride_for_width"
                                         :sint32 '(:sint32 :sint32) 0 -1))
          (error "signed result not normalized"))
        (unless (= -7 (nl-ffi-libffi-call "libc.so.6" "atoi" :sint32 '(:pointer)
                                         (let ((o (nl-ffi-memory-cstring "-7")))
                                           (setq bounds o) (nl-ffi-memory-address o))))
          (error "negative argument/result cell"))
        (nl-ffi-memory-release bounds) (setq bounds nil)
        (let ((first (nl-ffi-libffi-call "libc.so.6" "div" '(:struct :sint32 :sint32)
                                        '(:sint32 :sint32) 17 5)))
          (unless (equal first [3 0 0 0 2 0 0 0]) (error "real aggregate return %S" first)))
        (garbage-collect)
        (unless (equal (nl-ffi-libffi-call "libc.so.6" "div" '(:struct :sint32 :sint32)
                                          '(:sint32 :sint32) 17 5) [3 0 0 0 2 0 0 0])
          (error "CIF aggregate owners did not survive GC"))
        (setq canvas (nl-ffi-libffi-call "libcairo.so.2" "cairo_image_surface_create"
                                        :pointer '(:sint32 :sint32 :sint32) 0 100 100)
              cr (nl-ffi-libffi-call "libcairo.so.2" "cairo_create" :pointer '(:pointer) canvas)
              bounds (nl-ffi-memory-allocate 32))
        ;; The fifth-position double cannot use the existing direct call path.
        (nl-ffi-libffi-call "libcairo.so.2" "cairo_rectangle" :void
                           '(:pointer :double :double :double :double) cr 1.25 2.5 30.75 40.0)
        (let ((p (nl-ffi-memory-address bounds)))
          (nl-ffi-libffi-call "libcairo.so.2" "cairo_path_extents" :void
                             '(:pointer :pointer :pointer :pointer :pointer)
                             cr p (+ p 8) (+ p 16) (+ p 24))
          ;; IEEE754 little-endian high halves: 1.25, 2.5, 32, 42.5.
          (unless (equal (list (nl-ffi-libffi-u32 p 4) (nl-ffi-libffi-u32 p 12)
                              (nl-ffi-libffi-u32 p 20) (nl-ffi-libffi-u32 p 28))
                         '(1072955392 1074003968 1077936128 1078280192))
            (error "safe double packing/high-arity path extents")))
        (setq complete t)
        (princ "LIBFFI-ABI-PASS|signed=2|aggregate=2|double-position5=1|gc=1\n"))
    (when bounds (nl-ffi-memory-release bounds))
    (when (> cr 0) (nl-ffi-libffi-scalar "cairo_destroy" [:void :pointer] cr))
    (when (> canvas 0) (nl-ffi-libffi-scalar "cairo_surface_destroy" [:void :pointer] canvas))
    (nl-ffi-libffi-release))
  (unless complete (error "libffi suite incomplete")))
t
