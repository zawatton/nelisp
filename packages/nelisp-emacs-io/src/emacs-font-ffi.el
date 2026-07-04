;;; emacs-font-ffi.el --- FreeType font metrics over FFI (Doc 06 F1) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 06 F1: font metrics through FreeType via the FFI — the GUI blocker
;; (Doc 05 §6).  This module covers the *metrics* half (glyph advance widths,
;; which drive line layout); rasterization + HarfBuzz text shaping are larger
;; follow-ons.  Paralleling how the Rust GTK layer exposes font-* today.
;;
;; VERIFICATION BOUNDARY (author-only here): FreeType lives in libfreetype.so,
;; whose symbols are NOT syscalls, so the syscall shim used to verify
;; C3/D2/C1/C2 on the standalone reader cannot reach them — this needs a
;; *libffi-enabled* nelisp build (a real `nl-ffi-call' that dlopens shared
;; objects).  Under host Emacs / the syscall-shim binary
;; `emacs-font-ffi-available-p' is nil and every entry point degrades to nil.
;;
;; Advance is read via `FT_Get_Char_Index' + `FT_Get_Advance' (an out-param,
;; FT_Fixed 16.16) rather than by walking the `FT_GlyphSlot' struct, so no
;; fragile struct-offset reads are needed for the common metric.  Ascender /
;; descender (in `face->size->metrics') do need struct offsets and are left as
;; a follow-up.
;;
;; FreeType C lifecycle mirrored here:
;;   FT_Init_FreeType(&library)
;;   FT_New_Face(library, path, 0, &face)
;;   FT_Set_Pixel_Sizes(face, 0, px)
;;   gi = FT_Get_Char_Index(face, charcode)
;;   FT_Get_Advance(face, gi, FT_LOAD_DEFAULT, &advance)   ; 16.16 fixed
;;   FT_Done_Face(face); FT_Done_FreeType(library)

;;; Code:

(require 'emacs-network-ffi)

(defvar emacs-font-ffi-libfreetype-path
  (or (and (fboundp 'getenv) (getenv "EMACS_FONT_FFI_LIBFREETYPE"))
      (let ((candidates
             '("/lib/x86_64-linux-gnu/libfreetype.so.6"
               "/lib64/libfreetype.so.6"
               "/lib/aarch64-linux-gnu/libfreetype.so.6"
               "/usr/lib/libfreetype.so.6"
               "/usr/lib/libfreetype.so"
               "/usr/lib/libfreetype.dylib")))
        (let ((found nil))
          (while (and candidates (not found))
            (when (and (fboundp 'file-readable-p)
                       (file-readable-p (car candidates)))
              (setq found (car candidates)))
            (setq candidates (cdr candidates)))
          found))
      "/lib/x86_64-linux-gnu/libfreetype.so.6")
  "Absolute path to the FreeType shared object.
Override via `EMACS_FONT_FFI_LIBFREETYPE' or by `setq' before this file loads.")

(defconst emacs-font-ffi-FT_LOAD_DEFAULT 0 "FT_Load flags: default loading.")

(defun emacs-font-ffi-available-p ()
  "Non-nil when the FreeType FFI can be used (needs a libffi nelisp build)."
  (and (fboundp 'nl-ffi-call)
       (stringp emacs-font-ffi-libfreetype-path)
       (or (not (fboundp 'file-readable-p))
           (file-readable-p emacs-font-ffi-libfreetype-path))))

(defun emacs-font-ffi--call (func sig &rest args)
  "Dispatch FreeType FUNC with SIG + ARGS through `nl-ffi-call'."
  (apply #'nl-ffi-call emacs-font-ffi-libfreetype-path func sig args))

(defun emacs-font-ffi--read-ptr (buf off)
  "Read a 64-bit pointer/long from BUF at OFF as two 32-bit halves."
  (logior (logand (nl-ffi-read-i32 buf off) #xffffffff)
          (ash (logand (nl-ffi-read-i32 buf (+ off 4)) #xffffffff) 32)))

(defun emacs-font-ffi-open (path pixel-size)
  "Open the font file PATH at PIXEL-SIZE.
Returns a plist (:library PTR :face PTR :size PIXEL-SIZE) on success, or nil
when the FFI is unavailable or FreeType fails (Doc 06 F1)."
  (when (and (emacs-font-ffi-available-p) (stringp path)
             (integerp pixel-size) (> pixel-size 0))
    (let ((libbuf (nl-ffi-malloc 8)))
      (if (not (= 0 (emacs-font-ffi--call "FT_Init_FreeType"
                                          [:sint32 :pointer] libbuf)))
          (progn (nl-ffi-free libbuf) nil)
        (let* ((library (emacs-font-ffi--read-ptr libbuf 0))
               (facebuf (nl-ffi-malloc 8))
               (rc (emacs-font-ffi--call
                    "FT_New_Face" [:sint32 :sint64 :string :sint64 :pointer]
                    library path 0 facebuf)))
          (nl-ffi-free libbuf)
          (if (not (= 0 rc))
              (progn (nl-ffi-free facebuf)
                     (emacs-font-ffi--call "FT_Done_FreeType" [:sint32 :sint64]
                                           library)
                     nil)
            (let ((face (emacs-font-ffi--read-ptr facebuf 0)))
              (nl-ffi-free facebuf)
              (emacs-font-ffi--call "FT_Set_Pixel_Sizes"
                                    [:sint32 :sint64 :sint32 :sint32]
                                    face 0 pixel-size)
              (list :library library :face face :size pixel-size))))))))

(defun emacs-font-ffi-char-advance (font charcode)
  "Return the horizontal advance of CHARCODE in FONT, in whole pixels, or nil.
FONT is a plist from `emacs-font-ffi-open' (Doc 06 F1)."
  (when (and (emacs-font-ffi-available-p) font (integerp charcode))
    (let* ((face (plist-get font :face))
           (gi (emacs-font-ffi--call "FT_Get_Char_Index"
                                     [:sint32 :sint64 :sint64] face charcode))
           (advbuf (nl-ffi-malloc 8))
           (rc (emacs-font-ffi--call
                "FT_Get_Advance" [:sint32 :sint64 :sint32 :sint32 :pointer]
                face gi emacs-font-ffi-FT_LOAD_DEFAULT advbuf)))
      (prog1
          (when (= 0 rc)
            ;; advance is FT_Fixed (16.16); round to whole pixels.
            (ash (+ (emacs-font-ffi--read-ptr advbuf 0) #x8000) -16))
        (nl-ffi-free advbuf)))))

(defun emacs-font-ffi-close (font)
  "Release FONT's FreeType face + library.  Returns t when it acted."
  (when (and (emacs-font-ffi-available-p) font)
    (emacs-font-ffi--call "FT_Done_Face" [:sint32 :sint64] (plist-get font :face))
    (emacs-font-ffi--call "FT_Done_FreeType" [:sint32 :sint64]
                          (plist-get font :library))
    t))

(provide 'emacs-font-ffi)
;;; emacs-font-ffi.el ends here
