(load "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/.worktrees/ccore-runtime-20261002/packages/nl-ffi/src/nl-ffi.el" nil t)
(require 'nl-ffi-memory)
(ffi:library "libxcb.so.1")
(defun g1-call (name sig &rest args) (apply #'nl-ffi-compat-call nil name sig args))
(let* ((display (or (getenv "DISPLAY") ":9876")) (owner (nl-ffi-memory-cstring display))
       (p (nl-ffi-memory-address owner))
       (xcb (g1-call "xcb_connect" [:pointer :pointer :pointer] p 0)))
  (princ (format "display=%S owned-cstring=%S connection=%d error=%d fd=%d\n"
                 display (nl-ffi-get-string p) xcb
                 (g1-call "xcb_connection_has_error" [:sint32 :pointer] xcb)
                 (g1-call "xcb_get_file_descriptor" [:sint32 :pointer] xcb)))
  (g1-call "xcb_disconnect" [:void :pointer] xcb)
  (nl-ffi-memory-release owner))
(princ "XCB-DONE\n")
t
