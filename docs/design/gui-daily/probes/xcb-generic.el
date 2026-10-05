;;; Generic XCB API/descriptor probe on a deliberately unusable connection.
;;; Sequence0 on failed connection is negative evidence, never a mapped-window pass.
(load "/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/.worktrees/ccore-runtime-20261002/packages/nl-ffi/src/nl-ffi.el" nil t)
(require 'nl-ffi-memory)
(ffi:library "libxcb.so.1")
(ffi:library "libcairo.so.2")
(ffi:library "libxkbcommon-x11.so.0")
(defun g1-call (name sig &rest args) (apply #'nl-ffi-compat-call nil name sig args))
(dolist (entry '(("libxcb.so.1" . "xcb_send_request") ("libxcb.so.1" . "xcb_poll_for_reply")
                 ("libxcb.so.1" . "xcb_request_check") ("libxcb.so.1" . "xcb_get_setup")
                 ("libcairo.so.2" . "cairo_xcb_surface_create")
                 ("libxkbcommon-x11.so.0" . "xkb_x11_setup_xkb_extension")
                 ("libxkbcommon-x11.so.0" . "xkb_x11_state_new_from_device")))
  (let ((p (nl-ffi--dlsym (nl-ffi-library-handle (car entry)) (cdr entry))))
    (princ (format "symbol-%s=%d\n" (cdr entry) p))
    (unless (> p 0) (error "Missing symbol %S" entry))))
(let* ((name-o (nl-ffi-memory-cstring ":9876"))
       (connection (g1-call "xcb_connect" [:pointer :pointer :pointer] (nl-ffi-memory-address name-o) 0))
       (iov-o (nl-ffi-memory-allocate 48)) (iov (nl-ffi-memory-address iov-o))
       (packet-o (nl-ffi-memory-allocate 4)) (packet (nl-ffi-memory-address packet-o))
       (request-o (nl-ffi-memory-allocate 24)) (request (nl-ffi-memory-address request-o)))
  (unwind-protect
      (progn
        (ptr-write-u8 packet 0 127) (ptr-write-u8 packet 2 1) ; NoOperation
        (ptr-write-u64 iov 32 packet) (ptr-write-u64 iov 40 4)
        (ptr-write-u64 request 0 1)
        (ptr-write-u8 request 16 127) (ptr-write-u8 request 17 1)
        (let ((err (g1-call "xcb_connection_has_error" [:sint32 :pointer] connection))
              (seq (g1-call "xcb_send_request" [:uint32 :pointer :sint32 :pointer :pointer]
                            connection 0 (+ iov 32) request)))
          (princ (format "dead-connection-send-request=error:%d sequence:%d\n" err seq))
          (unless (and (> err 0) (= seq 0)) (error "Expected dead connection/sequence0"))))
    (g1-call "xcb_disconnect" [:void :pointer] connection)
    (dolist (o (list name-o iov-o packet-o request-o)) (nl-ffi-memory-release o))))
(princ "XCB-GENERIC-DONE\n")
t
