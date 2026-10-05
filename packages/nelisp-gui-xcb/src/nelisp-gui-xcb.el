;;; nelisp-gui-xcb.el --- XCB transport, native ownership and input -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nl-ffi-libffi)
(require 'emacs-command-loop)
(define-error 'nelisp-gui-xcb-error "XCB transport error")
(defconst nelisp-gui-xcb-cookie-type '(:struct :uint32))
;; State: connection, window, visual, screen, xkb context/keymap/state, alive.
;; No live native pointer is installed while loading/baking this file.

(defun nelisp-gui-xcb-call (name signature &rest args)
  (apply #'nl-ffi-libffi-scalar name signature args))

(defun nelisp-gui-xcb--bytes-number (bytes offset count)
  (let ((n 0))
    (dotimes (i count) (setq n (+ n (ash (aref bytes (+ offset i)) (* 8 i))))) n))

(defun nelisp-gui-xcb-check (state)
  "Signal a controlled condition on dead connections, never enter a blocking wait."
  (unless (and (aref state 7) (= 0 (nelisp-gui-xcb-call
                                 "xcb_connection_has_error" [:sint32 :pointer] (aref state 0))))
    (signal 'nelisp-gui-xcb-error (list 'server-death)))
  t)

(defun nelisp-gui-xcb-cookie (state name types &rest args)
  "Send a generated XCB request with an actual cookie struct result."
  (nelisp-gui-xcb-check state)
  (let* ((bytes (apply #'nl-ffi-libffi-call "libxcb.so.1" name
                       nelisp-gui-xcb-cookie-type (cons :pointer types)
                       (aref state 0) args))
         (sequence (nelisp-gui-xcb--bytes-number bytes 0 4)))
    (unless (> sequence 0) (signal 'nelisp-gui-xcb-error (list 'zero-cookie name)))
    sequence))

(defun nelisp-gui-xcb-barrier (state)
  "Poll a later reply with a two-second deadline before checking void cookies."
  (let ((seq (nelisp-gui-xcb-cookie state "xcb_get_input_focus" nil))
        (o (nl-ffi-memory-allocate 16)) (done nil) (deadline (+ (float-time) 2.0)))
    (unwind-protect
        (let ((p (nl-ffi-memory-address o)))
          (nelisp-gui-xcb-call "xcb_flush" [:sint32 :pointer] (aref state 0))
          (while (and (not done) (< (float-time) deadline))
            (nelisp-gui-xcb-check state)
            (setq done (= 1 (nelisp-gui-xcb-call
                             "xcb_poll_for_reply" [:sint32 :pointer :uint32 :pointer :pointer]
                             (aref state 0) seq p (+ p 8))))
            (unless done (sleep-for 0.005)))
          (unless done (signal 'nelisp-gui-xcb-error (list 'reply-timeout seq)))
          (let ((reply (ptr-read-u64 p 0)) (err (ptr-read-u64 p 8)))
            (when (> reply 0) (nelisp-gui-xcb-call "free" [:void :pointer] reply))
            (when (> err 0)
              (nelisp-gui-xcb-call "free" [:void :pointer] err)
              (signal 'nelisp-gui-xcb-error (list 'barrier-error)))))
      (nl-ffi-memory-release o))))

(defun nelisp-gui-xcb-request-check (state sequence)
  "Return (CODE RESOURCE SEQUENCE) or nil, after a completed later barrier."
  (nelisp-gui-xcb-barrier state)
  (nelisp-gui-xcb-check state)
  (let ((o (nl-ffi-memory-allocate 8)))
    (unwind-protect
        (let ((p (nl-ffi-memory-address o)))
          (ptr-write-u32 p 0 sequence)
          (let ((err (nl-ffi-libffi-call "libxcb.so.1" "xcb_request_check" :pointer
                                       (list :pointer nelisp-gui-xcb-cookie-type) (aref state 0) p)))
            (when (> err 0)
              (unwind-protect
                  (list (ptr-read-u8 err 1) (nl-ffi-libffi-u32 err 4)
                        (nl-ffi-libffi-u32 err 32))
                (nelisp-gui-xcb-call "free" [:void :pointer] err)))))
      (nl-ffi-memory-release o))))

(defun nelisp-gui-xcb-checked (state name types &rest args)
  (let* ((seq (apply #'nelisp-gui-xcb-cookie state name types args))
         (err (nelisp-gui-xcb-request-check state seq)))
    (when err (signal 'nelisp-gui-xcb-error (list name err))) seq))

(defun nelisp-gui-xcb--screen (connection screen-number)
  "Parse setup only inside its advertised byte bounds; select matching visual."
  (let* ((setup (nelisp-gui-xcb-call "xcb_get_setup" [:pointer :pointer] connection))
         (bytes (nl-ffi-libffi-call "libxcb.so.1" "xcb_setup_roots_iterator"
                                  '(:struct :pointer :sint32 :sint32) '(:pointer) setup))
         (screen (nelisp-gui-xcb--bytes-number bytes 0 8))
         (count (nelisp-gui-xcb--bytes-number bytes 8 4))
         (end (+ setup 8 (* 4 (nl-ffi-libffi-u16 setup 6))))
         (index 0) (visual nil))
    (unless (and (> setup 0) (> screen 0) (< screen-number count) (<= count 16))
      (error "XCB: invalid screen selection"))
    (while (<= index screen-number)
      (unless (<= (+ screen 40) end) (error "XCB: truncated screen"))
      (let ((depth (+ screen 40)) (n (ptr-read-u8 screen 39))
            (root-visual (nl-ffi-libffi-u32 screen 32)))
        (dotimes (_ n)
          (unless (<= (+ depth 8) end) (error "XCB: truncated depth"))
          (let* ((nv (nl-ffi-libffi-u16 depth 2)) (vp (+ depth 8)))
            (unless (<= (+ vp (* nv 24)) end) (error "XCB: truncated visuals"))
            (when (= index screen-number)
              (dotimes (i nv)
                (when (= root-visual (nl-ffi-libffi-u32 vp (* i 24)))
                  (setq visual (+ vp (* i 24))))))
            (setq depth (+ vp (* nv 24)))))
        (if (= index screen-number)
            (setq index (1+ index))
          (setq screen depth index (1+ index)))))
    (unless visual (error "XCB: root visual not found"))
    (list screen visual)))

(defun nelisp-gui-xcb-open (title width height)
  "Create an authenticated, checked, mapped XCB window with server XKB state."
  (dolist (lib '("libxcb.so.1" "libcairo.so.2" "libxkbcommon.so.0"
                 "libxkbcommon-x11.so.0" "libc.so.6")) (ffi:library lib))
  (let ((state (vector 0 0 0 0 0 0 0 t)) (complete nil)
        (screen-o (nl-ffi-memory-allocate 4)) (params (nl-ffi-memory-allocate 8)))
    (unwind-protect
        (progn
          (aset state 0 (nelisp-gui-xcb-call "xcb_connect" [:pointer :pointer :pointer]
                                           0 (nl-ffi-memory-address screen-o)))
          (nelisp-gui-xcb-check state)
          (let* ((pair (nelisp-gui-xcb--screen (aref state 0)
                                            (nl-ffi-libffi-u32 (nl-ffi-memory-address screen-o) 0)))
                 (screen (car pair)) (visual (cadr pair))
                 (wid (nelisp-gui-xcb-call "xcb_generate_id" [:uint32 :pointer] (aref state 0)))
                 (p (nl-ffi-memory-address params)))
            (aset state 1 wid) (aset state 2 visual) (aset state 3 screen)
            (ptr-write-u32 p 0 (nl-ffi-libffi-u32 screen 12))
            ;; Key press/release, exposure, structure and focus notifications.
            (ptr-write-u32 p 4 (+ 1 2 32768 131072 2097152))
            (nelisp-gui-xcb-checked
             state "xcb_create_window_checked"
             '(:uint8 :uint32 :uint32 :sint16 :sint16 :uint16 :uint16 :uint16 :uint16 :uint32 :uint32 :pointer)
             (ptr-read-u8 screen 38) wid (nl-ffi-libffi-u32 screen 0)
             32 32 width height 0 1 (nl-ffi-libffi-u32 screen 32) (+ 2 2048) p)
            (let ((text (nl-ffi-memory-cstring title)))
              (unwind-protect
                  (nelisp-gui-xcb-checked state "xcb_change_property_checked"
                                          '(:uint8 :uint32 :uint32 :uint32 :uint8 :uint32 :pointer)
                                          0 wid 39 31 8 (length title) (nl-ffi-memory-address text))
                (nl-ffi-memory-release text)))
            (nelisp-gui-xcb-checked state "xcb_map_window_checked" '(:uint32) wid)
            (nelisp-gui-xcb-checked state "xcb_set_input_focus_checked" '(:uint8 :uint32 :uint32)
                                    1 wid 0))
          (let ((o (nl-ffi-memory-allocate 16)))
            (unwind-protect
                (let ((p (nl-ffi-memory-address o)))
                  (unless (= 1 (nl-ffi-libffi-call
                                "libxkbcommon-x11.so.0" "xkb_x11_setup_xkb_extension" :sint32
                                '(:pointer :uint16 :uint16 :sint32 :pointer :pointer :pointer :pointer)
                                (aref state 0) 1 0 0 p (+ p 2) (+ p 4) (+ p 5)))
                    (error "XKB negotiation failed"))
                  (let ((device (nelisp-gui-xcb-call "xkb_x11_get_core_keyboard_device_id"
                                                   [:sint32 :pointer] (aref state 0))))
                    (unless (< device 2147483648) (error "XKB device unavailable"))
                    (aset state 4 (nelisp-gui-xcb-call "xkb_context_new" [:pointer :uint32] 0))
                    (aset state 5 (nelisp-gui-xcb-call "xkb_x11_keymap_new_from_device"
                                                     [:pointer :pointer :pointer :sint32 :uint32]
                                                     (aref state 4) (aref state 0) device 0))
                    (unless (> (aref state 5) 0) (error "XKB server keymap unavailable"))
                    (aset state 6 (nelisp-gui-xcb-call "xkb_x11_state_new_from_device"
                                                     [:pointer :pointer :pointer :sint32]
                                                     (aref state 5) (aref state 0) device))
                    (unless (> (aref state 6) 0) (error "XKB state unavailable"))))
              (nl-ffi-memory-release o)))
          (setq complete t) state)
      (nl-ffi-memory-release screen-o) (nl-ffi-memory-release params)
      (unless complete (nelisp-gui-xcb-close state)))))

(defun nelisp-gui-xcb-key (state event)
  "Translate core X events via the negotiated server keymap into shared events."
  (let* ((code (ptr-read-u8 event 1)) (mods (nl-ffi-libffi-u16 event 28))
         (xkb (aref state 6)))
    (nl-ffi-libffi-call "libxkbcommon.so.0" "xkb_state_update_mask" :uint32
                       '(:pointer :uint32 :uint32 :uint32 :uint32 :uint32 :uint32)
                       xkb (logand mods 255) 0 0 0 0 (logand (ash mods -13) 3))
    (let* ((sym (nelisp-gui-xcb-call "xkb_state_key_get_one_sym" [:uint32 :pointer :uint32] xkb code))
           (unicode (nelisp-gui-xcb-call "xkb_state_key_get_utf32" [:uint32 :pointer :uint32] xkb code))
           (ev (emacs-command-loop-normalize-key-event
                sym mods unicode :control-mask 4
                :named-events '((65293 . 13) (65288 . 127) (65307 . 27)
                                (65361 . left) (65363 . right) (65362 . up) (65364 . down)
                                (65481 . f12)))))
      (when ev (if (and (integerp ev) (= (logand mods 8) 8)) (logior ev 134217728) ev)))))

(defun nelisp-gui-xcb-poll (state)
  "Return one transport event, freeing its native event exactly once."
  (nelisp-gui-xcb-check state)
  (let ((p (nelisp-gui-xcb-call "xcb_poll_for_event" [:pointer :pointer] (aref state 0))))
    (if (= p 0) (progn (nelisp-gui-xcb-check state) nil)
      (unwind-protect
          (let ((type (logand (ptr-read-u8 p 0) 127)))
            (cond
             ((= type 0) (signal 'nelisp-gui-xcb-error
                                (list 'asynchronous (ptr-read-u8 p 1) (nl-ffi-libffi-u32 p 4))))
             ((= type 2) (list :key (nelisp-gui-xcb-key state p)))
             ((= type 12) '(:expose t))
             ((= type 17) (signal 'nelisp-gui-xcb-error (list 'window-destroyed)))
             (t '(:ignored t))))
        (nelisp-gui-xcb-call "free" [:void :pointer] p)))))

(defun nelisp-gui-xcb-bad-window (state)
  "Exercise BadWindow3 with the matching resource/sequence, then recover."
  (let* ((seq (nelisp-gui-xcb-cookie state "xcb_map_window_checked" '(:uint32) 0))
         (err (nelisp-gui-xcb-request-check state seq)))
    (unless (equal err (list 3 0 seq)) (error "BadWindow mismatch: %S seq=%d" err seq))
    (nelisp-gui-xcb-checked state "xcb_map_window_checked" '(:uint32) (aref state 1))
    (princ (format "GUI-BAD-WINDOW|code=3|resource=0|sequence=%d|recovered=1\n" seq))))

(defun nelisp-gui-xcb-close (state)
  "Destroy native owners; disconnect also works on a dead X server."
  (dolist (entry '((6 . "xkb_state_unref") (5 . "xkb_keymap_unref") (4 . "xkb_context_unref")))
    (when (> (aref state (car entry)) 0)
      (nelisp-gui-xcb-call (cdr entry) [:void :pointer] (aref state (car entry)))
      (aset state (car entry) 0)))
  (when (> (aref state 0) 0)
    (nelisp-gui-xcb-call "xcb_disconnect" [:void :pointer] (aref state 0))
    (aset state 0 0))
  (aset state 7 nil))

(provide 'nelisp-gui-xcb)
