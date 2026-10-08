;;; nelisp-gui-xcb.el --- XCB transport, native ownership and input -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nl-ffi-libffi)
(require 'emacs-command-loop)
(define-error 'nelisp-gui-xcb-error "XCB transport error")
(defconst nelisp-gui-xcb-cookie-type '(:struct :uint32))
;; State: connection, window, visual, screen, xkb context/keymap/state, alive,
;; negotiated XKB event base, input fd, owned pollfd storage, selected device,
;; serialized effective keyboard group, prefetched owned native event.
;; No live native pointer is installed while loading/baking this file.

(defvar nelisp-gui-xcb--calls (make-hash-table :test 'eq)
  "Process-local prepared scalar calls; never populated at image build time.")

(defvar nelisp-gui-xcb-trace-events nil
  "Log every received core/extension event when explicitly requested.")
(defvar nelisp-gui-xcb--mask-frame nil
  "Process-local owned argument storage for the seven-argument XKB mask ABI.")

(defun nelisp-gui-xcb--update-mask (state event &optional core-mods)
  "Apply StateNotify, or the effective CORE-MODS snapshot of a key event.
Reuse owned argument storage; a key snapshot preserves the ordered XKB group."
  (unless nelisp-gui-xcb--mask-frame
    (let* ((cif (nl-ffi-libffi-prepare "libxkbcommon.so.0" "xkb_state_update_mask" :uint32
                                     '(:pointer :uint32 :uint32 :uint32 :uint32 :uint32 :uint32)))
           (owner (nl-ffi-memory-allocate 128)) (p (nl-ffi-memory-address owner)))
      (dotimes (i 7) (ptr-write-u64 p (* i 8) (+ p 56 (* i 8))))
      (setq nelisp-gui-xcb--mask-frame (vector cif owner p))))
  (let* ((frame nelisp-gui-xcb--mask-frame) (cif (aref frame 0))
         (p (aref frame 2)) (cells (+ p 56)))
    (ptr-write-u64 cells 0 (aref state 6))
    (ptr-write-u32 cells 8 (if core-mods (logand core-mods 255) (ptr-read-u8 event 10)))
    (ptr-write-u32 cells 16 (if core-mods 0 (ptr-read-u8 event 11)))
    (ptr-write-u32 cells 24 (if core-mods 0 (ptr-read-u8 event 12)))
    (ptr-write-u32 cells 32 (if core-mods 0 (logand (nelisp-gui-xcb--signed16 event 14) 4294967295)))
    (ptr-write-u32 cells 40 (if core-mods 0 (logand (nelisp-gui-xcb--signed16 event 16) 4294967295)))
    (ptr-write-u32 cells 48 (if core-mods (aref state 12) (ptr-read-u8 event 18)))
    (nelisp-gui-xcb-call "ffi_call" [:void :pointer :pointer :pointer :pointer]
                         (aref cif 0) (aref cif 1) (+ p 112) p)
    (unless core-mods
      (aset state 12 (nelisp-gui-xcb-call "xkb_state_serialize_layout" [:uint32 :pointer :uint32]
                                         (aref state 6) 128)))))

(defun nelisp-gui-xcb--scalar-address (name signature)
  "Retain a scalar address through the public owned libffi descriptor API."
  ;; ptr-call-typed has six argument slots and only four float mask bits.
  ;; Bit 16 describes the return value, not a fifth double argument.
  (when (and (<= 1 (length signature) 7)
             (memq (aref signature 0)
                   '(:void :pointer :uint8 :sint8 :uint16 :sint16
                     :uint32 :sint32 :uint64 :sint64))
             (let ((i 1) (eligible t))
               (while (and eligible (< i (length signature)))
                 (setq eligible
                       (or (memq (aref signature i)
                                 '(:pointer :uint8 :sint8 :uint16 :sint16
                                   :uint32 :sint32 :uint64 :sint64))
                           (and (<= i 4) (eq (aref signature i) :double))))
                 (setq i (1+ i)))
               eligible))
    (let ((library
           (cond ((string-prefix-p "ffi_" name) "libffi.so.8")
                 ((string-prefix-p "xcb_xkb_" name) "libxcb-xkb.so.1")
                 ((string-prefix-p "xcb_" name) "libxcb.so.1")
                 ((string-prefix-p "xkb_x11_" name) "libxkbcommon-x11.so.0")
                 ((string-prefix-p "xkb_" name) "libxkbcommon.so.0")
                 ((string-prefix-p "cairo_" name) "libcairo.so.2")
                 ((string-prefix-p "pango_" name) "libpangocairo-1.0.so.0")
                 ((string-prefix-p "g_" name) "libgobject-2.0.so.0")
                 (t "libc.so.6"))))
      (aref (nl-ffi-libffi-prepare library name (aref signature 0)
                                   (cdr (append signature nil))) 1))))

(defun nelisp-gui-xcb-call (name signature &rest args)
  "Call a fixed GUI scalar ABI, caching resolution and argument classification.
The existing scalar provider validates the first call.  Later numeric calls
use the same public ptr-call ABI with six padded arguments.  Aggregate calls
and unsupported scalar signatures continue through the original provider."
  (let* ((key (intern name)) (cached (gethash key nelisp-gui-xcb--calls))
         (entry (and cached (or (eq signature (aref cached 3)) (equal signature (aref cached 3))) cached)))
    (if (not entry)
        (let ((result (apply #'nl-ffi-libffi-scalar name signature args))
              (address (nelisp-gui-xcb--scalar-address name signature)) (mask 0))
          (when (and address (> address 0))
            (dotimes (i (length signature))
              (when (eq (aref signature i) :double)
                (setq mask (logior mask (if (= i 0) 16 (ash 1 (1- i)))))))
            (puthash key (vector address mask (1- (length signature)) (copy-sequence signature)) nelisp-gui-xcb--calls))
          result)
      (unless (= (length args) (aref entry 2))
        (signal 'nl-ffi-wrong-arity (list name (aref entry 2) (length args))))
      ;; This adapter receives already-owned addresses, never Lisp strings.
      ;; Retain scalar argument checks before crossing the ABI seam.
      (let ((i 1) (rest args))
        (while rest
          (unless (if (eq (aref signature i) :double) (numberp (car rest))
                    (integerp (car rest)))
            (signal 'wrong-type-argument (list 'numberp (car rest))))
          (when (eq (aref signature i) :double) (setcar rest (float (car rest))))
          (setq i (1+ i) rest (cdr rest))))
      ;; Fixed arity avoids an appended/padded list on every native call.
      (let ((result (if (= 0 (aref entry 1))
                        (ptr-call (aref entry 0) (or (car args) 0) (or (cadr args) 0)
                                  (or (nth 2 args) 0) (or (nth 3 args) 0)
                                  (or (nth 4 args) 0) (or (nth 5 args) 0))
                      (ptr-call-typed (aref entry 0) (aref entry 1)
                                      (or (car args) 0) (or (cadr args) 0)
                                      (or (nth 2 args) 0) (or (nth 3 args) 0)
                                      (or (nth 4 args) 0) (or (nth 5 args) 0)))))
        (if (eq (aref signature 0) :void) nil result)))))

(defun nelisp-gui-xcb-bytes-number (bytes offset count)
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
  (let* ((library (if (string-prefix-p "xcb_xkb_" name) "libxcb-xkb.so.1" "libxcb.so.1"))
         (bytes (apply #'nl-ffi-libffi-call library name
                       nelisp-gui-xcb-cookie-type (cons :pointer types)
                       (aref state 0) args))
         (sequence (nelisp-gui-xcb-bytes-number bytes 0 4)))
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
         (screen (nelisp-gui-xcb-bytes-number bytes 0 8))
         (count (nelisp-gui-xcb-bytes-number bytes 8 4))
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
  (clrhash nelisp-gui-xcb--calls)
  (dolist (lib '("libxcb.so.1" "libcairo.so.2" "libxkbcommon.so.0"
                 "libxkbcommon-x11.so.0" "libxcb-xkb.so.1" "libc.so.6")) (ffi:library lib))
  ;; Cold images may have been baked under another DISPLAY.  The reader's
  ;; environment is authoritative; synchronize libc's lazy-loaded environment
  ;; before XCB performs authentication or consults its default display.
  (dolist (name '("DISPLAY" "XAUTHORITY" "HOME"))
    (let ((value (getenv name)) (key (nl-ffi-memory-cstring name)))
      (unwind-protect
          (if value
              (let ((o (nl-ffi-memory-cstring value)))
                (unwind-protect
                    (nelisp-gui-xcb-call "setenv" [:sint32 :pointer :pointer :sint32]
                                         (nl-ffi-memory-address key) (nl-ffi-memory-address o) 1)
                  (nl-ffi-memory-release o)))
            (nelisp-gui-xcb-call "unsetenv" [:sint32 :pointer] (nl-ffi-memory-address key)))
        (nl-ffi-memory-release key))))
  (let ((state (vector 0 0 0 0 0 0 0 t 0 nil nil nil 0 0)) (complete nil)
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
            (ptr-write-u32 p 4 (+ 1 2 4 8 64 32768 131072 2097152 4194304))
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
            ;; Under a window manager the map request is redirected, so the
            ;; window is not viewable yet and SetInputFocus fails with
            ;; BadMatch (8).  The WM gives focus on map, as for GNU Emacs;
            ;; only a bare server (Xvfb) needs this request.
            (condition-case err
                (nelisp-gui-xcb-checked state "xcb_set_input_focus_checked" '(:uint8 :uint32 :uint32)
                                        1 wid 0)
              (nelisp-gui-xcb-error
               (unless (eq 8 (car-safe (nth 2 err)))
                 (signal (car err) (cdr err))))))
          (let ((o (nl-ffi-memory-allocate 16)))
            (unwind-protect
                (let ((p (nl-ffi-memory-address o)))
                  (unless (= 1 (nl-ffi-libffi-call
                                "libxkbcommon-x11.so.0" "xkb_x11_setup_xkb_extension" :sint32
                                '(:pointer :uint16 :uint16 :sint32 :pointer :pointer :pointer :pointer)
                                (aref state 0) 1 0 0 p (+ p 2) (+ p 4) (+ p 5)))
                    (error "XKB negotiation failed"))
                  (aset state 8 (ptr-read-u8 p 4))
                  (let ((device (nelisp-gui-xcb-call "xkb_x11_get_core_keyboard_device_id"
                                                     [:sint32 :pointer] (aref state 0))))
                    (unless (< device 2147483648) (error "XKB device unavailable"))
                    (nelisp-gui-xcb-select-keyboard-events state device)
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
          (aset state 9 (nelisp-gui-xcb-call "xcb_get_file_descriptor"
                                             [:sint32 :pointer] (aref state 0)))
          (aset state 10 (nl-ffi-memory-allocate 8))
          (aset state 12 (nelisp-gui-xcb-call "xkb_state_serialize_layout" [:uint32 :pointer :uint32]
                                             (aref state 6) 128))
          (setq complete t) state)
      (nl-ffi-memory-release screen-o) (nl-ffi-memory-release params)
      (unless complete (nelisp-gui-xcb-close state)))))

(defun nelisp-gui-xcb-select-keyboard-events (state device)
  "Select checked XKB map/device/state notifications for DEVICE."
  ;; https://xkbcommon.org/doc/current/group__x11.html
  ;; selectAll=7 needs no variable details; all eight map parts are selected.
  (unless (equal (getenv "NELISP_GUI_FAULT") "stale-keymap")
    (nelisp-gui-xcb-checked state "xcb_xkb_select_events_checked"
                            '(:uint16 :uint16 :uint16 :uint16 :uint16 :uint16 :pointer)
                            device 7 0 7 255 255 0)
    (aset state 11 device)))

(defun nelisp-gui-xcb-refresh-keymap (state)
  "Refresh the server device keymap after MappingNotify or focus restoration."
  (let* ((device (nelisp-gui-xcb-call "xkb_x11_get_core_keyboard_device_id"
                                      [:sint32 :pointer] (aref state 0)))
         (map (nelisp-gui-xcb-call "xkb_x11_keymap_new_from_device"
                                   [:pointer :pointer :pointer :sint32 :uint32]
                                   (aref state 4) (aref state 0) device 0))
         (xkb (and (> map 0) (nelisp-gui-xcb-call "xkb_x11_state_new_from_device"
                                                  [:pointer :pointer :pointer :sint32]
                                                  map (aref state 0) device))))
    (unless (and xkb (> xkb 0))
      (when (> map 0) (nelisp-gui-xcb-call "xkb_keymap_unref" [:void :pointer] map))
      (error "XKB refresh failed"))
    (nelisp-gui-xcb-call "xkb_state_unref" [:void :pointer] (aref state 6))
    (nelisp-gui-xcb-call "xkb_keymap_unref" [:void :pointer] (aref state 5))
    (aset state 5 map) (aset state 6 xkb)
    (unless (eq device (aref state 11))
      (nelisp-gui-xcb-select-keyboard-events state device))
    (aset state 12 (nelisp-gui-xcb-call "xkb_state_serialize_layout" [:uint32 :pointer :uint32] xkb 128))
    (princ "GUI-XKB|refresh=1|\n")))

(defun nelisp-gui-xcb-key (state event)
  "Translate server XKB keysyms and unconsumed modifiers to Emacs events."
  (let* ((start (and (boundp 'nelisp-gui-frontend--timing) nelisp-gui-frontend--timing (float-time)))
         (code (ptr-read-u8 event 1)) (mods (nl-ffi-libffi-u16 event 28))
         (xkb (aref state 6)))
    ;; A refresh reply can describe a later server state than queued keys.
    ;; Decode modifiers from this event, retaining the ordered XKB group.
    (nelisp-gui-xcb--update-mask state event mods)
    (let* ((sym (nelisp-gui-xcb-call "xkb_state_key_get_one_sym" [:uint32 :pointer :uint32] xkb code))
           (group (aref state 12))
           (unicode (nelisp-gui-xcb-call "xkb_keysym_to_utf32" [:uint32 :uint32] sym))
           (consumed (nelisp-gui-xcb-call "xkb_state_key_get_consumed_mods" [:uint32 :pointer :uint32] xkb code))
           (named (cdr (assq sym '((65293 . 13) (65288 . 127) (65289 . 9) (65307 . 27)
                                   (65361 . left) (65363 . right) (65362 . up) (65364 . down)
                                   (65360 . home) (65367 . end) (65365 . prior) (65366 . next)
                                   (65535 . delete) (65421 . kp-enter)))))
           (base (or named (and (>= sym 65470) (<= sym 65504)
                                (intern (format "f%d" (1+ (- sym 65470)))))
                     (and (> unicode 0) unicode)))
           (active nil) ev)
      (when base
        (when (/= 0 (logand mods 4)) (push 'control active))
        (when (and (/= 0 (logand mods 8)) (= 0 (logand consumed 8))) (push 'meta active))
        (when (and (/= 0 (logand mods 64)) (= 0 (logand consumed 64))) (push 'super active))
        (when (and (/= 0 (logand mods 1))
                   (or (symbolp base) (= 0 (logand consumed 1))
                       (and (memq 'control active) (integerp base)
                            (>= base ?A) (<= base ?Z)))) (push 'shift active))
        (setq ev (if active (event-convert-list (append (nreverse active) (list base))) base))
        ;; C-SPC and C punctuation have Emacs's compact control encoding.
        (when (and (integerp base) (memq 'control active) (memq base '(32 64)))
          (setq ev (logand ev (lognot (+ 67108864 255)))))
        (princ (format "GUI-KEY|code=%d|sym=%d|mods=%d|consumed=%d|event=%S|group=%d|\n"
                       code sym mods consumed ev group)))
      (when start (princ (format "GUI-DECODE-TIME|event=%S|seconds=%.6f|\n" ev (- (float-time) start))))
      ev)))

(defun nelisp-gui-xcb--signed16 (p offset)
  (let ((n (nl-ffi-libffi-u16 p offset))) (if (>= n 32768) (- n 65536) n)))

(defun nelisp-gui-xcb-file-descriptor (state)
  "Return the X connection fd used by the frontend's blocking wait."
  (aref state 9))

(defun nelisp-gui-xcb-wait (state timeout-ms)
  "Wait at most TIMEOUT-MS milliseconds, preserving already buffered input.
XCB reply reads can queue events after a frontend's last drain.  Flush
requests and check XCB itself before poll(2); a prefetched event remains
owned by STATE until `nelisp-gui-xcb-poll' consumes it or close frees it.
Return nonzero for pending input or fd readiness, zero for a timeout."
  ;; nl-ffi-memory already requires the Linux x86-64 syscall ABI.  Reuse
  ;; that raw OS seam and one process-local pollfd, avoiding scalar FFI
  ;; resolution and a fresh mmap on every idle iteration.
  (nelisp-gui-xcb-check state)
  (unless (> (aref state 13) 0)
    (nelisp-gui-xcb-call "xcb_flush" [:sint32 :pointer] (aref state 0))
    ;; poll_for_event checks the userspace queue before reading the socket.
    ;; Do not decode here: waiting must not run commands or selection handlers.
    (aset state 13 (nelisp-gui-xcb-call "xcb_poll_for_event" [:pointer :pointer]
                                     (aref state 0))))
  (if (> (aref state 13) 0) 1
    (nelisp-gui-xcb-check state)
    (let ((p (nl-ffi-memory-address (aref state 10))))
      (ptr-write-u32 p 0 (nelisp-gui-xcb-file-descriptor state))
      (ptr-write-u32 p 4 1)
      (syscall-direct 7 p 1 (max 0 (ceiling timeout-ms)) 0 0 0))))

(defun nelisp-gui-xcb-poll (state)
  "Return one transport event, freeing its native event exactly once."
  (nelisp-gui-xcb-check state)
  (let ((p (if (> (aref state 13) 0)
               (prog1 (aref state 13) (aset state 13 0))
             (nelisp-gui-xcb-call "xcb_poll_for_event" [:pointer :pointer] (aref state 0)))))
    (if (= p 0) (progn (nelisp-gui-xcb-check state) nil)
      (unwind-protect
          (let ((type (logand (ptr-read-u8 p 0) 127)))
            (when nelisp-gui-xcb-trace-events
              (princ (format "GUI-XEVENT|type=%d|detail=%d|time=%.6f|\n"
                             type (ptr-read-u8 p 1) (float-time))))
            (cond
             ;; Selection transport cannot consume key press/release events.
             ((= type 2) (list :key (nelisp-gui-xcb-key state p)))
             ((= type 3) '(:ignored t))
             ((and (fboundp 'nelisp-gui-selection-active-p) (nelisp-gui-selection-active-p)
                   (nelisp-gui-selection-event type p)) '(:ignored t))
             ((= type 0) (signal 'nelisp-gui-xcb-error
                                 (list 'asynchronous (ptr-read-u8 p 1) (nl-ffi-libffi-u32 p 4))))
             ((= type (aref state 8))
              (let ((subtype (ptr-read-u8 p 1)))
                (when (and nelisp-gui-xcb-trace-events (= subtype 0))
                  (princ (format "GUI-XKB-NEW|device=%d|old-device=%d|changed=%d|request=%d:%d|\n"
                                 (ptr-read-u8 p 8) (ptr-read-u8 p 9)
                                 (nl-ffi-libffi-u16 p 16) (ptr-read-u8 p 14) (ptr-read-u8 p 15))))
                (cond ((memq subtype '(0 1)) (nelisp-gui-xcb-refresh-keymap state))
                      ((= subtype 2) (nelisp-gui-xcb--update-mask state p)))
                (princ (format "GUI-XKB|notify=%d|\n" subtype)))
              '(:ignored t))
             ((memq type '(4 5 6))
              (list :pointer (list type (ptr-read-u8 p 1)
                                   (nelisp-gui-xcb--signed16 p 24) (nelisp-gui-xcb--signed16 p 26)
                                   (nl-ffi-libffi-u32 p 4) (nl-ffi-libffi-u16 p 28))))
             ((memq type '(9 10))
              (when (= type 9) (nelisp-gui-xcb-refresh-keymap state))
              (list :focus (if (= type 9) 'focus-in 'focus-out)))
             ((= type 34) (nelisp-gui-xcb-refresh-keymap state) '(:ignored t))
             ((= type 22)
              (list :resize (cons (nl-ffi-libffi-u16 p 20) (nl-ffi-libffi-u16 p 22))))
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
  (when (> (aref state 13) 0)
    (nelisp-gui-xcb-call "free" [:void :pointer] (aref state 13))
    (aset state 13 0))
  (when nelisp-gui-xcb--mask-frame
    (nl-ffi-memory-release (aref nelisp-gui-xcb--mask-frame 1))
    (setq nelisp-gui-xcb--mask-frame nil))
  (when (aref state 10)
    (nl-ffi-memory-release (aref state 10))
    (aset state 10 nil))
  (dolist (entry '((6 . "xkb_state_unref") (5 . "xkb_keymap_unref") (4 . "xkb_context_unref")))
    (when (> (aref state (car entry)) 0)
      (nelisp-gui-xcb-call (cdr entry) [:void :pointer] (aref state (car entry)))
      (aset state (car entry) 0)))
  (when (> (aref state 0) 0)
    (nelisp-gui-xcb-call "xcb_disconnect" [:void :pointer] (aref state 0))
    (aset state 0 0))
  (aset state 7 nil)
  (clrhash nelisp-gui-xcb--calls))

(provide 'nelisp-gui-xcb)
