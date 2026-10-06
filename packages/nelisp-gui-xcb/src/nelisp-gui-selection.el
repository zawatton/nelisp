;;; nelisp-gui-selection.el --- Bounded ICCCM X selection transport -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-gui-xcb)
(require 'emacs-select)
(defvar nelisp-gui-selection--state nil)
(defvar nelisp-gui-selection--atoms nil)
(defvar nelisp-gui-selection--owners nil)
(defvar nelisp-gui-selection--sends nil)
(defvar nelisp-gui-selection--receive nil)
(defvar nelisp-gui-selection--time 0)
(defvar nelisp-gui-selection--clock nil)
(defvar nelisp-gui-selection--events nil)
(defvar nelisp-gui-selection--peer-cookies nil)
(defvar nelisp-gui-selection-max-bytes 4194304)
(defvar nelisp-gui-selection-timeout 3.0)
(defconst nelisp-gui-selection--chunk 32768)
(defconst nelisp-gui-selection--targets '(TARGETS TIMESTAMP UTF8_STRING STRING TEXT))

(defun nelisp-gui-selection--request (name types &rest args)
  (let ((seq (apply #'nelisp-gui-xcb-cookie nelisp-gui-selection--state name types args)))
    ;; Retain a bounded attribution ring for asynchronous BadWindow after a
    ;; foreign requestor disappears between an event and our property write.
    (when (member name '("xcb_change_property" "xcb_change_window_attributes"
                        "xcb_send_event" "xcb_destroy_window" "xcb_get_property"))
      (let ((window (if (member name '("xcb_change_property" "xcb_send_event"))
                        (nth 1 args) (car args))))
        (unless (= window (aref nelisp-gui-selection--state 1))
          (push (cons seq window) nelisp-gui-selection--peer-cookies)
          (when (> (length nelisp-gui-selection--peer-cookies) 128)
            (setcdr (nthcdr 127 nelisp-gui-selection--peer-cookies) nil))))) seq))
(defun nelisp-gui-selection--flush ()
  (nelisp-gui-xcb-call "xcb_flush" [:sint32 :pointer] (aref nelisp-gui-selection--state 0)))
(defun nelisp-gui-selection--reply (sequence)
  "Poll a bounded reply. Caller owns its malloc pointer."
  (let ((o (nl-ffi-memory-allocate 16)) (done nil)
        (deadline (if nelisp-gui-selection--receive
                      (min (aref nelisp-gui-selection--receive 6)
                           (aref nelisp-gui-selection--receive 7)
                           (+ (float-time) nelisp-gui-selection-timeout))
                    (+ (float-time) nelisp-gui-selection-timeout))))
    (unwind-protect
        (let ((p (nl-ffi-memory-address o)))
          (nelisp-gui-selection--flush)
          ;; Observe an already available reply before consulting the clock.
          ;; A descheduled consumer must still validate its property metadata,
          ;; including the running byte cap, before reporting a reply timeout.
          (while (not done)
            (nelisp-gui-xcb-check nelisp-gui-selection--state)
            (setq done (= 1 (nelisp-gui-xcb-call
                             "xcb_poll_for_reply" [:sint32 :pointer :uint32 :pointer :pointer]
                             (aref nelisp-gui-selection--state 0) sequence p (+ p 8))))
            (unless done
              (when (>= (float-time) deadline)
                (nelisp-gui-xcb-call "xcb_discard_reply" [:void :pointer :uint32]
                                     (aref nelisp-gui-selection--state 0) sequence)
                (error "Selection reply timeout"))
              (sleep-for 0.002)))
          (let ((r (ptr-read-u64 p 0)) (e (ptr-read-u64 p 8)))
            (when (> e 0)
              (nelisp-gui-xcb-call "free" [:void :pointer] e)
              (when (> r 0) (nelisp-gui-xcb-call "free" [:void :pointer] r))
              (error "Selection X error"))
            (unless (> r 0) (error "Selection missing reply")) r))
      (nl-ffi-memory-release o))))
(defun nelisp-gui-selection--atom (name)
  (or (cdr (assq name nelisp-gui-selection--atoms))
      (let* ((text (symbol-name name)) (o (nl-ffi-memory-cstring text)) (r nil))
        (unwind-protect
            (progn
              (setq r (nelisp-gui-selection--reply
                       (nelisp-gui-selection--request "xcb_intern_atom"
                        '(:uint8 :uint16 :pointer) 0 (length text) (nl-ffi-memory-address o))))
              (let ((atom (nl-ffi-libffi-u32 r 8)))
                (push (cons name atom) nelisp-gui-selection--atoms) atom))
          (when r (nelisp-gui-xcb-call "free" [:void :pointer] r))
          (nl-ffi-memory-release o)))))
(defun nelisp-gui-selection--symbol (atom)
  (car (rassq atom nelisp-gui-selection--atoms)))
(defun nelisp-gui-selection-active-p ()
  "Return non-nil when this transport is attached to a live frontend."
  (and nelisp-gui-selection--state t))
(defun nelisp-gui-selection-poll ()
  "Return deferred input first, otherwise poll the XCB transport."
  (if nelisp-gui-selection--events (pop nelisp-gui-selection--events)
    (nelisp-gui-xcb-poll nelisp-gui-selection--state)))
(defun nelisp-gui-selection--pump ()
  ;; Preserve nonselection events while synchronous ConvertSelection waits.
  ;; They are dispatched by the ordinary shared command loop after return.
  (let ((event (nelisp-gui-xcb-poll nelisp-gui-selection--state)))
    (when (and event (not (plist-get event :ignored)))
      (setq nelisp-gui-selection--events
            (nconc nelisp-gui-selection--events (list event))))
    event))
(defun nelisp-gui-selection--wait-ready (deadline)
  "Process queued events before expiring an otherwise idle wait.
Return non-nil while progress or time remains. Selection events validate
their byte cap synchronously in the pump, even after a scheduling pause."
  (or (nelisp-gui-selection--pump)
      (when (< (float-time) deadline)
        (sleep-for 0.002) t)))
(defun nelisp-gui-selection--timestamp ()
  "Obtain a fresh server timestamp via PropertyNotify, never use wall time."
  (setq nelisp-gui-selection--clock nil)
  (let ((o (nl-ffi-memory-allocate 4))
        (deadline (+ (float-time) nelisp-gui-selection-timeout)))
    (unwind-protect
        (progn
          (nelisp-gui-selection--property (aref nelisp-gui-selection--state 1)
            (nelisp-gui-selection--atom '_NELISP_TIME) (nelisp-gui-selection--atom 'INTEGER)
            32 1 (nl-ffi-memory-address o))
          (nelisp-gui-selection--flush)
          (while (and (not nelisp-gui-selection--clock)
                      (nelisp-gui-selection--wait-ready deadline)))
          (or nelisp-gui-selection--clock (error "Selection timestamp timeout")))
      (nl-ffi-memory-release o))))
(defun nelisp-gui-selection--property (window property type format count pointer)
  (nelisp-gui-selection--request "xcb_change_property"
   '(:uint8 :uint32 :uint32 :uint32 :uint8 :uint32 :pointer)
   0 window property type format count pointer))
(defun nelisp-gui-selection--delete (window property)
  (nelisp-gui-selection--request "xcb_delete_property" '(:uint32 :uint32) window property))
(defun nelisp-gui-selection--owner (type)
  (let ((r (nelisp-gui-selection--reply
            (nelisp-gui-selection--request "xcb_get_selection_owner" '(:uint32)
             (nelisp-gui-selection--atom type)))))
    (unwind-protect (nl-ffi-libffi-u32 r 8)
      (nelisp-gui-xcb-call "free" [:void :pointer] r))))
(defun nelisp-gui-selection-owner-p (type)
  (= (nelisp-gui-selection--owner type) (aref nelisp-gui-selection--state 1)))
(defun nelisp-gui-selection-exists-p (type)
  (> (nelisp-gui-selection--owner type) 0))
(defun nelisp-gui-selection-set (type text)
  (let ((cell (assq type nelisp-gui-selection--owners)))
    (when (or text (nelisp-gui-selection-owner-p type))
      (when (and text (> (string-bytes (encode-coding-string text 'utf-8-unix))
                         nelisp-gui-selection-max-bytes))
        (error "Selection transfer cap"))
      (let ((time (nelisp-gui-selection--timestamp)))
        (nelisp-gui-selection--request "xcb_set_selection_owner" '(:uint32 :uint32 :uint32)
         (if text (aref nelisp-gui-selection--state 1) 0) (nelisp-gui-selection--atom type) time)
        (nelisp-gui-selection--flush)
        (when (and text (not (nelisp-gui-selection-owner-p type)))
          (error "Selection ownership rejected"))
        (if cell (setcdr cell (and text (vector text time)))
          (when text (push (cons type (vector text time)) nelisp-gui-selection--owners)))
        (princ (format "GUI-SELECTION|set=%S|bytes=%d|time=%d|\n" type
                        (if text (string-bytes text) 0) time))))))
(defun nelisp-gui-selection--notify (event property)
  (let ((o (nl-ffi-memory-allocate 32)))
    (unwind-protect
        (let ((p (nl-ffi-memory-address o)))
          (ptr-write-u8 p 0 31)
          (dolist (offset '(4 12 16 20)) (ptr-write-u32 p offset (nl-ffi-libffi-u32 event offset)))
          (ptr-write-u32 p 8 (nl-ffi-libffi-u32 event 12))
          (ptr-write-u32 p 12 (nl-ffi-libffi-u32 event 16))
          (ptr-write-u32 p 16 (nl-ffi-libffi-u32 event 20))
          (ptr-write-u32 p 20 property)
          (nelisp-gui-selection--request "xcb_send_event" '(:uint8 :uint32 :uint32 :pointer)
           0 (nl-ffi-libffi-u32 event 12) 0 p))
      (nl-ffi-memory-release o))))
(defun nelisp-gui-selection--send-drop (send reason)
  (nl-ffi-memory-release (aref send 4))
  (setq nelisp-gui-selection--sends (delq send nelisp-gui-selection--sends))
  (unless (or (memq reason '(close owner-death))
              (catch 'another
                (dolist (s nelisp-gui-selection--sends)
                  (when (= (aref s 0) (aref send 0)) (throw 'another t)))))
    ;; Stop watching a surviving peer when its final transfer ends.
    (let ((o (nl-ffi-memory-allocate 4)))
      (unwind-protect
          (nelisp-gui-selection--request "xcb_change_window_attributes"
           '(:uint32 :uint32 :pointer) (aref send 0) 2048 (nl-ffi-memory-address o))
        (nl-ffi-memory-release o)))
    (nelisp-gui-selection--flush))
  (princ (format "GUI-SELECTION|send-end=%S|bytes=%d|\n" reason (aref send 5))))
(defun nelisp-gui-selection-expire ()
  (dolist (send (copy-sequence nelisp-gui-selection--sends))
    (when (> (float-time) (aref send 6))
      (nelisp-gui-selection--send-drop send 'timeout))))
(defun nelisp-gui-selection-next-delay (maximum)
  "Cap MAXIMUM by the nearest asynchronous outgoing transfer deadline.
The send deadline already includes its total lifetime cap.  Incoming
transfers use their own synchronous bounded wait, rather than this idle wait."
  (if (null nelisp-gui-selection--sends) maximum
    (let ((now (float-time)) (delay maximum))
      (dolist (send nelisp-gui-selection--sends)
        (setq delay (min delay (max 0.0 (- (aref send 6) now)))))
      delay)))
(defun nelisp-gui-selection--serve (event)
  (let* ((window (nl-ffi-libffi-u32 event 12))
         (selection (nelisp-gui-selection--symbol (nl-ffi-libffi-u32 event 16)))
         (target (nelisp-gui-selection--symbol (nl-ffi-libffi-u32 event 20)))
         (property (nl-ffi-libffi-u32 event 24))
         (value (cdr (assq selection nelisp-gui-selection--owners)))
         (time (nl-ffi-libffi-u32 event 4))
         (accepted nil) (o nil) (retained nil))
    ;; ICCCM obsolete clients may pass None: use the target atom as property.
    (when (= property 0) (setq property (nl-ffi-libffi-u32 event 20)))
    (unwind-protect
        (catch 'selection-refused
          (when (and value (memq target nelisp-gui-selection--targets)
                   (or (= time 0) (< (logand (- time (aref value 1)) 4294967295) 2147483648))
                   (< (length nelisp-gui-selection--sends) 8)
                   (not (catch 'duplicate
                          (dolist (s nelisp-gui-selection--sends)
                            (when (and (= window (aref s 0)) (= property (aref s 1)))
                              (throw 'duplicate t))))))
          (let ((format 8) (type target) (count 0))
            (cond
             ((eq target 'TARGETS)
              (setq format 32 type 'ATOM count (length nelisp-gui-selection--targets)
                    o (nl-ffi-memory-allocate (* count 4)))
              (let ((i 0)) (dolist (name nelisp-gui-selection--targets)
                            (ptr-write-u32 (nl-ffi-memory-address o) (* i 4)
                                           (nelisp-gui-selection--atom name)) (setq i (1+ i)))))
             ((eq target 'TIMESTAMP)
              (setq format 32 type 'INTEGER count 1 o (nl-ffi-memory-allocate 4))
              (ptr-write-u32 (nl-ffi-memory-address o) 0 (aref value 1)))
             (t
              (setq type (if (eq target 'STRING) 'STRING 'UTF8_STRING))
              (let ((bytes (encode-coding-string (aref value 0)
                           (if (eq type 'STRING) 'iso-latin-1-unix 'utf-8-unix))))
                (setq count (length bytes))
                ;; Revalidate when converting: the configured cap can change
                ;; after ownership (and other runtimes allow string growth).
                (when (> count nelisp-gui-selection-max-bytes)
                  (princ "GUI-SELECTION|send-rejected=cap|\n")
                  (throw 'selection-refused nil))
                (setq o (nl-ffi-memory-allocate count))
                (ptr-write-bytes (nl-ffi-memory-address o) bytes))))
            (if (<= count nelisp-gui-selection--chunk)
                (nelisp-gui-selection--property window property (nelisp-gui-selection--atom type)
                                                format count (nl-ffi-memory-address o))
              ;; Subscribe to deletion and death on the peer's window. XCB still
              ;; owns framing; no sockets or callbacks are borrowed here.
              (let ((params (nl-ffi-memory-allocate 4)))
                (unwind-protect
                    (progn
                      (ptr-write-u32 (nl-ffi-memory-address params) 0 (+ 4194304 131072))
                      (nelisp-gui-selection--request "xcb_change_window_attributes"
                       '(:uint32 :uint32 :pointer) window 2048 (nl-ffi-memory-address params))
                      (ptr-write-u32 (nl-ffi-memory-address params) 0 count)
                      (nelisp-gui-selection--property window property (nelisp-gui-selection--atom 'INCR)
                       32 1 (nl-ffi-memory-address params)))
                  (nl-ffi-memory-release params)))
              ;; window/property/type/count/native-owner/offset/idle-deadline/total-deadline
              (push (vector window property (nelisp-gui-selection--atom type) count o 0
                            (+ (float-time) nelisp-gui-selection-timeout) (+ (float-time) 15.0))
                    nelisp-gui-selection--sends)
              (setq retained t)
              (princ (format "GUI-SELECTION|incr-send=%S|bytes=%d|\n" selection count)))
            (setq accepted t))))
      (when (and o (not retained)) (nl-ffi-memory-release o)))
    (nelisp-gui-selection--notify event (if accepted property 0))
    (nelisp-gui-selection--flush)))
(defun nelisp-gui-selection--read-property (window property)
  "Delete/read a complete bounded property and return (TYPE FORMAT BYTES)."
  (let ((r (nelisp-gui-selection--reply
            (nelisp-gui-selection--request "xcb_get_property"
             '(:uint8 :uint32 :uint32 :uint32 :uint32 :uint32)
             1 window property 0 0 (1+ (/ nelisp-gui-selection-max-bytes 4))))))
    (unwind-protect
        (let* ((format (ptr-read-u8 r 1)) (count (nl-ffi-libffi-u32 r 16))
               (size (* count (/ format 8))) (type (nl-ffi-libffi-u32 r 8)))
          (unless (and (memq format '(0 8 16 32)) (<= size nelisp-gui-selection-max-bytes)
                       (= 0 (nl-ffi-libffi-u32 r 12))
                       (<= size (* 4 (nl-ffi-libffi-u32 r 4))))
            (error "Selection transfer cap or malformed property"))
          ;; Check the running INCR cap from the reply header, before copying
          ;; a potentially multi-megabyte native payload into Lisp.  The header
          ;; is enough to reject an overrun even if copying would exhaust the
          ;; idle or total deadline on a slow/descheduled machine.
          (when (and nelisp-gui-selection--receive
                     (eq (aref nelisp-gui-selection--receive 3) 'incr)
                     (> (+ (aref nelisp-gui-selection--receive 5) size)
                        nelisp-gui-selection-max-bytes))
            (error "Selection INCR transfer cap"))
          (list type format (ptr-read-bytes (+ r 32) size)))
      (nelisp-gui-xcb-call "free" [:void :pointer] r))))
(defun nelisp-gui-selection--receive-property ()
  (let* ((r nelisp-gui-selection--receive)
         (prop (nelisp-gui-selection--read-property (aref r 0) (aref r 1)))
         (type (nth 0 prop)) (format (nth 1 prop)) (bytes (nth 2 prop)))
    (cond
     ((and (eq (aref r 3) 'notify) (= type (nelisp-gui-selection--atom 'INCR)))
      ;; xclip sends an empty INTEGER/32 INCR announcement. Treat it as an
      ;; unknown lower bound (zero); the running cap still applies to all bytes.
      (unless (and (= format 32) (memq (length bytes) '(0 4))
                   (or (= (length bytes) 0)
                       (<= (nelisp-gui-xcb-bytes-number bytes 0 4) nelisp-gui-selection-max-bytes)))
        (error "Selection INCR cap or malformed announcement"))
      (aset r 3 'incr)
      (princ (format "GUI-SELECTION|incr-receive=1|announced=%d|\n"
                      (if (= (length bytes) 0) 0 (nelisp-gui-xcb-bytes-number bytes 0 4)))))
     (t
      (unless (and (> type 0) (or (= format 8) (= format 32))
                   (or (= (aref r 2) type)
                       (and (= (aref r 2) (nelisp-gui-selection--atom 'TEXT))
                            (memq (nelisp-gui-selection--symbol type) '(UTF8_STRING STRING)))
                       (and (= (aref r 2) (nelisp-gui-selection--atom 'TIMESTAMP))
                            (= type (nelisp-gui-selection--atom 'INTEGER)))
                       (and (= (aref r 2) (nelisp-gui-selection--atom 'TARGETS))
                            (= type (nelisp-gui-selection--atom 'ATOM))))
                   (if (memq (nelisp-gui-selection--symbol type) '(ATOM INTEGER))
                       (= format 32) (= format 8))
                   (or (null (aref r 8)) (equal (aref r 8) (list type format))))
        (error "Selection property type mismatch: expected=%d actual=%d format=%d size=%d" (aref r 2) type format (length bytes)))
      (aset r 8 (list type format))
      (aset r 5 (+ (aref r 5) (length bytes)))
      (when (> (aref r 5) nelisp-gui-selection-max-bytes) (error "Selection INCR transfer cap"))
      (when (> (length bytes) 0) (aset r 4 (cons bytes (aref r 4))))
      (when (or (eq (aref r 3) 'notify) (= (length bytes) 0)) (aset r 3 'done))))
    (aset r 6 (+ (float-time) nelisp-gui-selection-timeout))
    (nelisp-gui-selection--flush)))
(defun nelisp-gui-selection-event (type event)
  "Handle raw selection events before the native event is freed; return handled."
  (cond
   ((and (= type 0) (= (ptr-read-u8 event 1) 3)
         (equal (cdr (assq (nl-ffi-libffi-u32 event 32) nelisp-gui-selection--peer-cookies))
                (nl-ffi-libffi-u32 event 4)))
    (dolist (send (copy-sequence nelisp-gui-selection--sends))
      (when (= (aref send 0) (nl-ffi-libffi-u32 event 4))
        (nelisp-gui-selection--send-drop send 'owner-death)))
    (when (and nelisp-gui-selection--receive
               (= (aref nelisp-gui-selection--receive 0) (nl-ffi-libffi-u32 event 4)))
      (aset nelisp-gui-selection--receive 3 'refused))
    (princ "GUI-SELECTION|peer-BadWindow=1|\n") t)
   ((= type 29)
    (let ((cell (assq (nelisp-gui-selection--symbol (nl-ffi-libffi-u32 event 12))
                     nelisp-gui-selection--owners)))
      (when (and cell (cdr cell)
                 (< (logand (- (nl-ffi-libffi-u32 event 4) (aref (cdr cell) 1)) 4294967295)
                    2147483648))
        (setcdr cell nil)))
    (princ "GUI-SELECTION|clear=1|\n") t)
   ((= type 30) (nelisp-gui-selection--serve event) t)
   ((= type 31)
    (let ((r nelisp-gui-selection--receive))
      (when (and r (eq (aref r 3) 'notify) (= (aref r 0) (nl-ffi-libffi-u32 event 8))
                 (= (aref r 2) (nl-ffi-libffi-u32 event 16))
                 (= (aref r 9) (nl-ffi-libffi-u32 event 12)))
        (if (= (nl-ffi-libffi-u32 event 20) 0) (aset r 3 'refused)
          (when (= (aref r 1) (nl-ffi-libffi-u32 event 20))
            (nelisp-gui-selection--receive-property))))) t)
   ((= type 28)
    (let ((window (nl-ffi-libffi-u32 event 4)) (property (nl-ffi-libffi-u32 event 8))
          (r nelisp-gui-selection--receive) (deleted (= (ptr-read-u8 event 16) 1)))
      (setq nelisp-gui-selection--time (nl-ffi-libffi-u32 event 12))
      (when (and (= window (aref nelisp-gui-selection--state 1))
                 (= property (nelisp-gui-selection--atom '_NELISP_TIME)) (not deleted))
        (setq nelisp-gui-selection--clock nelisp-gui-selection--time))
      (when (and r (eq (aref r 3) 'incr) (not deleted)
                 (= window (aref r 0)) (= property (aref r 1)))
        (nelisp-gui-selection--receive-property))
      (when deleted
        (dolist (send (copy-sequence nelisp-gui-selection--sends))
          (when (and (= window (aref send 0)) (= property (aref send 1)))
            (let ((n (min nelisp-gui-selection--chunk (- (aref send 3) (aref send 5)))))
              (nelisp-gui-selection--property window property (aref send 2) 8 n
               (+ (nl-ffi-memory-address (aref send 4)) (aref send 5)))
              (aset send 5 (+ (aref send 5) n))
              (aset send 6 (min (aref send 7) (+ (float-time) nelisp-gui-selection-timeout)))
              (when (= n 0) (nelisp-gui-selection--send-drop send 'complete))
              (nelisp-gui-selection--flush)))))) t)
   ((and (= type 17) (/= (nl-ffi-libffi-u32 event 8) (aref nelisp-gui-selection--state 1)))
    (dolist (send (copy-sequence nelisp-gui-selection--sends))
      (when (= (nl-ffi-libffi-u32 event 8) (aref send 0))
        (nelisp-gui-selection--send-drop send 'owner-death))) t)))
(defun nelisp-gui-selection-get (selection target)
  "ConvertSelection on a fresh requestor window with idle/total deadlines."
  (let ((local (cdr (assq selection nelisp-gui-selection--owners))))
    (if (and local (nelisp-gui-selection-owner-p selection))
        (cond ((eq target 'TIMESTAMP) (aref local 1))
              ((eq target 'TARGETS) (vconcat nelisp-gui-selection--targets))
              ((memq target '(UTF8_STRING TEXT)) (aref local 0))
              ((eq target 'STRING) (decode-coding-string
                 (encode-coding-string (aref local 0) 'iso-latin-1-unix) 'iso-latin-1-unix)))
      (when (and (not nelisp-gui-selection--receive) (nelisp-gui-selection-exists-p selection))
        (let* ((state nelisp-gui-selection--state)
               (window (nelisp-gui-xcb-call "xcb_generate_id" [:uint32 :pointer] (aref state 0)))
               (o (nl-ffi-memory-allocate 4)) (result nil))
          (unwind-protect
              (condition-case err
                  (progn
                    (ptr-write-u32 (nl-ffi-memory-address o) 0 4194304)
                    (nelisp-gui-selection--request "xcb_create_window"
                     '(:uint8 :uint32 :uint32 :sint16 :sint16 :uint16 :uint16 :uint16 :uint16 :uint32 :uint32 :pointer)
                     0 window (nl-ffi-libffi-u32 (aref state 3) 0) 0 0 1 1 0 2 0 2048 (nl-ffi-memory-address o))
                    ;; window/property/target/status/chunks/count/idle/total/type/selection
                    (setq nelisp-gui-selection--receive
                          (vector window (nelisp-gui-selection--atom '_NELISP_SELECTION)
                                  (nelisp-gui-selection--atom target) 'notify nil 0
                                  (+ (float-time) nelisp-gui-selection-timeout) (+ (float-time) 15.0) nil
                                  (nelisp-gui-selection--atom selection)))
                    (nelisp-gui-selection--request "xcb_convert_selection" '(:uint32 :uint32 :uint32 :uint32 :uint32)
                     window (aref nelisp-gui-selection--receive 9) (aref nelisp-gui-selection--receive 2)
                     (aref nelisp-gui-selection--receive 1) (nelisp-gui-selection--timestamp))
                    ;; Timestamp acquisition has its own deadline. Only start
                    ;; transfer clocks once ConvertSelection has been issued.
                    (aset nelisp-gui-selection--receive 6 (+ (float-time) nelisp-gui-selection-timeout))
                    (aset nelisp-gui-selection--receive 7 (+ (float-time) 15.0))
                    (nelisp-gui-selection--flush)
                    (let ((r nelisp-gui-selection--receive))
                      (while (and (memq (aref r 3) '(notify incr))
                                  (nelisp-gui-selection--wait-ready
                                   (min (aref r 6) (aref r 7)))))
                      (if (eq (aref r 3) 'done)
                          (let ((bytes (apply #'concat (nreverse (aref r 4)))))
                            (setq result
                             (cond
                              ((eq target 'TIMESTAMP)
                               (unless (= (length bytes) 4) (error "Selection invalid timestamp"))
                               (nelisp-gui-xcb-bytes-number bytes 0 4))
                              ((eq target 'TARGETS)
                               (unless (= (% (length bytes) 4) 0) (error "Selection invalid targets"))
                               (let ((v (make-vector (/ (length bytes) 4) nil)))
                                 (dotimes (i (length v))
                                   (aset v i (or (nelisp-gui-selection--symbol
                                      (nelisp-gui-xcb-bytes-number bytes (* i 4) 4))
                                      (nelisp-gui-xcb-bytes-number bytes (* i 4) 4)))) v))
                              (t (decode-coding-string bytes
                                  (if (= (car (aref r 8)) (nelisp-gui-selection--atom 'STRING))
                                      'iso-latin-1-unix 'utf-8-unix))))))
                        (princ (format "GUI-SELECTION|receive-end=%S|bytes=%d|\n"
                                        (if (eq (aref r 3) 'refused) 'refused 'timeout) (aref r 5))))))
                (error (princ (format "GUI-SELECTION|receive-error=%S|\n" err))))
            (setq nelisp-gui-selection--receive nil)
            (nelisp-gui-selection--request "xcb_destroy_window" '(:uint32) window)
            (nelisp-gui-selection--flush)
            (nl-ffi-memory-release o)) result)))))
(defun nelisp-gui-selection-open (state)
  (setq nelisp-gui-selection--state state nelisp-gui-selection--atoms nil
        nelisp-gui-selection--owners nil nelisp-gui-selection--sends nil
        nelisp-gui-selection--events nil nelisp-gui-selection--receive nil
        nelisp-gui-selection--peer-cookies nil)
  (dolist (name '(PRIMARY SECONDARY CLIPBOARD UTF8_STRING STRING TEXT TARGETS TIMESTAMP
                         INTEGER ATOM INCR _NELISP_TIME _NELISP_SELECTION))
    (nelisp-gui-selection--atom name))
  (emacs-select-install (list :set #'nelisp-gui-selection-set :get #'nelisp-gui-selection-get
                             :owner #'nelisp-gui-selection-owner-p :exists #'nelisp-gui-selection-exists-p)))
(defun nelisp-gui-selection-close ()
  (dolist (send (copy-sequence nelisp-gui-selection--sends))
    (nelisp-gui-selection--send-drop send 'close))
  (setq emacs-select-backend nil nelisp-gui-selection--state nil
        nelisp-gui-selection--owners nil nelisp-gui-selection--events nil
        nelisp-gui-selection--receive nil nelisp-gui-selection--peer-cookies nil
        interprogram-cut-function nil interprogram-paste-function nil))
(provide 'nelisp-gui-selection)
