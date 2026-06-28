;;; nelisp-x11.el --- a pure-elisp X11 client for the standalone -*- lexical-binding: t; -*-
;;
;; Speaks the X11 core protocol directly over the display's Unix-domain socket
;; (/tmp/.X11-unix/X<n>) using only `syscall-direct' (socket=41, connect=42,
;; read=0, write=1, poll=7) and the `alloc-bytes' / `ptr-*' memory ops.  No C,
;; no Xlib -- the standalone draws into a real X window.
;;
;; Stage 1 (this file, initial): connect + the connection-setup handshake, with
;; enough of the reply parsed (resource-id base/mask, root window + visual,
;; black/white pixels, depth, keycode range) to create windows and a GC next.
;;
;; A "display" is a vector:
;;   [FD ID-BASE ID-MASK ID-NEXT ROOT VISUAL WHITE BLACK DEPTH MIN-KC MAX-KC]

(defconst nelisp-x11--d-fd 0)
(defconst nelisp-x11--d-idbase 1)
(defconst nelisp-x11--d-idmask 2)
(defconst nelisp-x11--d-idnext 3)
(defconst nelisp-x11--d-root 4)
(defconst nelisp-x11--d-visual 5)
(defconst nelisp-x11--d-white 6)
(defconst nelisp-x11--d-black 7)
(defconst nelisp-x11--d-depth 8)
(defconst nelisp-x11--d-minkc 9)
(defconst nelisp-x11--d-maxkc 10)

;;; Little-endian buffer accessors ------------------------------------

(defun nelisp-x11--u16 (buf off)
  (+ (ptr-read-u8 buf off) (* (ptr-read-u8 buf (+ off 1)) 256)))

(defun nelisp-x11--u32 (buf off)
  (+ (ptr-read-u8 buf off)
     (* (ptr-read-u8 buf (+ off 1)) 256)
     (* (ptr-read-u8 buf (+ off 2)) 65536)
     (* (ptr-read-u8 buf (+ off 3)) 16777216)))

(defun nelisp-x11--put16 (buf off v)
  (ptr-write-u8 buf off (logand v 255))
  (ptr-write-u8 buf (+ off 1) (logand (/ v 256) 255)))

(defun nelisp-x11--put32 (buf off v)
  (ptr-write-u8 buf off (logand v 255))
  (ptr-write-u8 buf (+ off 1) (logand (/ v 256) 255))
  (ptr-write-u8 buf (+ off 2) (logand (/ v 65536) 255))
  (ptr-write-u8 buf (+ off 3) (logand (/ v 16777216) 255)))

(defun nelisp-x11--pad4 (n) (* (/ (+ n 3) 4) 4))

;;; Socket I/O --------------------------------------------------------

(defun nelisp-x11--write (fd buf n)
  "Write N bytes of BUF to FD, looping until all are sent.  Return N or -1."
  (let ((sent 0))
    (while (and (>= sent 0) (< sent n))
      (let ((w (syscall-direct 1 fd (+ buf sent) (- n sent) 0 0 0)))
        (if (<= w 0) (setq sent -1) (setq sent (+ sent w)))))
    sent))

(defun nelisp-x11--read-n (fd buf n)
  "Read exactly N bytes from FD into BUF, looping.  Return N or -1."
  (let ((got 0))
    (while (and (>= got 0) (< got n))
      (let ((r (syscall-direct 0 fd (+ buf got) (- n got) 0 0 0)))
        (if (<= r 0) (setq got -1) (setq got (+ got r)))))
    got))

;;; Connect + handshake ----------------------------------------------

(defun nelisp-x11--connect-socket (display-num)
  "Open + connect a Unix socket to /tmp/.X11-unix/X<DISPLAY-NUM>.  Return fd or -1."
  (let ((fd (syscall-direct 41 1 1 0 0 0 0)))   ; socket(AF_UNIX, SOCK_STREAM, 0)
    (if (< fd 0) -1
      (let* ((path (concat "/tmp/.X11-unix/X" (number-to-string display-num)))
             (n (length path))
             (sa (alloc-bytes 128 8)) (i 0))
        (ptr-write-u8 sa 0 1) (ptr-write-u8 sa 1 0)   ; sun_family = AF_UNIX
        (while (< i n) (ptr-write-u8 sa (+ 2 i) (aref path i)) (setq i (1+ i)))
        (ptr-write-u8 sa (+ 2 n) 0)
        (if (< (syscall-direct 42 fd sa (+ 3 n) 0 0 0) 0) -1 fd)))))

(defun nelisp-x11-connect (&optional display-num)
  "Connect to the X server (default display 0) and run the setup handshake.
Return a display vector, or a (error . REASON) cons on failure."
  (let ((fd (nelisp-x11--connect-socket (or display-num 0))))
    (if (< fd 0) (cons 'error "connect failed")
      ;; Connection setup request (little-endian, empty authorization).
      (let ((req (alloc-bytes 12 8)))
        (ptr-write-u8 req 0 108)            ; 'l' little-endian
        (nelisp-x11--put16 req 2 11)        ; protocol-major 11
        (nelisp-x11--put16 req 4 0)         ; protocol-minor 0
        (nelisp-x11--put16 req 6 0)         ; auth-name length
        (nelisp-x11--put16 req 8 0)         ; auth-data length
        (nelisp-x11--write fd req 12)
        ;; Read the 8-byte prefix, then the rest.
        (let ((hdr (alloc-bytes 8 8)))
          (if (< (nelisp-x11--read-n fd hdr 8) 0) (cons 'error "no reply")
            (let ((status (ptr-read-u8 hdr 0)))
              (if (/= status 1) (cons 'error (list 'setup-status status))
                (let* ((addlen (* (nelisp-x11--u16 hdr 6) 4))
                       (buf (alloc-bytes (+ addlen 8) 8)))
                  (if (< (nelisp-x11--read-n fd buf addlen) 0)
                      (cons 'error "short setup")
                    (nelisp-x11--parse-setup fd buf)))))))))))

(defun nelisp-x11--parse-setup (fd buf)
  "Parse the post-prefix setup BUF (offsets relative to byte 8 of the reply)."
  ;; Header fields (relative to the reply byte 8, i.e. buf+0 == reply+8):
  ;;  +0 release, +4 id-base, +8 id-mask, +12 motion-buf, +16 vendor-len(u16),
  ;;  +18 max-req(u16), +20 num-screens(u8), +21 num-formats(u8),
  ;;  +24 min-keycode(u8), +25 max-keycode(u8).  Then vendor (pad4),
  ;;  then 8*num-formats, then SCREEN 0.
  (let* ((idbase (nelisp-x11--u32 buf 4))
         (idmask (nelisp-x11--u32 buf 8))
         (vendor-len (nelisp-x11--u16 buf 16))
         (nformats (ptr-read-u8 buf 21))
         (minkc (ptr-read-u8 buf 26))
         (maxkc (ptr-read-u8 buf 27))
         ;; header is 32 bytes (buf+0..31), then vendor (pad4), then formats.
         (screen0 (+ 32 (nelisp-x11--pad4 vendor-len) (* 8 nformats)))
         ;; SCREEN: +0 root, +8 white, +12 black, +32 root-visual, +38 depth(u8)
         (root   (nelisp-x11--u32 buf screen0))
         (white  (nelisp-x11--u32 buf (+ screen0 8)))
         (black  (nelisp-x11--u32 buf (+ screen0 12)))
         (visual (nelisp-x11--u32 buf (+ screen0 32)))
         (depth  (ptr-read-u8 buf (+ screen0 38))))
    (vector fd idbase idmask 0 root visual white black depth minkc maxkc)))

;;; Resource IDs ------------------------------------------------------

(defun nelisp-x11-gen-id (dpy)
  "Allocate a fresh resource ID on DPY (idbase | n; idmask is contiguous)."
  (let ((next (1+ (aref dpy nelisp-x11--d-idnext))))
    (aset dpy nelisp-x11--d-idnext next)
    (logior (aref dpy nelisp-x11--d-idbase)
            (logand next (aref dpy nelisp-x11--d-idmask)))))

(defun nelisp-x11-close (dpy)
  "Close DPY's socket."
  (syscall-direct 3 (aref dpy nelisp-x11--d-fd) 0 0 0 0 0))

;;; Core requests -----------------------------------------------------

(defun nelisp-x11-create-window (dpy x y w h)
  "CreateWindow as a child of the root, inheriting depth/visual/colormap.
The window selects Exposure + KeyPress events.  Return the new window id."
  (let* ((fd (aref dpy nelisp-x11--d-fd))
         (wid (nelisp-x11-gen-id dpy))
         (r (alloc-bytes 40 8)))
    (ptr-write-u8 r 0 1)                       ; opcode CreateWindow
    (ptr-write-u8 r 1 0)                       ; depth = CopyFromParent
    (nelisp-x11--put16 r 2 10)                 ; request length (8 + 2 values)
    (nelisp-x11--put32 r 4 wid)
    (nelisp-x11--put32 r 8 (aref dpy nelisp-x11--d-root))  ; parent
    (nelisp-x11--put16 r 12 x) (nelisp-x11--put16 r 14 y)
    (nelisp-x11--put16 r 16 w) (nelisp-x11--put16 r 18 h)
    (nelisp-x11--put16 r 20 0)                 ; border-width
    (nelisp-x11--put16 r 22 1)                 ; class = InputOutput
    (nelisp-x11--put32 r 24 0)                 ; visual = CopyFromParent
    (nelisp-x11--put32 r 28 2050)             ; value-mask CWBackPixel|CWEventMask
    (nelisp-x11--put32 r 32 (aref dpy nelisp-x11--d-white)) ; background
    (nelisp-x11--put32 r 36 32769)            ; event-mask Exposure|KeyPress
    (nelisp-x11--write fd r 40)
    wid))

(defun nelisp-x11-map-window (dpy wid)
  "MapWindow WID (make it visible)."
  (let ((r (alloc-bytes 8 8)))
    (ptr-write-u8 r 0 8) (nelisp-x11--put16 r 2 2) (nelisp-x11--put32 r 4 wid)
    (nelisp-x11--write (aref dpy nelisp-x11--d-fd) r 8)))

(defun nelisp-x11-open-font (dpy name)
  "OpenFont core font NAME; return its font id."
  (let* ((fid (nelisp-x11-gen-id dpy))
         (n (length name)) (pad (nelisp-x11--pad4 n))
         (r (alloc-bytes (+ 12 pad) 8)) (i 0))
    (ptr-write-u8 r 0 45) (nelisp-x11--put16 r 2 (+ 3 (/ pad 4)))
    (nelisp-x11--put32 r 4 fid) (nelisp-x11--put16 r 8 n)
    (while (< i n) (ptr-write-u8 r (+ 12 i) (aref name i)) (setq i (1+ i)))
    (nelisp-x11--write (aref dpy nelisp-x11--d-fd) r (+ 12 pad))
    fid))

(defun nelisp-x11-create-gc (dpy drawable fg bg font)
  "CreateGC on DRAWABLE with FG/BG pixels and FONT; return the gc id."
  (let* ((gc (nelisp-x11-gen-id dpy)) (r (alloc-bytes 28 8)))
    (ptr-write-u8 r 0 55) (nelisp-x11--put16 r 2 7)
    (nelisp-x11--put32 r 4 gc) (nelisp-x11--put32 r 8 drawable)
    (nelisp-x11--put32 r 12 16396)            ; GCForeground|GCBackground|GCFont
    (nelisp-x11--put32 r 16 fg) (nelisp-x11--put32 r 20 bg)
    (nelisp-x11--put32 r 24 font)
    (nelisp-x11--write (aref dpy nelisp-x11--d-fd) r 28)
    gc))

(defun nelisp-x11-image-text8 (dpy drawable gc x y str)
  "Draw STR (8-bit) at X,Y on DRAWABLE with GC (foreground on background)."
  (let* ((n (string-bytes str)) (pad (nelisp-x11--pad4 n))
         (r (alloc-bytes (+ 16 pad) 8)) (i 0))
    (ptr-write-u8 r 0 76) (ptr-write-u8 r 1 n)
    (nelisp-x11--put16 r 2 (+ 4 (/ pad 4)))
    (nelisp-x11--put32 r 4 drawable) (nelisp-x11--put32 r 8 gc)
    (nelisp-x11--put16 r 12 x) (nelisp-x11--put16 r 14 y)
    (while (< i n) (ptr-write-u8 r (+ 16 i) (string-byte str i)) (setq i (1+ i)))
    (nelisp-x11--write (aref dpy nelisp-x11--d-fd) r (+ 16 pad))))

(defun nelisp-x11-poll-event (dpy timeout-ms)
  "Wait up to TIMEOUT-MS for a packet on DPY; return a 32-byte buffer or nil.
A 32-byte server packet is an error (byte0=0), reply (1) or event (>=2)."
  (let* ((fd (aref dpy nelisp-x11--d-fd))
         (pfd (alloc-bytes 8 8)))
    (ptr-write-u64 pfd 0 (+ fd 4294967296))   ; fd | POLLIN<<32
    (if (<= (syscall-direct 7 pfd 1 timeout-ms 0 0 0) 0) nil
      (let ((ev (alloc-bytes 32 8)))
        (if (< (nelisp-x11--read-n fd ev 32) 0) nil ev)))))

;;; Demo --------------------------------------------------------------

(defun nelisp-x11-hello (&optional max-events)
  "Open a window, draw a greeting on Expose, quit on KeyPress.
With MAX-EVENTS (for headless verification) stop after that many packets and
return the list of packet codes seen."
  (let ((dpy (nelisp-x11-connect 0)))
    (if (eq (car-safe dpy) 'error) dpy
      (let* ((wid (nelisp-x11-create-window dpy 120 120 460 140))
             (font (nelisp-x11-open-font dpy "fixed"))
             (gc (nelisp-x11-create-gc dpy wid
                                       (aref dpy nelisp-x11--d-black)
                                       (aref dpy nelisp-x11--d-white) font)))
        (nelisp-x11-map-window dpy wid)
        (let ((run t) (codes nil) (seen 0))
          (while run
            (let ((ev (nelisp-x11-poll-event dpy 3000)))
              (cond
               ;; Timeout: headless verification stops; interactive keeps waiting.
               ((null ev) (when max-events (setq run nil)))
               (t
                (let ((code (logand (ptr-read-u8 ev 0) 127)))
                  (setq codes (cons code codes) seen (1+ seen))
                  (cond
                   ((= code 12)                ; Expose
                    (nelisp-x11-image-text8 dpy wid gc 24 60 "Hello from NeLisp X11!")
                    (nelisp-x11-image-text8 dpy wid gc 24 92 "Pure-elisp X client. Press a key."))
                   ((= code 2) (setq run nil))) ; KeyPress
                  (when (and max-events (>= seen max-events)) (setq run nil)))))))
          (nelisp-x11-close dpy)
          (reverse codes))))))

(provide 'nelisp-x11)
;;; nelisp-x11.el ends here
