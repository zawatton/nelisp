;;; nemacs-runtime-cdb.el --- buffer-free CDB reader for the bridge  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; A self-contained, buffer-free reader for Bernstein constant databases
;; (cdb), baked into the bridge runtime image.  ddskk's `cdb.el' reads via
;; `with-current-buffer' + `insert-file-contents-literally' +
;; `buffer-substring-no-properties' and builds its hash with `aset' on a
;; 4-byte string -- none of which work in the bridge runtime (no buffers; raw
;; immutable strings).  `(require 'cdb)' silently no-ops there, so these
;; definitions stand in: they expose the same `cdb-init' / `cdb-get' /
;; `cdb-uninit' interface ddskk's skk-convert.el / dictionary-server-cdb.el
;; call, implemented with `syscall-direct' file reads (open=2 / pread64=17 /
;; close=3) and an integer djb hash (64-bit ints have no 32-bit overflow, so
;; the byte-vector dance is unnecessary).
;;
;; This makes SKK kana-kanji conversion work fully locally on the GUI runtime
;; -- no network: yomi -> cdb-get -> candidates from the dictionary file.
;;
;; Gated on (not (fboundp 'cdb-get)) so a host Emacs / real cdb.el wins.
;; Requires the syscall shim's `nl-ffi-shim--cstr' / `nl-ffi-read-bytes'
;; (baked earlier) and `substring' (for cdb--sget).

;;; Code:

(unless (fboundp 'cdb-get)
  (when (and (fboundp 'syscall-direct)
             (fboundp 'nl-ffi-read-bytes)
             (fboundp 'nl-ffi-shim--cstr))

    (defconst cdb--header-size 2048)
    (defvar cdb--headers (make-hash-table :test 'equal)
      "path -> cached 2048-byte cdb header string.")

    (defun cdb--read (path offset length)
      "Read LENGTH bytes at OFFSET from PATH as a string, via raw syscalls."
      (let* ((cstr (nl-ffi-shim--cstr path))
             (fd (syscall-direct 2 cstr 0 0 0 0 0))        ; open(path, O_RDONLY)
             (buf (alloc-bytes (if (> length 0) length 1) 8))
             (n (syscall-direct 17 fd buf length offset 0 0))) ; pread64(fd,buf,len,off)
        (syscall-direct 3 fd 0 0 0 0 0)                    ; close(fd)
        (nl-ffi-read-bytes buf (if (> n 0) n 0))))

    (defun cdb--u32 (s off)
      "Little-endian uint32 at OFF in byte string S."
      (logior (aref s off)
              (ash (aref s (+ off 1)) 8)
              (ash (aref s (+ off 2)) 16)
              (ash (aref s (+ off 3)) 24)))

    (defun cdb--sget (s off n) (substring s off (+ off n)))

    (defun cdb--hash (key)
      "djb cdb hash of KEY as a 32-bit integer."
      (let ((h 5381) (i 0) (n (length key)))
        (while (< i n)
          (setq h (logand 4294967295
                          (logxor (logand 4294967295 (* h 33)) (aref key i))))
          (setq i (1+ i)))
        h))

    (defun cdb-init (path)
      "Open the cdb at PATH (cache its header).  Returns PATH."
      (unless (gethash path cdb--headers)
        (puthash path (cdb--read path 0 cdb--header-size) cdb--headers))
      path)

    (defun cdb-uninit (path)
      (remhash path cdb--headers)
      nil)

    (defun cdb-get (path key)
      "Return the value associated with KEY in the cdb at PATH, or nil."
      (let ((header (gethash path cdb--headers))
            (hv (cdb--hash key)))
        (unless header (error "cdb not initialized: %s" path))
        (let* ((boffset (* 8 (logand hv 255)))
               (foffset (cdb--u32 header boffset))
               (nents (cdb--u32 header (+ 4 boffset))))
          (if (zerop nents)
              nil
            (let ((ents (cdb--read path foffset (* nents 8)))
                  (o (ash hv -8))
                  (n 0)
                  (result nil)
                  (done nil))
              (while (and (not done) (< n nents))
                (let ((i (mod (+ o n) nents)))
                  (when (= hv (cdb--u32 ents (* i 8)))
                    (let ((rfo (cdb--u32 ents (+ 4 (* i 8)))))
                      (unless (zerop rfo)
                        (let ((klen (cdb--u32 (cdb--read path rfo 4) 0)))
                          (when (equal key (cdb--read path (+ 8 rfo) klen))
                            (let ((vlen (cdb--u32 (cdb--read path (+ 4 rfo) 4) 0)))
                              (setq result (cdb--read path (+ klen 8 rfo) vlen)
                                    done t))))))))
                (setq n (1+ n)))
              result)))))))

(provide 'nemacs-runtime-cdb)

;;; nemacs-runtime-cdb.el ends here
