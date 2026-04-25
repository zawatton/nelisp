;;; nelisp-text-buffer.el --- Phase 8 mutable text primitive (gap-buffer based)  -*- lexical-binding: t; -*-

;; Phase 8 (Doc 33 LOCKED-2026-04-25-v2 §5) — NeLisp Layer 0
;; (dialect-agnostic backend) の mutable text container。
;;
;; *display 概念 0* が design root: no point/marker/overlay/face/major-mode/
;; buffer-local var/undo。pure cursor + mutable text 操作のみ。
;;
;; data structure = gap-buffer (Emacs Lisp 25 年 precedent、Doc 33 §5.1)
;; - bytes = unibyte string (UTF-8 internal repr)、中央に "gap" region 確保
;; - cursor は char index (0-indexed)、gap は byte position 単位
;; - insert at cursor = O(1) amortized (gap 縮小、closed 時のみ memmove)
;; - delete at cursor = O(1) amortized
;; - random move cursor = O(d) where d = gap 移動距離
;; - substring = O(n) (gap skip + decode-coding-string)
;;
;; multibyte handling:
;; - internal repr = UTF-8 byte sequence (Phase 7.4 coding と同 baseline)
;; - cursor は char index (= O(n) cursor → byte mapping)
;; - byte-length は byte 単位 length
;; - text-buffer-multibyte-p で UTF-8 / unibyte 判定
;;
;; search (Doc 33 §5.2 v2 LOCKED): literal substring only。regex は extension
;; package (= nelisp-regex-emacs-compat / nelisp-regex-simple) へ defer。
;;
;; Emacs =buffer= type と意図的に *別 type 名* (Doc 33 §5.3)。
;; nelisp-elisp-compat の =buffer= 実装は内部で =text-buffer= を使い、
;; その上に marker / overlay / text-property を build する model。

;;; Code:

(require 'cl-lib)

;;; defstruct

(cl-defstruct (nelisp-text-buffer
               (:constructor nelisp-text-buffer--make-raw)
               (:copier nil)
               (:predicate nelisp-text-buffer-p))
  "Mutable text buffer with gap-buffer data structure.
Doc 33 LOCKED-2026-04-25-v2 §5。display 概念 0、cursor + mutable text のみ。

Slots:
- BYTES        : unibyte string holding UTF-8 byte sequence with a gap region.
- GAP-START    : byte position where gap begins (= insertion point in bytes).
- GAP-END      : byte position one past gap (= next valid byte after gap).
- CHAR-COUNT   : total char count (= text-buffer-length API return value).
- BYTE-COUNT   : total UTF-8 byte count (= text-buffer-byte-length).
- MULTIBYTE-P  : t for UTF-8 mode, nil for unibyte (= text-buffer-multibyte-p).
- CURSOR-CHAR  : current cursor position in char units (0-indexed).
- CURSOR-BYTE  : cached byte position corresponding to cursor-char (or nil)."
  (bytes        nil)
  (gap-start    0   :type integer)
  (gap-end      0   :type integer)
  (char-count   0   :type integer)
  (byte-count   0   :type integer)
  (multibyte-p  t)
  (cursor-char  0   :type integer)
  (cursor-byte  0   :type integer))

;;; Internal helpers

(defconst nelisp-text-buffer--initial-gap 64
  "Initial gap size (bytes) for a freshly-allocated text buffer.")

(defconst nelisp-text-buffer--min-gap 16
  "Minimum gap size (bytes) maintained after every insert.
When the gap shrinks below this threshold we reallocate to grow it.")

(defun nelisp-text-buffer--gap-size (tb)
  "Return current gap size (bytes) of TB."
  (- (nelisp-text-buffer-gap-end tb)
     (nelisp-text-buffer-gap-start tb)))

(defun nelisp-text-buffer--total-bytes (tb)
  "Return total allocated byte length of TB's underlying buffer."
  (length (nelisp-text-buffer-bytes tb)))

(defun nelisp-text-buffer--byte-at (tb byte-pos)
  "Return the logical byte at BYTE-POS in TB (skipping the gap).
BYTE-POS is in [0, byte-count) — pre-gap if BYTE-POS < gap-start, else
post-gap."
  (let ((bytes (nelisp-text-buffer-bytes tb))
        (gap-start (nelisp-text-buffer-gap-start tb))
        (gap-size (nelisp-text-buffer--gap-size tb)))
    (if (< byte-pos gap-start)
        (aref bytes byte-pos)
      (aref bytes (+ byte-pos gap-size)))))

(defun nelisp-text-buffer--utf8-char-bytes (first-byte)
  "Given FIRST-BYTE of a UTF-8 sequence, return its byte length (1-4)."
  (cond
   ((< first-byte #x80) 1)
   ((< first-byte #xC0) 1) ;; continuation byte (shouldn't be first); treat as 1
   ((< first-byte #xE0) 2)
   ((< first-byte #xF0) 3)
   (t                   4)))

(defun nelisp-text-buffer--char-pos-to-byte-pos (tb char-pos)
  "Return byte position (0-indexed) corresponding to CHAR-POS in TB.
CHAR-POS is in [0, char-count]. End-of-buffer is byte-count."
  (cond
   ((zerop char-pos) 0)
   ((>= char-pos (nelisp-text-buffer-char-count tb))
    (nelisp-text-buffer-byte-count tb))
   ((not (nelisp-text-buffer-multibyte-p tb))
    char-pos)
   (t
    ;; multibyte: walk the logical byte sequence forward, counting chars.
    (let ((byte-pos 0)
          (chars-seen 0))
      (while (< chars-seen char-pos)
        (let ((b (nelisp-text-buffer--byte-at tb byte-pos)))
          (setq byte-pos (+ byte-pos
                            (nelisp-text-buffer--utf8-char-bytes b))
                chars-seen (1+ chars-seen))))
      byte-pos))))

(defun nelisp-text-buffer--logical-to-physical (tb byte-pos)
  "Translate logical BYTE-POS (gap-skipping) into a physical index in BYTES.
For positions before the gap this is the identity; for positions at or
after the gap this adds the gap size."
  (let ((gap-start (nelisp-text-buffer-gap-start tb))
        (gap-size (nelisp-text-buffer--gap-size tb)))
    (if (< byte-pos gap-start)
        byte-pos
      (+ byte-pos gap-size))))

(defun nelisp-text-buffer--ensure-gap-size (tb needed)
  "Ensure TB's gap is at least NEEDED bytes. Reallocate buffer if not."
  (let ((gap-size (nelisp-text-buffer--gap-size tb)))
    (when (< gap-size needed)
      (let* ((old-bytes  (nelisp-text-buffer-bytes tb))
             (gap-start  (nelisp-text-buffer-gap-start tb))
             (gap-end    (nelisp-text-buffer-gap-end tb))
             (old-total  (length old-bytes))
             (post-len   (- old-total gap-end))
             (new-gap    (max needed
                              nelisp-text-buffer--initial-gap
                              (* 2 gap-size)))
             (new-total  (+ gap-start new-gap post-len))
             (new-bytes  (make-string new-total 0)))
        ;; copy pre-gap region
        (when (> gap-start 0)
          (let ((i 0))
            (while (< i gap-start)
              (aset new-bytes i (aref old-bytes i))
              (setq i (1+ i)))))
        ;; copy post-gap region (shifted to new gap-end)
        (when (> post-len 0)
          (let ((src gap-end)
                (dst (+ gap-start new-gap)))
            (while (< src old-total)
              (aset new-bytes dst (aref old-bytes src))
              (setq src (1+ src)
                    dst (1+ dst)))))
        (setf (nelisp-text-buffer-bytes tb) new-bytes)
        (setf (nelisp-text-buffer-gap-start tb) gap-start)
        (setf (nelisp-text-buffer-gap-end tb) (+ gap-start new-gap))))))

(defun nelisp-text-buffer--move-gap-to-byte (tb byte-pos)
  "Move TB's gap so that gap-start = BYTE-POS (logical byte index).
BYTE-POS must be in [0, byte-count]."
  (let ((gap-start (nelisp-text-buffer-gap-start tb))
        (gap-end   (nelisp-text-buffer-gap-end tb))
        (bytes     (nelisp-text-buffer-bytes tb)))
    (cond
     ((= byte-pos gap-start)
      nil) ;; nothing to do
     ((< byte-pos gap-start)
      ;; shift bytes [byte-pos .. gap-start) rightward into the gap end
      ;; result: bytes[byte-pos..gap-start) → bytes[gap-end - (gap-start - byte-pos) .. gap-end)
      (let* ((shift-len (- gap-start byte-pos))
             (src-start byte-pos)
             (dst-start (- gap-end shift-len)))
        ;; copy backward (high to low) to avoid clobbering when ranges overlap
        (let ((i (1- shift-len)))
          (while (>= i 0)
            (aset bytes (+ dst-start i) (aref bytes (+ src-start i)))
            (setq i (1- i))))
        (setf (nelisp-text-buffer-gap-start tb) byte-pos)
        (setf (nelisp-text-buffer-gap-end tb) (- gap-end shift-len))))
     (t
      ;; byte-pos > gap-start: shift bytes [gap-end .. gap-end + (byte-pos - gap-start))
      ;; leftward to fill from gap-start.
      (let* ((shift-len (- byte-pos gap-start))
             (src-start gap-end)
             (dst-start gap-start))
        (let ((i 0))
          (while (< i shift-len)
            (aset bytes (+ dst-start i) (aref bytes (+ src-start i)))
            (setq i (1+ i))))
        (setf (nelisp-text-buffer-gap-start tb) byte-pos)
        (setf (nelisp-text-buffer-gap-end tb) (+ gap-end shift-len)))))))

(defun nelisp-text-buffer--encode (tb str)
  "Encode STR according to TB's multibyte flag, return a unibyte byte string.
For multibyte buffers we UTF-8 encode; for unibyte buffers we require
STR to already be a unibyte string of bytes."
  (cond
   ((nelisp-text-buffer-multibyte-p tb)
    (encode-coding-string str 'utf-8 t))
   (t
    ;; Unibyte mode: caller is responsible for raw bytes; if STR is
    ;; multibyte, encode UTF-8 and treat each byte as a "char". This
    ;; keeps the API forgiving while preserving byte-faithful storage.
    (if (multibyte-string-p str)
        (encode-coding-string str 'utf-8 t)
      str))))

(defun nelisp-text-buffer--count-chars-in-bytes (tb byte-string)
  "Return the char count corresponding to BYTE-STRING for TB."
  (cond
   ((not (nelisp-text-buffer-multibyte-p tb))
    (length byte-string))
   (t
    ;; UTF-8: count bytes whose top 2 bits are not 10 (= continuation)
    (let ((i 0)
          (n (length byte-string))
          (chars 0))
      (while (< i n)
        (let ((b (aref byte-string i)))
          (setq i (+ i (nelisp-text-buffer--utf8-char-bytes b))
                chars (1+ chars))))
      chars))))

;;; Public API (Doc 33 §5.2 v2 LOCKED — 9 primitives)

;;;###autoload
(defun make-text-buffer (&optional initial-content)
  "Create a fresh `nelisp-text-buffer'.
If INITIAL-CONTENT (a string) is supplied, the buffer is pre-loaded
with it and the cursor is positioned at the beginning.

Multibyte mode is inferred from INITIAL-CONTENT: a multibyte string
yields a multibyte buffer; a unibyte string yields a unibyte one. With
no initial content the buffer defaults to multibyte (UTF-8)."
  (let* ((multibyte (if initial-content
                        (multibyte-string-p initial-content)
                      t))
         (encoded (cond
                   ((null initial-content) "")
                   (multibyte (encode-coding-string initial-content 'utf-8 t))
                   (t initial-content)))
         (init-byte-count (length encoded))
         (init-char-count (cond
                           ((null initial-content) 0)
                           (multibyte (length initial-content))
                           (t init-byte-count)))
         (gap-len nelisp-text-buffer--initial-gap)
         (total (+ init-byte-count gap-len))
         (bytes (make-string total 0))
         (tb (nelisp-text-buffer--make-raw
              :bytes bytes
              :gap-start init-byte-count
              :gap-end (+ init-byte-count gap-len)
              :char-count init-char-count
              :byte-count init-byte-count
              :multibyte-p multibyte
              :cursor-char init-char-count
              :cursor-byte init-byte-count)))
    ;; copy encoded into bytes [0..init-byte-count)
    (when (> init-byte-count 0)
      (let ((i 0))
        (while (< i init-byte-count)
          (aset bytes i (aref encoded i))
          (setq i (1+ i)))))
    tb))

;;;###autoload
(defun text-buffer-insert (tb str)
  "Insert STR at TB's cursor position.
The cursor advances to the end of the inserted text. Returns TB."
  (unless (nelisp-text-buffer-p tb)
    (signal 'wrong-type-argument (list 'nelisp-text-buffer-p tb)))
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
  (let* ((encoded (nelisp-text-buffer--encode tb str))
         (n-bytes (length encoded))
         (n-chars (nelisp-text-buffer--count-chars-in-bytes tb encoded)))
    (when (> n-bytes 0)
      ;; ensure cursor-byte cache in sync with cursor-char
      (let ((cursor-byte (nelisp-text-buffer--char-pos-to-byte-pos
                          tb (nelisp-text-buffer-cursor-char tb))))
        (nelisp-text-buffer--move-gap-to-byte tb cursor-byte)
        (nelisp-text-buffer--ensure-gap-size tb n-bytes)
        (let ((bytes (nelisp-text-buffer-bytes tb))
              (gap-start (nelisp-text-buffer-gap-start tb)))
          (let ((i 0))
            (while (< i n-bytes)
              (aset bytes (+ gap-start i) (aref encoded i))
              (setq i (1+ i)))))
        (setf (nelisp-text-buffer-gap-start tb)
              (+ (nelisp-text-buffer-gap-start tb) n-bytes))
        (setf (nelisp-text-buffer-byte-count tb)
              (+ (nelisp-text-buffer-byte-count tb) n-bytes))
        (setf (nelisp-text-buffer-char-count tb)
              (+ (nelisp-text-buffer-char-count tb) n-chars))
        (setf (nelisp-text-buffer-cursor-char tb)
              (+ (nelisp-text-buffer-cursor-char tb) n-chars))
        (setf (nelisp-text-buffer-cursor-byte tb)
              (nelisp-text-buffer-gap-start tb))))
    tb))

;;;###autoload
(defun text-buffer-delete (tb start end)
  "Delete the characters in [START, END) from TB.
START and END are 0-indexed char positions; START <= END <= length.
The cursor is adjusted: if it lay inside the deleted range it is
moved to START; if it lay after END it is shifted left by the deleted
char count. Returns TB."
  (unless (nelisp-text-buffer-p tb)
    (signal 'wrong-type-argument (list 'nelisp-text-buffer-p tb)))
  (unless (and (integerp start) (integerp end))
    (signal 'wrong-type-argument (list 'integerp start end)))
  (let ((char-count (nelisp-text-buffer-char-count tb)))
    (when (or (< start 0) (> end char-count) (> start end))
      (signal 'args-out-of-range (list start end char-count))))
  (let ((n-chars (- end start)))
    (when (> n-chars 0)
      (let* ((start-byte (nelisp-text-buffer--char-pos-to-byte-pos tb start))
             (end-byte   (nelisp-text-buffer--char-pos-to-byte-pos tb end))
             (n-bytes    (- end-byte start-byte))
             (cursor-char (nelisp-text-buffer-cursor-char tb)))
        (nelisp-text-buffer--move-gap-to-byte tb start-byte)
        ;; expand gap to absorb [start-byte .. end-byte)
        (setf (nelisp-text-buffer-gap-end tb)
              (+ (nelisp-text-buffer-gap-end tb) n-bytes))
        (setf (nelisp-text-buffer-byte-count tb)
              (- (nelisp-text-buffer-byte-count tb) n-bytes))
        (setf (nelisp-text-buffer-char-count tb)
              (- (nelisp-text-buffer-char-count tb) n-chars))
        ;; cursor adjustment
        (cond
         ((<= cursor-char start)
          ;; cursor before deleted region: byte index unchanged
          (setf (nelisp-text-buffer-cursor-byte tb) start-byte))
         ((< cursor-char end)
          ;; cursor inside deleted region: collapse to start
          (setf (nelisp-text-buffer-cursor-char tb) start)
          (setf (nelisp-text-buffer-cursor-byte tb) start-byte))
         (t
          ;; cursor after deleted region: shift left
          (setf (nelisp-text-buffer-cursor-char tb)
                (- cursor-char n-chars))
          (setf (nelisp-text-buffer-cursor-byte tb)
                (nelisp-text-buffer--char-pos-to-byte-pos
                 tb (nelisp-text-buffer-cursor-char tb))))))))
  tb)

;;;###autoload
(defun text-buffer-cursor (tb)
  "Return the current cursor position of TB as a 0-indexed char index."
  (unless (nelisp-text-buffer-p tb)
    (signal 'wrong-type-argument (list 'nelisp-text-buffer-p tb)))
  (nelisp-text-buffer-cursor-char tb))

;;;###autoload
(defun text-buffer-set-cursor (tb pos)
  "Move TB's cursor to char position POS (0-indexed).
POS must be in [0, length]. Returns TB."
  (unless (nelisp-text-buffer-p tb)
    (signal 'wrong-type-argument (list 'nelisp-text-buffer-p tb)))
  (unless (integerp pos)
    (signal 'wrong-type-argument (list 'integerp pos)))
  (let ((char-count (nelisp-text-buffer-char-count tb)))
    (when (or (< pos 0) (> pos char-count))
      (signal 'args-out-of-range (list pos char-count))))
  (setf (nelisp-text-buffer-cursor-char tb) pos)
  (setf (nelisp-text-buffer-cursor-byte tb)
        (nelisp-text-buffer--char-pos-to-byte-pos tb pos))
  tb)

;;;###autoload
(defun text-buffer-substring (tb start end)
  "Return the substring of TB in char range [START, END)."
  (unless (nelisp-text-buffer-p tb)
    (signal 'wrong-type-argument (list 'nelisp-text-buffer-p tb)))
  (unless (and (integerp start) (integerp end))
    (signal 'wrong-type-argument (list 'integerp start end)))
  (let ((char-count (nelisp-text-buffer-char-count tb)))
    (when (or (< start 0) (> end char-count) (> start end))
      (signal 'args-out-of-range (list start end char-count))))
  (cond
   ((= start end) "")
   (t
    (let* ((start-byte (nelisp-text-buffer--char-pos-to-byte-pos tb start))
           (end-byte   (nelisp-text-buffer--char-pos-to-byte-pos tb end))
           (n-bytes    (- end-byte start-byte))
           (raw        (make-string n-bytes 0))
           (i          0))
      (while (< i n-bytes)
        (aset raw i (nelisp-text-buffer--byte-at tb (+ start-byte i)))
        (setq i (1+ i)))
      (if (nelisp-text-buffer-multibyte-p tb)
          (decode-coding-string raw 'utf-8 t)
        raw)))))

;;;###autoload
(defun text-buffer-search (tb pattern &optional from-pos)
  "Search TB for the literal substring PATTERN.
Returns the 0-indexed char position of the first match at or after
FROM-POS (default 0), or nil if PATTERN is not found.

This is *literal-only* search per Doc 33 §5.2 v2 LOCKED — regex search
is intentionally deferred to extension packages
(`nelisp-regex-emacs-compat' / `nelisp-regex-simple')."
  (unless (nelisp-text-buffer-p tb)
    (signal 'wrong-type-argument (list 'nelisp-text-buffer-p tb)))
  (unless (stringp pattern)
    (signal 'wrong-type-argument (list 'stringp pattern)))
  (let* ((from (or from-pos 0))
         (char-count (nelisp-text-buffer-char-count tb)))
    (when (or (< from 0) (> from char-count))
      (signal 'args-out-of-range (list from char-count)))
    (cond
     ((string-empty-p pattern) from)
     (t
      (let* ((pat-encoded (nelisp-text-buffer--encode tb pattern))
             (pat-len     (length pat-encoded))
             (pat-chars   (nelisp-text-buffer--count-chars-in-bytes
                           tb pat-encoded))
             (byte-count  (nelisp-text-buffer-byte-count tb))
             (from-byte   (nelisp-text-buffer--char-pos-to-byte-pos tb from))
             (cur-byte    from-byte)
             (cur-char    from)
             (last-char   (- char-count pat-chars))
             (found-char  nil))
        (catch 'done
          (when (or (zerop pat-len) (< last-char from))
            (throw 'done nil))
          (while (<= cur-char last-char)
            (let ((match t)
                  (i 0))
              (while (and match (< i pat-len))
                (when (/= (nelisp-text-buffer--byte-at tb (+ cur-byte i))
                          (aref pat-encoded i))
                  (setq match nil))
                (setq i (1+ i)))
              (cond
               (match
                (setq found-char cur-char)
                (throw 'done nil))
               (t
                ;; advance cursor by one char (= UTF-8 lead byte width)
                (let ((b (nelisp-text-buffer--byte-at tb cur-byte)))
                  (setq cur-byte
                        (+ cur-byte
                           (if (nelisp-text-buffer-multibyte-p tb)
                               (nelisp-text-buffer--utf8-char-bytes b)
                             1))
                        cur-char (1+ cur-char))))))))
        (when (and (null found-char)
                   (zerop pat-len))
          (setq found-char from))
        ;; Edge case: pattern length 0 already handled above; handle
        ;; whole-buffer match for last position when last-char < 0.
        (ignore byte-count)
        found-char)))))

;;;###autoload
(defun text-buffer-length (tb)
  "Return the length of TB in chars (= cursor-addressable units)."
  (unless (nelisp-text-buffer-p tb)
    (signal 'wrong-type-argument (list 'nelisp-text-buffer-p tb)))
  (nelisp-text-buffer-char-count tb))

;;;###autoload
(defun text-buffer-multibyte-p (tb)
  "Return non-nil if TB stores text as UTF-8 multibyte."
  (unless (nelisp-text-buffer-p tb)
    (signal 'wrong-type-argument (list 'nelisp-text-buffer-p tb)))
  (nelisp-text-buffer-multibyte-p tb))

;;;###autoload
(defun text-buffer-byte-length (tb)
  "Return the length of TB in bytes (UTF-8 byte count for multibyte buffers)."
  (unless (nelisp-text-buffer-p tb)
    (signal 'wrong-type-argument (list 'nelisp-text-buffer-p tb)))
  (nelisp-text-buffer-byte-count tb))

(provide 'nelisp-text-buffer)
;;; nelisp-text-buffer.el ends here
