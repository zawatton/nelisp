;;; nemacs-editor-transport.el --- GUI transport shaping for AOT input -*- lexical-binding: t; -*-

;; The native GUI should send raw key sequences and leave key lookup,
;; minibuffer, command execution, and redisplay state ownership to
;; nelisp-emacs.  Legacy direct-command IR remains in nemacs-editor.el as
;; migration source, but it must not reach the compiled GUI binary.

(require 'cl-lib)

(defvar nemacs-direct-command-transport-dropped 0)
(defvar nemacs-redisplay-correction-dropped 0)
(defvar nemacs-hscroll-redisplay-patched 0)
(defvar nemacs-window-split-redisplay-patched 0)
(defvar nemacs-tabline-redisplay-patched 0)
(defvar nemacs-transport-paths-rewritten 0)

(defun nemacs--legacy-cmd-write-form-p (form)
  (and (consp form)
       (eq (car form) 'let*)
       (let ((bindings (cadr form))
             (body (caddr form)))
         (and (consp bindings)
              (equal (car bindings)
                     '(cfd (syscall-direct 257 -100 cmdp 577 438 0 0)))
              (consp body)
              (eq (car body) 'seq)
              (let ((write (cadr body))
                    (close (caddr body)))
                (and (consp write)
                     (eq (nth 0 write) 'syscall-direct)
                     (equal (nth 1 write) 1)
                     (eq (nth 2 write) 'cfd)
                     (eq (nth 3 write) 'mb)
                     (equal (nth 5 write) 0)
                     (equal (nth 6 write) 0)
                     (equal (nth 7 write) 0)
                     (equal close
                            '(syscall-direct 3 cfd 0 0 0 0 0))))))))

(defun nemacs--redisplay-correction-form-p (form)
  (member form
          '((if (> ws tlen) (setq ws 0) 0)
            (if (< pt2 ws) (setq ws 0) 0)
            (if (> poff2 rn) (setq poff2 rn) 0))))

(defun nemacs--strip-legacy-cmd-channel (form)
  (cond
   ((nemacs--legacy-cmd-write-form-p form)
    (setq nemacs-direct-command-transport-dropped
          (+ nemacs-direct-command-transport-dropped 1))
    0)
   ((nemacs--redisplay-correction-form-p form)
    (setq nemacs-redisplay-correction-dropped
          (+ nemacs-redisplay-correction-dropped 1))
    0)
   ((consp form)
    (mapcar #'nemacs--strip-legacy-cmd-channel form))
   (t form)))

(defvar nemacs-prefork-buffer-publish-dropped 0)

(defun nemacs--prefork-buffer-publish-p (form)
  "Return non-nil for the legacy pre-bridge tb->bufp publish block."
  (equal form
         '(let* ((wfd (syscall-direct 257 -100 bufp 577 438 0 0)))
            (seq (syscall-direct 1 wfd tb (ptr-read-u16 st 2) 0 0 0)
                 (syscall-direct 3 wfd 0 0 0 0 0)))))

(defun nemacs--drop-prefork-buffer-publish (form)
  "Remove GUI-local buffer publishes before bridge forks.
The bridge/session store is authoritative; publishing tb here can overwrite it
with the compiled demo text before the bridge handles the actual key."
  (cond
   ((nemacs--prefork-buffer-publish-p form)
    (setq nemacs-prefork-buffer-publish-dropped
          (+ nemacs-prefork-buffer-publish-dropped 1))
    0)
   ((consp form)
    (mapcar #'nemacs--drop-prefork-buffer-publish form))
   (t form)))

(defun nemacs--path-write-forms (var path)
  (append
   (cl-loop for byte across (string-as-unibyte path)
            for i from 0
            collect (list 'ptr-write-u8 var i byte))
   (list (list 'ptr-write-u8 var (length path) 0))))

(defun nemacs--hscroll-read-form ()
  `(seq
    ,@(nemacs--path-write-forms 'spath "/tmp/nemacs-window-hscroll")
    (let* ((hfd (syscall-direct 257 -100 spath 0 0 0 0))
           (hn (if (>= hfd 0) (syscall-direct 0 hfd mb 64 0 0 0) 0)))
      (seq
       (if (> hn 0)
           (let* ((hi 0) (hoff 0))
             (seq
              (while (< hi hn)
                (let* ((hc (ptr-read-u8 mb hi)))
                  (seq
                   (if (if (>= hc 48) (if (< hc 58) 1 0) 0)
                       (setq hoff (+ (* hoff 10) (- hc 48)))
                     0)
                   (setq hi (+ hi 1)))))
              (setq hs hoff)))
         0)
       (if (>= hfd 0) (syscall-direct 3 hfd 0 0 0 0 0) 0)))))

(defun nemacs--window-split-read-and-clamp-form ()
  `(seq
    ,@(nemacs--path-write-forms 'spath "/tmp/nemacs-window-split-delta")
    (let* ((sfd (syscall-direct 257 -100 spath 0 0 0 0))
           (sn (if (>= sfd 0) (syscall-direct 0 sfd mb 64 0 0 0) 0)))
      (seq
       (if (> sn 0)
           (let* ((si 0) (soff 0) (sneg 0))
             (seq
              (if (= (ptr-read-u8 mb 0) 45)
                  (seq (setq sneg 1) (setq si 1))
                0)
              (while (< si sn)
                (let* ((sc (ptr-read-u8 mb si)))
                  (seq
                   (if (if (>= sc 48) (if (< sc 58) 1 0) 0)
                       (setq soff (+ (* soff 10) (- sc 48)))
                     0)
                   (setq si (+ si 1)))))
              (if (= sneg 1)
                  (setq sd (- 0 soff))
                (setq sd soff))))
         0)
       (if (>= sfd 0) (syscall-direct 3 sfd 0 0 0 0 0) 0)))
    (setq vsp (+ (/ ww 2) (* sd 9)))
    (if (< vsp 40) (setq vsp 40) 0)
    (if (> vsp (- ww 40)) (setq vsp (- ww 40)) 0)
    (setq hsp (+ (/ (- wh 22) 2) (* sd 16)))
    (if (< hsp 32) (setq hsp 32) 0)
    (if (> hsp (- (- wh 22) 32)) (setq hsp (- (- wh 22) 32)) 0)))

(defun nemacs--tabline-read-and-draw-form ()
  `(seq
    ,@(nemacs--path-write-forms 'spath "/tmp/nemacs-tab-state")
    (let* ((tfd (syscall-direct 257 -100 spath 0 0 0 0))
           (tn (if (>= tfd 0) (syscall-direct 0 tfd mb 80 0 0 0) 0))
           (ti 0)
           (tabs 0)
           (name-start 0)
           (slen 0)
           (sk 0))
      (seq
       (if (>= tfd 0) (syscall-direct 3 tfd 0 0 0 0 0) 0)
       (if (> tn 0)
           (seq
            (while (if (< ti tn) (< tabs 2) 0)
              (seq
               (if (= (ptr-read-u8 mb ti) 9)
                   (setq tabs (+ tabs 1))
                 0)
               (setq ti (+ ti 1))))
            (setq name-start ti)
            (while (if (< (+ name-start slen) tn)
                       (if (< slen 40)
                           (if (= (ptr-read-u8 mb (+ name-start slen)) 10)
                               0
                             (if (= (ptr-read-u8 mb (+ name-start slen)) 9) 0 1))
                         0)
                     0)
              (setq slen (+ slen 1)))
            (if (> slen 0)
                (seq
                 (ptr-write-u8 buf 0 56)
                 (ptr-write-u8 buf 1 0)
                 (ptr-write-u16 buf 2 4)
                 (ptr-write-u32 buf 4 gc)
                 (ptr-write-u32 buf 8 4)
                 (ptr-write-u32 buf 12 ml)
                 (syscall-direct 1 fd buf 16 0 0 0)
                 (ptr-write-u8 buf 0 70)
                 (ptr-write-u8 buf 1 0)
                 (ptr-write-u16 buf 2 5)
                 (ptr-write-u32 buf 4 pm)
                 (ptr-write-u32 buf 8 gc)
                 (ptr-write-u16 buf 12 0)
                 (ptr-write-u16 buf 14 0)
                 (ptr-write-u16 buf 16 ww)
                 (ptr-write-u16 buf 18 22)
                 (syscall-direct 1 fd buf 20 0 0 0)
                 (ptr-write-u8 buf 0 56)
                 (ptr-write-u8 buf 1 0)
                 (ptr-write-u16 buf 2 4)
                 (ptr-write-u32 buf 4 gc)
                 (ptr-write-u32 buf 8 4)
                 (ptr-write-u32 buf 12 fg)
                 (syscall-direct 1 fd buf 16 0 0 0)
                 (ptr-write-u8 buf 0 76)
                 (ptr-write-u8 buf 1 slen)
                 (ptr-write-u16 buf 2 (+ 4 (/ (* 4 (/ (+ slen 3) 4)) 4)))
                 (ptr-write-u32 buf 4 pm)
                 (ptr-write-u32 buf 8 gc)
                 (ptr-write-u16 buf 12 12)
                 (ptr-write-u16 buf 14 16)
                 (while (< sk slen)
                   (seq
                    (ptr-write-u8 buf (+ 16 sk) (ptr-read-u8 mb (+ name-start sk)))
                    (setq sk (+ sk 1))))
                 (syscall-direct 1 fd buf (+ 16 (* 4 (/ (+ slen 3) 4))) 0 0 0))
              0))
         0)))))

(defun nemacs--replace-hscroll-display-atoms (form)
  (cond
   ((equal form '(ptr-read-u8 tb (+ lstart k)))
    '(ptr-read-u8 tb (+ dstart k)))
   ((eq form 'linelen) 'dlen)
   ((consp form)
    (mapcar #'nemacs--replace-hscroll-display-atoms form))
   (t form)))

(defun nemacs--hscroll-display-line-form-p (form)
  (and (consp form)
       (eq (car form) 'let*)
       (equal (cadr form)
              '((linelen (- i lstart)) (k 0)))
       (consp (caddr form))
       (eq (caaddr form) 'seq)))

(defun nemacs--patch-hscroll-display-line (form)
  (let ((body (nemacs--replace-hscroll-display-atoms (caddr form))))
    `(let* ((linelen (- i lstart))
            (k 0)
            (dstart (+ lstart hs))
            (dlen (- linelen hs)))
       (seq
        (if (< dlen 0) (setq dlen 0) 0)
        ,@(cdr body)))))

(defun nemacs--patch-hscroll-cursor-x (form)
  (if (equal form '(ptr-write-u16 buf 12 (+ 12 (* cc 9))))
      '(ptr-write-u16 buf 12 (+ 12 (* (if (> hs cc) 0 (- cc hs)) 9)))
    form))

(defun nemacs--patch-hscroll-display (form)
  (cond
   ((nemacs--hscroll-display-line-form-p form)
    (nemacs--patch-hscroll-display-line form))
   ((equal form '(ptr-write-u16 buf 12 (+ 12 (* cc 9))))
    (nemacs--patch-hscroll-cursor-x form))
   ((consp form)
    (mapcar #'nemacs--patch-hscroll-display form))
   (t (nemacs--patch-hscroll-cursor-x form))))

(defun nemacs--window-split-if-p (form layout-value)
  (and (consp form)
       (eq (car form) 'if)
       (equal (cadr form) `(= (ptr-read-u8 st 5) ,layout-value))))

(defun nemacs--replace-window-split-atoms (form)
  (cond
   ((equal form '(/ ww 2)) 'vsp)
   ((equal form '(/ (- wh 22) 2)) 'hsp)
   ((consp form)
    (mapcar #'nemacs--replace-window-split-atoms form))
   (t form)))

(defun nemacs--patch-window-split-display (form)
  (cond
   ((nemacs--window-split-if-p form 1)
    `(if ,(cadr form)
         ,@(mapcar #'nemacs--replace-window-split-atoms (cddr form))))
   ((nemacs--window-split-if-p form 2)
    `(if ,(cadr form)
         ,@(mapcar #'nemacs--replace-window-split-atoms (cddr form))))
   ((consp form)
    (mapcar #'nemacs--patch-window-split-display form))
   (t form)))

(defun nemacs--redisplay-let-p (form)
  (and (consp form)
       (eq (car form) 'let*)
       (let ((bindings (cadr form)))
         (and (equal (car bindings) '(i 0))
              (member '(ws (ptr-read-u16 st 16)) bindings)
              (member '(ci 0) bindings)
              (member '(cl 0) bindings)
              (member '(cc 0) bindings)))))

(defun nemacs--insert-after-first (item pred list)
  (let ((out nil)
        (rest list)
        (inserted nil))
    (while rest
      (let ((cur (car rest)))
        (push cur out)
        (when (and (not inserted) (funcall pred cur))
          (push item out)
          (setq inserted t)))
      (setq rest (cdr rest)))
    (nreverse out)))

(defun nemacs--patch-hscroll-redisplay (form)
  (cond
   ((nemacs--redisplay-let-p form)
    (setq nemacs-hscroll-redisplay-patched
          (+ nemacs-hscroll-redisplay-patched 1))
    (let* ((bindings (cadr form))
           (body (caddr form))
           (body-forms (cdr body))
           (bindings-with-hscroll
            (nemacs--insert-after-first
             '(hs 0)
             (lambda (binding) (equal binding '(ws (ptr-read-u16 st 16))))
             bindings))
           (body-with-hscroll
            (cons 'seq
                  (nemacs--insert-after-first
                   (nemacs--hscroll-read-form)
                   (lambda (body-form) (equal body-form '(setq ci ws)))
                   body-forms))))
      `(let* ,bindings-with-hscroll
         ,(nemacs--patch-hscroll-display body-with-hscroll))))
   ((consp form)
    (mapcar #'nemacs--patch-hscroll-redisplay form))
   (t form)))

(defun nemacs--patch-window-split-redisplay (form)
  (cond
   ((nemacs--redisplay-let-p form)
    (setq nemacs-window-split-redisplay-patched
          (+ nemacs-window-split-redisplay-patched 1))
    (let* ((bindings (cadr form))
           (body (caddr form))
           (body-forms (cdr body))
           (bindings-with-split
            (nemacs--insert-after-first
             '(hsp 0)
             (lambda (binding) (equal binding '(ws (ptr-read-u16 st 16))))
             (nemacs--insert-after-first
              '(vsp 0)
              (lambda (binding) (equal binding '(ws (ptr-read-u16 st 16))))
              (nemacs--insert-after-first
               '(sd 0)
               (lambda (binding) (equal binding '(ws (ptr-read-u16 st 16))))
               bindings))))
           (body-with-split
            (cons 'seq
                  (nemacs--insert-after-first
                   (nemacs--window-split-read-and-clamp-form)
                   (lambda (body-form) (equal body-form '(setq ci ws)))
                   body-forms))))
      `(let* ,bindings-with-split
         ,(nemacs--patch-window-split-display body-with-split))))
   ((consp form)
    (mapcar #'nemacs--patch-window-split-redisplay form))
   (t form)))

(defun nemacs--patch-tabline-redisplay (form)
  (cond
   ((nemacs--redisplay-let-p form)
    (setq nemacs-tabline-redisplay-patched
          (+ nemacs-tabline-redisplay-patched 1))
    (let* ((bindings (cadr form))
           (body (caddr form))
           (body-forms (cdr body))
           (body-with-tabline
            (cons 'seq
                  (nemacs--insert-after-first
                   (nemacs--tabline-read-and-draw-form)
                   (lambda (body-form)
                     (equal body-form '(syscall-direct 1 fd buf 20 0 0 0)))
                   body-forms))))
      `(let* ,bindings ,body-with-tabline)))
   ((consp form)
    (mapcar #'nemacs--patch-tabline-redisplay form))
   (t form)))

;; M12 display/font lane: paint the face spans nelisp-emacs resolves.
;;
;; The substrate writes /tmp/nemacs-face-spans (START.TAB.END.TAB.FACE
;; .TAB.#rrggbb lines, offsets in the nemacs-point coordinate space)
;; and /tmp/nemacs-font ("name".TAB.FONT / "script".TAB.SCRIPT) on
;; every redisplay.  This GUI only parses the resolved colors and font
;; name and paints them — the face decision itself stays in
;; nelisp-emacs (boundary doc 01 / nelisp-emacs Doc 09 section 6).
;; The legacy first-char line heuristic remains only as a migration
;; fallback for runs with no span data.

(defvar nemacs-face-span-redisplay-patched 0)
(defvar nemacs-face-span-color-replaced 0)
(defvar nemacs-font-openfont-patched 0)
(defvar nemacs-font-queryfont-patched 0)

(defconst nemacs--legacy-line-color-form
  '(if (= (ptr-read-u8 tb lstart) 42)
       15453579
     (if (= (ptr-read-u8 tb lstart) 35)
         kw
       (if (= (ptr-read-u8 tb lstart) 45)
           comment
         (if (= (ptr-read-u8 tb lstart) 59)
             comment
           fg))))
  "The GUI-side first-char color heuristic this lane retires to fallback.")

(defun nemacs--face-span-read-form ()
  "IR: read /tmp/nemacs-face-spans into spanbuf, set spn to its length."
  `(seq
    ,@(nemacs--path-write-forms 'spath "/tmp/nemacs-face-spans")
    (let* ((ffd (syscall-direct 257 -100 spath 0 0 0 0)))
      (seq
       (if (>= ffd 0)
           (setq spn (syscall-direct 0 ffd spanbuf 4096 0 0 0))
         (setq spn 0))
       (if (>= ffd 0) (syscall-direct 3 ffd 0 0 0 0 0) 0)))))

(defun nemacs--face-span-line-color-form (line-end)
  "IR: set lcol to the resolved color of the first span overlapping
the [lstart, LINE-END) line, or fg when no span covers it."
  `(let* ((sp 0) (sfound 0))
     (seq
      (setq lcol fg)
      (while (< sp spn)
        (let* ((sstart 0) (send 0) (scol 0) (sch 0))
          (seq
           (while (if (< sp spn) (if (= (ptr-read-u8 spanbuf sp) 9) 0 1) 0)
             (seq
              (setq sch (ptr-read-u8 spanbuf sp))
              (if (if (>= sch 48) (if (< sch 58) 1 0) 0)
                  (setq sstart (+ (* sstart 10) (- sch 48)))
                0)
              (setq sp (+ sp 1))))
           (setq sp (+ sp 1))
           (while (if (< sp spn) (if (= (ptr-read-u8 spanbuf sp) 9) 0 1) 0)
             (seq
              (setq sch (ptr-read-u8 spanbuf sp))
              (if (if (>= sch 48) (if (< sch 58) 1 0) 0)
                  (setq send (+ (* send 10) (- sch 48)))
                0)
              (setq sp (+ sp 1))))
           (setq sp (+ sp 1))
           (while (if (< sp spn) (if (= (ptr-read-u8 spanbuf sp) 9) 0 1) 0)
             (setq sp (+ sp 1)))
           (setq sp (+ sp 1))
           (while (if (< sp spn) (if (= (ptr-read-u8 spanbuf sp) 10) 0 1) 0)
             (seq
              (setq sch (ptr-read-u8 spanbuf sp))
              (if (= sch 35)
                  0
                (if (if (>= sch 48) (if (< sch 58) 1 0) 0)
                    (setq scol (+ (* scol 16) (- sch 48)))
                  (if (if (>= sch 97) (if (< sch 103) 1 0) 0)
                      (setq scol (+ (* scol 16) (- sch 87)))
                    (if (if (>= sch 65) (if (< sch 71) 1 0) 0)
                        (setq scol (+ (* scol 16) (- sch 55)))
                      0))))
              (setq sp (+ sp 1))))
           (setq sp (+ sp 1))
           (if (= sfound 0)
               (if (< sstart ,line-end)
                   (if (> send lstart)
                       (seq (setq lcol scol) (setq sfound 1))
                     0)
                 0)
             0)))))))

(defun nemacs--face-span-color-expr ()
  "IR expression: span color when span data is present, legacy fallback
heuristic otherwise."
  `(if (> spn 0) lcol ,nemacs--legacy-line-color-form))

(defun nemacs--newline-draw-let-p (form)
  (and (consp form)
       (eq (car form) 'let*)
       (equal (cadr form) '((linelen (- i lstart)) (k 0)))))

(defun nemacs--patch-face-span-draw-sites (form)
  "Inject per-line lcol computation into both text draw sites and
swap the legacy color heuristic for the span color."
  (cond
   ;; Site A: the per-newline draw (let* ((linelen (- i lstart)) (k 0)) ...)
   ((and (nemacs--newline-draw-let-p form)
         (equal (car (cadr form)) '(linelen (- i lstart))))
    (let ((body (caddr form)))
      `(let* ,(cadr form)
         (seq
          ,(nemacs--face-span-line-color-form 'i)
          ,@(mapcar #'nemacs--patch-face-span-draw-sites (cdr body))))))
   ;; Site B: the trailing line draw (let* ((linelen (- tlen lstart)) (k 0)) ...)
   ((and (consp form)
         (eq (car form) 'let*)
         (equal (cadr form) '((linelen (- tlen lstart)) (k 0))))
    (let ((body (caddr form)))
      `(let* ,(cadr form)
         (seq
          ,(nemacs--face-span-line-color-form 'tlen)
          ,@(mapcar #'nemacs--patch-face-span-draw-sites (cdr body))))))
   ((equal form nemacs--legacy-line-color-form)
    (setq nemacs-face-span-color-replaced
          (+ nemacs-face-span-color-replaced 1))
    (nemacs--face-span-color-expr))
   ((consp form)
    (mapcar #'nemacs--patch-face-span-draw-sites form))
   (t form)))

(defun nemacs--patch-face-span-redisplay (form)
  "Add spanbuf/spn/lcol to the redisplay scope, read the span file
once per frame, color each drawn line from the spans, and release
the span buffer at the end of the frame."
  (cond
   ((nemacs--redisplay-let-p form)
    (setq nemacs-face-span-redisplay-patched
          (+ nemacs-face-span-redisplay-patched 1))
    (let* ((bindings (cadr form))
           (body (caddr form))
           (body-forms (cdr body))
           (bindings-with-spans
            (append bindings
                    '((spanbuf (syscall-direct 9 0 4096 3 34 -1 0))
                      (spn 0)
                      (lcol 0))))
           (body-with-read
            (nemacs--insert-after-first
             (nemacs--face-span-read-form)
             (lambda (body-form) (equal body-form '(setq ci ws)))
             body-forms))
           (body-patched
            (mapcar #'nemacs--patch-face-span-draw-sites body-with-read)))
      `(let* ,bindings-with-spans
         (seq
          ,@body-patched
          (syscall-direct 11 spanbuf 4096 0 0 0 0)))))
   ((consp form)
    (mapcar #'nemacs--patch-face-span-redisplay form))
   (t form)))

(defun nemacs--openfont-let-p (form)
  (and (consp form)
       (eq (car form) 'let*)
       (equal (cadr form) '((kspp (ptr-read-u8 kmap 1))))))

(defun nemacs--font-override-read-form ()
  "IR: if /tmp/nemacs-font names a font, copy it over the cfg font
bytes (cfgbuf offset 35) and retarget fnlen before OpenFont runs.
The substrate's fontset pick is applied at session start; live font
re-open mid-session stays a documented omission."
  `(seq
    ,@(nemacs--path-write-forms 'spath "/tmp/nemacs-font")
    (let* ((nfd (syscall-direct 257 -100 spath 0 0 0 0))
           (nn (if (>= nfd 0) (syscall-direct 0 nfd mb 256 0 0 0) 0))
           (nk 0))
      (seq
       (if (>= nfd 0) (syscall-direct 3 nfd 0 0 0 0 0) 0)
       ;; expect the "name\t" kv prefix: n a m e TAB
       (if (> nn 5)
           (if (= (ptr-read-u8 mb 0) 110)
               (if (= (ptr-read-u8 mb 1) 97)
                   (if (= (ptr-read-u8 mb 2) 109)
                       (if (= (ptr-read-u8 mb 3) 101)
                           (if (= (ptr-read-u8 mb 4) 9)
                               (seq
                                (while (if (< (+ 5 nk) nn)
                                           (if (< nk 80)
                                               (if (= (ptr-read-u8 mb (+ 5 nk)) 10) 0 1)
                                             0)
                                         0)
                                  (seq
                                   (ptr-write-u8 cfgbuf (+ 35 nk)
                                                 (ptr-read-u8 mb (+ 5 nk)))
                                   (setq nk (+ nk 1))))
                                (if (> nk 0) (setq fnlen nk) 0))
                             0)
                         0)
                     0)
                 0)
             0)
         0)))))

(defun nemacs--patch-font-openfont (form)
  (cond
   ((nemacs--openfont-let-p form)
    (setq nemacs-font-openfont-patched
          (+ nemacs-font-openfont-patched 1))
    (let* ((body (caddr form))
           (body-with-queryfont
            (nemacs--insert-after-first
             (nemacs--font-queryfont-form)
             (lambda (body-form)
               (equal body-form
                      '(syscall-direct 1 fd buf
                                       (+ 12 (* 4 (/ (+ fnlen 3) 4)))
                                       0 0 0)))
             (cdr body))))
      `(let* ,(cadr form)
         (seq
          ,(nemacs--font-override-read-form)
          ,@body-with-queryfont))))
   ((consp form)
    (mapcar #'nemacs--patch-font-openfont form))
   (t form)))

(defun nemacs--font-queryfont-form ()
  "IR: after OpenFont, ask the X server for the loaded font metrics
and cache the actual ImageText16 cell width in cfgbuf byte 200.

X_QueryFont replies use xQueryFontReply (Xproto.h): maxBounds begins
at byte 24, and characterWidth is the third INT16 in xCharInfo, so
the width is at reply byte 28.  The reply is variable length; drain
any unread tail so font properties/char infos do not remain queued as
fake events.

QueryTextExtents then measures the same CHAR2B path this GUI uses for
line drawing.  On HiDPI GNOME the scalar font width can differ from
the ImageText16 advance; the text-extents result is the one the caret
must follow."
  (setq nemacs-font-queryfont-patched
        (+ nemacs-font-queryfont-patched 1))
  `(seq
    (ptr-write-u8 buf 0 47)
    (ptr-write-u8 buf 1 0)
    (ptr-write-u16 buf 2 2)
    (ptr-write-u32 buf 4 fid)
    (syscall-direct 1 fd buf 8 0 0 0)
    (let* ((qrn (syscall-direct 0 fd reply 65536 0 0 0))
           (qcw (ptr-read-u16 reply 28))
           (qleft 0))
      (seq
       (if (if (>= qrn 8) (= (ptr-read-u8 reply 0) 1) 0)
           (setq qleft (- (+ 32 (* 4 (ptr-read-u32 reply 4))) qrn))
         0)
       (while (> qleft 0)
         (let* ((qchunk (if (> qleft 65536) 65536 qleft))
                (qdrn (syscall-direct 0 fd reply qchunk 0 0 0)))
           (if (> qdrn 0)
               (setq qleft (- qleft qdrn))
             (setq qleft 0))))
       (if (if (>= qrn 60)
               (if (= (ptr-read-u8 reply 0) 1)
                   (if (> qcw 0) (< qcw 256) 0)
                 0)
             0)
           (ptr-write-u8 cfgbuf 200 qcw)
         0)
       (let* ((qti 0))
         (seq
          (ptr-write-u8 buf 0 48)
          (ptr-write-u8 buf 1 0)
          (ptr-write-u16 buf 2 7)
          (ptr-write-u32 buf 4 fid)
          (while (< qti 10)
            (seq
             (ptr-write-u8 buf (+ 8 (* qti 2)) 0)
             (ptr-write-u8 buf (+ 9 (* qti 2)) 97)
             (setq qti (+ qti 1))))
          (syscall-direct 1 fd buf 28 0 0 0)
          (let* ((qtrn (syscall-direct 0 fd reply 32 0 0 0))
                 (qtw (/ (ptr-read-u32 reply 16) 10)))
            (if (if (>= qtrn 32)
                    (if (= (ptr-read-u8 reply 0) 1)
                        (if (> qtw 0) (< qtw 256) 0)
                      0)
                  0)
                (ptr-write-u8 cfgbuf 200 qtw)
              0))))
       ,@(nemacs--path-write-forms 'spath "/tmp/nemacs-cell-width")
       (let* ((cwout (ptr-read-u8 cfgbuf 200))
              (cwlen 0)
              (cwfd (syscall-direct 257 -100 spath 577 438 0 0)))
         (seq
          (if (if (> cwout 0) (< cwout 256) 0) 0 (setq cwout 9))
          (if (>= cwout 100)
              (seq
               (ptr-write-u8 mb 0 (+ 48 (/ cwout 100)))
               (ptr-write-u8 mb 1 (+ 48 (/ (- cwout (* (/ cwout 100) 100)) 10)))
               (ptr-write-u8 mb 2 (+ 48 (- cwout (* (/ cwout 10) 10))))
               (setq cwlen 3))
            (if (>= cwout 10)
                (seq
                 (ptr-write-u8 mb 0 (+ 48 (/ cwout 10)))
                 (ptr-write-u8 mb 1 (+ 48 (- cwout (* (/ cwout 10) 10))))
                 (setq cwlen 2))
              (seq
               (ptr-write-u8 mb 0 (+ 48 cwout))
               (setq cwlen 1))))
          (if (>= cwfd 0)
              (seq
               (syscall-direct 1 cwfd mb cwlen 0 0 0)
               (syscall-direct 3 cwfd 0 0 0 0 0))
            0)))))))

;; M16 CJK glyph lane: draw text with ImageText16 over UTF-8 decoded
;; to UCS-2 code units, and move the cursor by display cells.
;;
;; The substrate transports the fontset pick (nemacs-font: name +
;; script + cw cell width) and the cursor's display-cell offset
;; (nemacs-cursor: cells) — this GUI only decodes bytes to glyph
;; indices and paints.  The misc-fixed "ja" face advances CJK glyphs
;; at exactly twice the ASCII width, so the 2-cell model stays exact.
;; Known omission: hscroll + multibyte misreads continuation bytes
;; (the hscroll display shift rewrites only the lead-byte reads).

(defvar nemacs-cjk-text-draw-patched 0)
(defvar nemacs-cjk-cursor-scan-patched 0)
(defvar nemacs-cjk-cursor-width-patched 0)
(defvar nemacs-cjk-font-cw-patched 0)

(defun nemacs--imagetext8-block-p (forms)
  "Match the ImageText8 emission run: (ptr-write-u8 buf 0 76) ... send."
  (and (consp forms)
       (equal (car forms) '(ptr-write-u8 buf 0 76))))

(defun nemacs--cjk-imagetext16-forms (len-expr)
  "IR: decode [lstart, lstart+LEN-EXPR) UTF-8 bytes into CHAR2B at
buf+16 and emit one ImageText16 request.  Reuses k as the byte index
so the hscroll atom rewrite still shifts the lead-byte reads."
  `((let* ((n16 0)
           (cu 0)
           (b0 0))
      (seq
       (while (< k ,len-expr)
         (seq
          (setq b0 (ptr-read-u8 tb (+ lstart k)))
          (if (< b0 128)
              (seq (setq cu b0) (setq k (+ k 1)))
            (if (< b0 192)
                (seq (setq cu 65533) (setq k (+ k 1)))
              (if (< b0 224)
                  (seq
                   (setq cu (+ (* (- b0 192) 64)
                               (- (ptr-read-u8 tb (+ (+ lstart k) 1)) 128)))
                   (setq k (+ k 2)))
                (if (< b0 240)
                    (seq
                     (setq cu (+ (+ (* (- b0 224) 4096)
                                    (* (- (ptr-read-u8 tb (+ (+ lstart k) 1)) 128) 64))
                                 (- (ptr-read-u8 tb (+ (+ lstart k) 2)) 128)))
                     (setq k (+ k 3)))
                  (seq (setq cu 65533) (setq k (+ k 4)))))))
          (if (< n16 120)
              (seq
               (ptr-write-u8 buf (+ 16 (* n16 2)) (/ cu 256))
               (ptr-write-u8 buf (+ 17 (* n16 2)) (- cu (* (/ cu 256) 256)))
               (setq n16 (+ n16 1)))
            0)))
       (ptr-write-u8 buf 0 77)
       (ptr-write-u8 buf 1 n16)
       (ptr-write-u16 buf 2 (+ 4 (/ (+ (* 2 n16) 3) 4)))
       (ptr-write-u32 buf 4 pm)
       (ptr-write-u32 buf 8 gc)
       (ptr-write-u16 buf 12 12)
       (ptr-write-u16 buf 14 (+ 30 (* line 16)))
       (syscall-direct 1 fd buf (+ 16 (* 4 (/ (+ (* 2 n16) 3) 4))) 0 0 0)))))

(defun nemacs--patch-cjk-draw-list (forms)
  "Replace each ImageText8 emission run inside FORMS with the
ImageText16 decoder (only the per-line text draws use opcode 76
with the tb byte-copy loop)."
  (let ((out nil)
        (rest forms))
    (while rest
      (if (and (nemacs--imagetext8-block-p rest)
               ;; require the tb copy loop shape within the next forms
               ;; so modeline/minibuffer ImageText8 (mb-based) is left
               ;; alone: those copy from mb, not tb
               (let ((probe rest)
                     (found nil)
                     (steps 0))
                 (while (and probe (< steps 12) (not found))
                   (when (equal (car-safe probe)
                                '(while (< k linelen)
                                   (seq
                                    (ptr-write-u8 buf (+ 16 k)
                                                  (ptr-read-u8 tb (+ lstart k)))
                                    (setq k (+ k 1)))))
                     (setq found t))
                   (setq probe (cdr probe))
                   (setq steps (1+ steps)))
                 found))
          (progn
            (setq nemacs-cjk-text-draw-patched
                  (+ nemacs-cjk-text-draw-patched 1))
            ;; drop the original run: opcode write .. send syscall
            (let ((dropped 0))
              (while (and rest
                          (not (and (eq (car-safe (car rest)) 'syscall-direct)
                                    (equal (nth 1 (car rest)) 1))))
                (setq rest (cdr rest))
                (setq dropped (1+ dropped)))
              ;; also drop the send itself
              (when rest (setq rest (cdr rest))))
            (dolist (f (nemacs--cjk-imagetext16-forms 'linelen))
              (push f out)))
        (push (nemacs--patch-cjk-text-draw (car rest)) out)
        (setq rest (cdr rest))))
    (nreverse out)))

(defun nemacs--patch-cjk-text-draw (form)
  (if (consp form)
      (nemacs--patch-cjk-draw-list form)
    form))

(defconst nemacs--cursor-scan-form
  '(while (< ci pt2)
     (seq
      (if (= (ptr-read-u8 tb ci) 10)
          (seq (setq cl (+ cl 1)) (setq cc 0))
        (setq cc (+ cc 1)))
      (setq ci (+ ci 1))))
  "The byte-counting cursor scan the CJK lane replaces.")

(defconst nemacs--cjk-cursor-scan-form
  '(while (< ci pt2)
     (let* ((cb (ptr-read-u8 tb ci)))
       (seq
        (if (= cb 10)
            (seq (setq cl (+ cl 1)) (setq cc 0) (setq ci (+ ci 1)))
          (if (< cb 128)
              (seq (setq cc (+ cc 1)) (setq ci (+ ci 1)))
            (if (< cb 192)
                (setq ci (+ ci 1))
              (if (< cb 224)
                  (seq (setq cc (+ cc 1)) (setq ci (+ ci 2)))
                (if (< cb 240)
                    (seq (setq cc (+ cc 2)) (setq ci (+ ci 3)))
                  (seq (setq cc (+ cc 2)) (setq ci (+ ci 4)))))))))))
  "Cell-counting multibyte cursor scan (CJK = 2 cells).")

(defun nemacs--patch-cjk-cursor (form)
  "Swap the cursor byte scan for the cell scan and make every cursor
x position use the substrate's cw cell width (cfgbuf byte 200; 0
means the default 9px grid)."
  (cond
   ((equal form nemacs--cursor-scan-form)
    (setq nemacs-cjk-cursor-scan-patched
          (+ nemacs-cjk-cursor-scan-patched 1))
    nemacs--cjk-cursor-scan-form)
   ((and (consp form)
         (eq (car form) 'ptr-write-u16)
         (eq (nth 1 form) 'buf)
         (equal (nth 2 form) 12)
         (let ((v (nth 3 form)))
           (and (consp v) (eq (car v) '+) (equal (nth 1 v) 12)
                (consp (nth 2 v)) (eq (car (nth 2 v)) '*)
                (equal (nth 2 (nth 2 v)) 9))))
    (setq nemacs-cjk-cursor-width-patched
          (+ nemacs-cjk-cursor-width-patched 1))
    (let ((mult (nth 2 (nth 3 form))))
      `(ptr-write-u16 buf 12
                      (+ 12 (* ,(nth 1 mult)
                               (if (= (ptr-read-u8 cfgbuf 200) 0)
                                   9
                                 (ptr-read-u8 cfgbuf 200)))))))
   ((consp form)
    (mapcar #'nemacs--patch-cjk-cursor form))
   (t form)))

(defun nemacs--font-cw-read-form ()
  "IR: parse the cw\\t line of /tmp/nemacs-font (already in mb) into
cfgbuf byte 200.  Runs inside the font-override block where nn holds
the file length."
  `(let* ((ck 0)
          (cwv 0))
     (seq
      (while (< ck (- nn 3))
        (seq
         (if (if (= (ptr-read-u8 mb ck) 10)
                 (if (= (ptr-read-u8 mb (+ ck 1)) 99)
                     (if (= (ptr-read-u8 mb (+ ck 2)) 119)
                         (= (ptr-read-u8 mb (+ ck 3)) 9)
                       0)
                   0)
               0)
             (let* ((cj (+ ck 4)))
               (seq
                (while (if (< cj nn)
                           (if (= (ptr-read-u8 mb cj) 10) 0 1)
                         0)
                  (seq
                   (if (if (>= (ptr-read-u8 mb cj) 48)
                           (< (ptr-read-u8 mb cj) 58)
                         0)
                       (setq cwv (+ (* cwv 10) (- (ptr-read-u8 mb cj) 48)))
                     0)
                   (setq cj (+ cj 1))))
                (setq ck nn)))
           (setq ck (+ ck 1)))))
      (if (> cwv 0) (ptr-write-u8 cfgbuf 200 cwv) 0))))

(defun nemacs--patch-cjk-font-cw (form)
  "Extend the M12 font-override block: after the name copy, also
parse the cw hint into cfgbuf byte 200."
  (cond
   ((and (consp form)
         (eq (car form) 'if)
         (equal (cadr form) '(> nn 5)))
    (setq nemacs-cjk-font-cw-patched
          (+ nemacs-cjk-font-cw-patched 1))
    `(seq ,form ,(nemacs--font-cw-read-form)))
   ((consp form)
    (mapcar #'nemacs--patch-cjk-font-cw form))
   (t form)))

(defun nemacs--ptr-write-u8-p (form)
  (and (consp form)
       (eq (car form) 'ptr-write-u8)
       (symbolp (nth 1 form))
       (integerp (nth 2 form))
       (integerp (nth 3 form))))

(defun nemacs--transport-path-replacement (path transport-dir)
  (let* ((tmp-prefix (concat "/tmp" "/nemacs-"))
         (default-config (concat "/tmp" "/nemacs.cfg"))
         (config-path (getenv "NEMACS_CONFIG_PATH")))
    (cond
     ((and config-path
           (not (string= config-path ""))
           (string= path default-config))
      (let ((replacement (expand-file-name config-path)))
        (and (not (string= replacement path))
             replacement)))
     ((string-prefix-p tmp-prefix path)
      (let ((replacement
             (expand-file-name (file-name-nondirectory path) transport-dir)))
        (and (not (string= replacement path))
             replacement))))))

(defun nemacs--ptr-write-u8-path-run (forms transport-dir)
  (when (nemacs--ptr-write-u8-p (car forms))
    (let* ((var (nth 1 (car forms)))
           (idx (nth 2 (car forms)))
           (rest forms)
           (bytes nil)
           (done nil))
      (when (= idx 0)
        (while (and rest
                    (not done)
                    (nemacs--ptr-write-u8-p (car rest))
                    (eq (nth 1 (car rest)) var)
                    (= (nth 2 (car rest)) idx))
          (let ((byte (nth 3 (car rest))))
            (push byte bytes)
            (setq rest (cdr rest))
            (setq idx (+ idx 1))
            (when (= byte 0)
              (setq done t))))
        (when done
          (let* ((path-bytes (nreverse bytes))
                 (path (substring (apply #'string path-bytes) 0 -1))
                 (replacement
                  (nemacs--transport-path-replacement path transport-dir)))
            (when replacement
              (list var
                    (append
                     (cl-loop for byte across (string-as-unibyte replacement)
                              for i from 0
                              collect (list 'ptr-write-u8 var i byte))
                     (list (list 'ptr-write-u8 var (length replacement) 0)))
                    rest))))))))

(defun nemacs--rewrite-transport-path-list (forms transport-dir)
  (let ((out nil)
        (rest forms))
    (while rest
      (let ((run (nemacs--ptr-write-u8-path-run rest transport-dir)))
        (if run
            (progn
              (setq nemacs-transport-paths-rewritten
                    (+ nemacs-transport-paths-rewritten 1))
              (setq out (nconc (nreverse (cadr run)) out))
              (setq rest (caddr run)))
          (setq out
                (cons (nemacs--rewrite-transport-paths (car rest) transport-dir)
                      out))
          (setq rest (cdr rest)))))
    (nreverse out)))

(defun nemacs--rewrite-transport-paths (form transport-dir)
  (cond
   ((consp form)
    (nemacs--rewrite-transport-path-list form transport-dir))
   (t form)))

;; M20 view slice: the GUI's text/point/window-start reads move to the
;; bridge's view channel (nemacs-view / -view-point / -view-start).
;; The bridge rebases those to the visible slice, so buffers beyond
;; the 64KB text mmap and the u16 state words render correctly; small
;; buffers pass through whole and the values equal the old ones.
;; nemacs-buf stays the FULL-text round-trip channel for the runtime.

(defvar nemacs-view-paths-rewritten 0)

(defconst nemacs--view-path-map
  '(("/tmp/nemacs-buf" . "/tmp/nemacs-view")
    ("/tmp/nemacs-point" . "/tmp/nemacs-view-point")
    ("/tmp/nemacs-window-start" . "/tmp/nemacs-view-start")))

(defun nemacs--view-path-run (forms)
  "Like `nemacs--ptr-write-u8-path-run' but replaces by name map."
  (when (nemacs--ptr-write-u8-p (car forms))
    (let* ((var (nth 1 (car forms)))
           (idx (nth 2 (car forms)))
           (rest forms)
           (bytes nil)
           (done nil))
      (when (= idx 0)
        (while (and rest
                    (not done)
                    (nemacs--ptr-write-u8-p (car rest))
                    (eq (nth 1 (car rest)) var)
                    (= (nth 2 (car rest)) idx))
          (let ((byte (nth 3 (car rest))))
            (push byte bytes)
            (setq rest (cdr rest))
            (setq idx (+ idx 1))
            (when (= byte 0)
              (setq done t))))
        (when done
          (let* ((path (substring (apply #'string (nreverse bytes)) 0 -1))
                 (replacement (cdr (assoc path nemacs--view-path-map))))
            (when replacement
              (list var
                    (append
                     (cl-loop for byte across (string-as-unibyte replacement)
                              for i from 0
                              collect (list 'ptr-write-u8 var i byte))
                     (list (list 'ptr-write-u8 var (length replacement) 0)))
                    rest))))))))

(defun nemacs--patch-view-path-list (forms)
  (let ((out nil)
        (rest forms))
    (while rest
      (let ((run (nemacs--view-path-run rest)))
        (if run
            (progn
              (setq nemacs-view-paths-rewritten
                    (+ nemacs-view-paths-rewritten 1))
              (setq out (nconc (nreverse (cadr run)) out))
              (setq rest (caddr run)))
          (setq out (cons (nemacs--patch-view-paths (car rest)) out))
          (setq rest (cdr rest)))))
    (nreverse out)))

(defun nemacs--patch-view-paths (form)
  (if (consp form)
      (nemacs--patch-view-path-list form)
    form))

;; M20 view-slice read redirect: large buffers exceed the GUI's 64KB
;; text mmap, so the bridge publishes a window-start slice on the view
;; channel.  nemacs-buf / nemacs-point are bidirectional in the GUI
;; (read on render flags 0, write on edit flags 577); only the
;; READ-ONLY opens move to the view channel — the edit write-backs stay
;; on the original paths and the bridge splices them at view-rebase.
;; window-start is read-only here (46/0), so all its opens redirect.
;; Dedicated path buffers (viewp/vptp/vstp) are allocated alongside the
;; existing ones and seeded once at body start.

(defvar nemacs-view-readopen-patched 0)

(defun nemacs--path-bytes-form (var path)
  "Forms that write PATH (NUL-terminated) into buffer VAR."
  (append
   (cl-loop for byte across (string-as-unibyte path)
            for i from 0
            collect (list 'ptr-write-u8 var i byte))
   (list (list 'ptr-write-u8 var (length path) 0))))

(defun nemacs--inject-view-bindings (form)
  "Add viewp/vptp/vstp allocations right after the bufp binding."
  (cond
   ((and (consp form)
         (equal form '(bufp (syscall-direct 9 0 4096 3 34 -1 0))))
    ;; can't expand one binding into many in place via a scalar return;
    ;; handled at the list level by `nemacs--inject-view-binding-list'
    form)
   ((consp form)
    (nemacs--inject-view-binding-list form))
   (t form)))

(defun nemacs--inject-view-binding-list (forms)
  (let ((out nil)
        (rest forms))
    (while rest
      (let ((cur (car rest)))
        (if (equal cur '(bufp (syscall-direct 9 0 4096 3 34 -1 0)))
            (progn
              (push cur out)
              (push '(viewp (syscall-direct 9 0 4096 3 34 -1 0)) out)
              (push '(vptp (syscall-direct 9 0 4096 3 34 -1 0)) out)
              (push '(vstp (syscall-direct 9 0 4096 3 34 -1 0)) out))
          (push (nemacs--inject-view-bindings cur) out)))
      (setq rest (cdr rest)))
    (nreverse out)))

(defun nemacs--seed-view-paths (form)
  "Insert viewp/vptp/vstp path seeds after bufp's path write run."
  (cond
   ((and (consp form) (eq (car form) 'seq))
    (let ((out nil)
          (rest (cdr form))
          (seeded nil))
      (while rest
        (push (nemacs--seed-view-paths (car rest)) out)
        (when (and (not seeded)
                   (equal (car rest) '(ptr-write-u8 bufp 15 0)))
          (setq seeded t)
          (dolist (f (nemacs--path-bytes-form 'viewp "/tmp/nemacs-view"))
            (push f out))
          (dolist (f (nemacs--path-bytes-form 'vptp "/tmp/nemacs-view-point"))
            (push f out))
          (dolist (f (nemacs--path-bytes-form 'vstp "/tmp/nemacs-view-start"))
            (push f out)))
        (setq rest (cdr rest)))
      (cons 'seq (nreverse out))))
   ((consp form)
    (mapcar #'nemacs--seed-view-paths form))
   (t form)))

(defun nemacs--redirect-view-readopens (form)
  "Rewrite read-only opens of bufp/gotop/wstp to the view buffers.
A read open is (syscall-direct 257 -100 VAR 0 0 0 0); the flags=0 arg
distinguishes it from the write open (flags 577)."
  (cond
   ((and (consp form)
         (eq (car form) 'syscall-direct)
         (equal (nth 1 form) 257)
         (equal (nth 2 form) -100)
         (memq (nth 3 form) '(bufp gotop wstp))
         (equal (nth 4 form) 0))
    (setq nemacs-view-readopen-patched
          (+ nemacs-view-readopen-patched 1))
    (let ((repl (cond ((eq (nth 3 form) 'bufp) 'viewp)
                      ((eq (nth 3 form) 'gotop) 'vptp)
                      (t 'vstp))))
      (list 'syscall-direct 257 -100 repl 0 0 0 0)))
   ((consp form)
    (mapcar #'nemacs--redirect-view-readopens form))
   (t form)))

(defun nemacs--patch-view-readopen (form)
  "Full M20 GUI read redirect: bindings + path seeds + open rewrites."
  (let ((with-bindings (nemacs--inject-view-bindings form)))
    (nemacs--redirect-view-readopens
     (nemacs--seed-view-paths with-bindings))))

;; P4: X DISPLAY number support.  The IR hardcodes the X11 unix-socket
;; path "/tmp/.X11-unix/X0" with the display digit baked at sa[18]=48
;; ('0'), so the binary only ever reaches :0 and cannot run on a nested
;; X (Xephyr :N) for deterministic visual tests.  Bake the build's
;; NEMACS_X_DISPLAY_NUM (single digit, default 0) into that byte; unset
;; => 48 => :0, identical to before (transparent for normal builds).
(defvar nemacs-x-display-patched 0)

(defun nemacs--patch-x-display (form)
  "Rewrite the hardcoded sa[18]=48 ('0') X display digit to 48+N where
N = NEMACS_X_DISPLAY_NUM (default 0)."
  (let ((n (string-to-number (or (getenv "NEMACS_X_DISPLAY_NUM") "0"))))
    (cond
     ((and (consp form)
           (eq (car form) 'ptr-write-u8)
           (eq (nth 1 form) 'sa)
           (equal (nth 2 form) 18)
           (equal (nth 3 form) 48))
      (setq nemacs-x-display-patched (1+ nemacs-x-display-patched))
      (list 'ptr-write-u8 'sa 18 (+ 48 n)))
     ((consp form)
      (mapcar #'nemacs--patch-x-display form))
     (t form))))

;; M22 GUI side: decode X ButtonPress (event type 4).  A click in the
;; tool-bar/dropdown area writes "XXXX,YYYY" to nemacs-toolbar-click and forks
;; the bridge, which resolves it to either an opened dropdown or a selected
;; menu item.  Purely additive on et=4: the et=2 keypress branch is left
;; untouched.  Runs BEFORE the view read redirect so the readback's RO opens
;; follow the slice channel.
(defvar nemacs-buttonpress-patched 0)

(defun nemacs--inject-tbcp-binding-list (forms)
  "Allocate tbcp (the nemacs-toolbar-click path buffer) right after bufp."
  (let ((out nil) (rest forms))
    (while rest
      (let ((cur (car rest)))
        (if (equal cur '(bufp (syscall-direct 9 0 4096 3 34 -1 0)))
            (progn
              (push cur out)
              (push '(tbcp (syscall-direct 9 0 4096 3 34 -1 0)) out))
          (push (if (consp cur) (nemacs--inject-tbcp-binding-list cur) cur) out)))
      (setq rest (cdr rest)))
    (nreverse out)))

(defun nemacs--seed-tbcp-path (form)
  "Write the tbcp path string just after bufp's path-seed run."
  (cond
   ((and (consp form) (eq (car form) 'seq))
    (let ((out nil) (rest (cdr form)) (seeded nil))
      (while rest
        (push (nemacs--seed-tbcp-path (car rest)) out)
        (when (and (not seeded) (equal (car rest) '(ptr-write-u8 bufp 15 0)))
          (setq seeded t)
          (dolist (f (nemacs--path-bytes-form 'tbcp "/tmp/nemacs-toolbar-click"))
            (push f out)))
        (setq rest (cdr rest)))
      (cons 'seq (nreverse out))))
   ((consp form) (mapcar #'nemacs--seed-tbcp-path form))
   (t form)))

(defun nemacs--buttonpress-form ()
  "IR for the et=4 ButtonPress branch."
  '(if (< (ptr-read-u16 reply 26) 178)
       (seq
        (let* ((vx (ptr-read-u16 reply 24))
               (vy (ptr-read-u16 reply 26)))
          (seq (ptr-write-u8 mb 0 (+ 48 (mod (/ vx 1000) 10)))
               (ptr-write-u8 mb 1 (+ 48 (mod (/ vx 100) 10)))
               (ptr-write-u8 mb 2 (+ 48 (mod (/ vx 10) 10)))
               (ptr-write-u8 mb 3 (+ 48 (mod vx 10)))
               (ptr-write-u8 mb 4 44)
               (ptr-write-u8 mb 5 (+ 48 (mod (/ vy 1000) 10)))
               (ptr-write-u8 mb 6 (+ 48 (mod (/ vy 100) 10)))
               (ptr-write-u8 mb 7 (+ 48 (mod (/ vy 10) 10)))
               (ptr-write-u8 mb 8 (+ 48 (mod vy 10)))
               (let* ((tfd (syscall-direct 257 -100 tbcp 577 438 0 0)))
                 (seq (syscall-direct 1 tfd mb 9 0 0 0)
                      (syscall-direct 3 tfd 0 0 0 0 0)))))
        (let* ((pfd (syscall-direct 257 -100 gotop 577 438 0 0))
               (pv (ptr-read-u16 st 0)) (d0 (/ pv 10000)) (r0 (mod pv 10000))
               (d1 (/ r0 1000)) (r1 (mod r0 1000)) (d2 (/ r1 100))
               (r2 (mod r1 100)) (d3 (/ r2 10)) (d4 (mod r2 10)))
          (seq (ptr-write-u8 mb 0 (+ 48 d0)) (ptr-write-u8 mb 1 (+ 48 d1))
               (ptr-write-u8 mb 2 (+ 48 d2)) (ptr-write-u8 mb 3 (+ 48 d3))
               (ptr-write-u8 mb 4 (+ 48 d4))
               (syscall-direct 1 pfd mb 5 0 0 0) (syscall-direct 3 pfd 0 0 0 0 0)))
        (let* ((kfd (syscall-direct 257 -100 keyp 577 438 0 0)))
          (syscall-direct 3 kfd 0 0 0 0 0))
        (ptr-write-u64 argvb 0 shp) (ptr-write-u64 argvb 8 mxp)
        (ptr-write-u64 argvb 16 0) (ptr-write-u64 envb 0 0)
        (let* ((pid (syscall-direct 57 0 0 0 0 0 0)))
          (if (= pid 0)
              (seq (syscall-direct 59 shp argvb envb 0 0 0)
                   (syscall-direct 60 1 0 0 0 0 0))
            (syscall-direct 61 pid 0 0 0 0 0)))
        (let* ((rfd (syscall-direct 257 -100 bufp 0 0 0 0))
               (rn (if (>= rfd 0) (syscall-direct 0 rfd tb 60000 0 0 0) 0)))
          (seq (ptr-write-u16 st 2 rn)
               (let* ((pfd2 (syscall-direct 257 -100 gotop 0 0 0 0))
                      (pn2 (if (>= pfd2 0) (syscall-direct 0 pfd2 mb 64 0 0 0) 0)))
                 (seq (if (> pn2 0)
                          (let* ((pi2 0) (poff2 0))
                            (seq (while (< pi2 pn2)
                                   (let* ((pc2 (ptr-read-u8 mb pi2)))
                                     (seq (if (if (>= pc2 48) (if (< pc2 58) 1 0) 0)
                                              (setq poff2 (+ (* poff2 10) (- pc2 48))) 0)
                                          (setq pi2 (+ pi2 1)))))
                                 (ptr-write-u16 st 0 poff2))) 0)
                      (if (>= pfd2 0) (syscall-direct 3 pfd2 0 0 0 0 0) 0)))
               (syscall-direct 3 rfd 0 0 0 0 0)))
        (let* ((tfd3 (syscall-direct 257 -100 wstp 0 0 0 0))
               (tn3 (if (>= tfd3 0) (syscall-direct 0 tfd3 mb 64 0 0 0) 0)))
          (seq (if (> tn3 0)
                   (let* ((ti3 0) (toff3 0))
                     (seq (while (< ti3 tn3)
                            (let* ((tc3 (ptr-read-u8 mb ti3)))
                              (seq (if (if (>= tc3 48) (if (< tc3 58) 1 0) 0)
                                       (setq toff3 (+ (* toff3 10) (- tc3 48))) 0)
                                   (setq ti3 (+ ti3 1)))))
                          (ptr-write-u16 st 16 toff3))) 0)
               (if (>= tfd3 0) (syscall-direct 3 tfd3 0 0 0 0 0) 0))))
     0))

(defun nemacs--restructure-et4 (form)
  "Wrap the et=2 if so a et=4 ButtonPress branch precedes its else arm."
  (cond
   ((and (consp form) (eq (car form) 'if) (equal (nth 1 form) '(= et 2)))
    (setq nemacs-buttonpress-patched (1+ nemacs-buttonpress-patched))
    (list 'if '(= et 2) (nth 2 form)
          (list 'if '(= et 4) (nemacs--buttonpress-form) (nth 3 form))))
   ((consp form) (mapcar #'nemacs--restructure-et4 form))
   (t form)))

(defun nemacs--patch-event-mask (form)
  "Add ButtonPress (0x4) to the CreateWindow event-mask so the window
actually receives clicks.  The IR sets it to 32769 (Exposure|KeyPress);
make it 32773 (Exposure|KeyPress|ButtonPress)."
  (cond
   ((and (consp form) (eq (car form) 'ptr-write-u32)
         (eq (nth 1 form) 'buf) (equal (nth 2 form) 40) (equal (nth 3 form) 32769))
    (list 'ptr-write-u32 'buf 40 32773))
   ((consp form) (mapcar #'nemacs--patch-event-mask form))
   (t form)))

(defun nemacs--patch-buttonpress (form)
  "Full M22 GUI ButtonPress: event mask + tbcp binding + path seed + et=4."
  (nemacs--restructure-et4
   (nemacs--seed-tbcp-path
    (nemacs--inject-tbcp-binding-list
     (nemacs--patch-event-mask form)))))

;; M21 minibuffer delegation (render slice): the bridge runs the real
;; minibuffer (prompt, completion candidates, history); the GUI's local
;; st6 box never showed any of it.  This draws the bridge minibuffer
;; state at the echo area when nemacs-minibuffer-active = "1": the
;; candidates a few lines up, then "prompt + input" on the bottom line,
;; via ImageText16 so Japanese prompts/input/candidates render.
;; Purely additive — it only paints when the bridge minibuffer is
;; active, which the input-route slice turns on; until then it is a
;; no-op overlay drawn just before the final CopyArea blit.

(defvar nemacs-minibuffer-render-patched 0)

(defun nemacs--set-gc-bg-form (color)
  "IR: ChangeGC GCBackground (mask 8) = COLOR, so ImageText cells use
COLOR as their background instead of the persistent CreateGC bg."
  `(seq
    (ptr-write-u8 buf 0 56) (ptr-write-u8 buf 1 0)
    (ptr-write-u16 buf 2 4) (ptr-write-u32 buf 4 gc)
    (ptr-write-u32 buf 8 8) (ptr-write-u32 buf 12 ,color)
    (syscall-direct 1 fd buf 16 0 0 0)))

(defun nemacs--mb-imagetext16-at (srcvar lenvar x y)
  "IR: decode LENVAR bytes of UTF-8 from SRCVAR into CHAR2B at buf+16
and emit one ImageText16 (opcode 77) at pixel X,Y on the pixmap."
  `(let* ((n16 0) (mk 0) (cu 0) (b0 0))
     (seq
      (while (< mk ,lenvar)
        (seq
         (setq b0 (ptr-read-u8 ,srcvar mk))
         (if (< b0 128)
             (seq (setq cu b0) (setq mk (+ mk 1)))
           (if (< b0 192)
               (seq (setq cu 65533) (setq mk (+ mk 1)))
             (if (< b0 224)
                 (seq
                  (setq cu (+ (* (- b0 192) 64)
                              (- (ptr-read-u8 ,srcvar (+ mk 1)) 128)))
                  (setq mk (+ mk 2)))
               (if (< b0 240)
                   (seq
                    (setq cu (+ (+ (* (- b0 224) 4096)
                                   (* (- (ptr-read-u8 ,srcvar (+ mk 1)) 128) 64))
                                (- (ptr-read-u8 ,srcvar (+ mk 2)) 128)))
                    (setq mk (+ mk 3)))
                 (seq (setq cu 65533) (setq mk (+ mk 4)))))))
         (if (< n16 250)
             (seq
              (ptr-write-u8 buf (+ 16 (* n16 2)) (/ cu 256))
              (ptr-write-u8 buf (+ 17 (* n16 2)) (- cu (* (/ cu 256) 256)))
              (setq n16 (+ n16 1)))
           0)))
      (if (> n16 0)
          (seq
           (ptr-write-u8 buf 0 77)
           (ptr-write-u8 buf 1 n16)
           (ptr-write-u16 buf 2 (+ 4 (/ (+ (* 2 n16) 3) 4)))
           (ptr-write-u32 buf 4 pm)
           (ptr-write-u32 buf 8 gc)
           (ptr-write-u16 buf 12 ,x)
           (ptr-write-u16 buf 14 ,y)
           (syscall-direct 1 fd buf (+ 16 (* 4 (/ (+ (* 2 n16) 3) 4))) 0 0 0))
        0))))

(defun nemacs--mb-read-file-form (pathvar dstvar lenvar cap)
  "IR: read up to CAP bytes of file PATHVAR into DSTVAR; set LENVAR."
  `(let* ((mfd (syscall-direct 257 -100 ,pathvar 0 0 0 0)))
     (seq
      (if (>= mfd 0)
          (setq ,lenvar (syscall-direct 0 mfd ,dstvar ,cap 0 0 0))
        (setq ,lenvar 0))
      (if (< ,lenvar 0) (setq ,lenvar 0) 0)
      (if (>= mfd 0) (syscall-direct 3 mfd 0 0 0 0 0) 0))))

(defun nemacs--mb-echo-area-form ()
  "IR: when the bridge minibuffer is active, paint prompt+input on the
echo line and a few candidates above it.  Uses mb as scratch."
  `(let* ((mblen2 0) (active2 0))
     (seq
      ;; the echo area (bottom 16px, below the shifted mode line) is
      ;; always present and cleared to bg — like Emacs's blank echo
      ;; line — so buffer text never bleeds into it
      (ptr-write-u8 buf 0 56) (ptr-write-u8 buf 1 0)
      (ptr-write-u16 buf 2 4) (ptr-write-u32 buf 4 gc)
      (ptr-write-u32 buf 8 4) (ptr-write-u32 buf 12 bg)
      (syscall-direct 1 fd buf 16 0 0 0)
      (ptr-write-u8 buf 0 70) (ptr-write-u8 buf 1 0)
      (ptr-write-u16 buf 2 5) (ptr-write-u32 buf 4 pm)
      (ptr-write-u32 buf 8 gc) (ptr-write-u16 buf 12 0)
      (ptr-write-u16 buf 14 (- wh 16)) (ptr-write-u16 buf 16 ww)
      (ptr-write-u16 buf 18 16) (syscall-direct 1 fd buf 20 0 0 0)
      ,(nemacs--mb-read-file-form 'mbap 'mb 'mblen2 4)
      (setq active2 (if (> mblen2 0) (if (= (ptr-read-u8 mb 0) 49) 1 0) 0))
      (if (= active2 1)
          (let* ((plen 0) (slen2 0) (clen 0) (ci2 0) (crow 0) (cstart 0))
            (seq
             ;; text foreground = fg, cell background = echo bg
             ,(nemacs--set-gc-bg-form 'bg)
             (ptr-write-u8 buf 0 56) (ptr-write-u8 buf 1 0)
             (ptr-write-u16 buf 2 4) (ptr-write-u32 buf 4 gc)
             (ptr-write-u32 buf 8 4) (ptr-write-u32 buf 12 fg)
             (syscall-direct 1 fd buf 16 0 0 0)
             ;; prompt then input on the echo line (mb scratch each)
             ,(nemacs--mb-read-file-form 'mbpp 'mb 'plen 512)
             ,(nemacs--mb-imagetext16-at 'mb 'plen 4 '(- wh 6))
             ,(nemacs--mb-read-file-form 'mbsp 'mb 'slen2 512)
             ,(nemacs--mb-imagetext16-at 'mb 'slen2 '(+ 4 (* plen 9)) '(- wh 6))
             ;; up to 3 candidate lines above the echo line
             ,(nemacs--mb-read-file-form 'mbcp 'mb 'clen 1024)
             (while (if (< ci2 clen) (< crow 3) 0)
               (let* ((lstart2 ci2) (llen 0))
                 (seq
                  (while (if (< ci2 clen) (if (= (ptr-read-u8 mb ci2) 10) 0 1) 0)
                    (setq ci2 (+ ci2 1)))
                  (setq llen (- ci2 lstart2))
                  (if (> llen 0)
                      (let* ((n16 0) (mk lstart2) (cu 0) (b0 0))
                        (seq
                         (while (< mk ci2)
                           (seq
                            (setq b0 (ptr-read-u8 mb mk))
                            (if (< b0 128)
                                (seq (setq cu b0) (setq mk (+ mk 1)))
                              (if (< b0 192)
                                  (seq (setq cu 65533) (setq mk (+ mk 1)))
                                (if (< b0 224)
                                    (seq (setq cu (+ (* (- b0 192) 64) (- (ptr-read-u8 mb (+ mk 1)) 128))) (setq mk (+ mk 2)))
                                  (if (< b0 240)
                                      (seq (setq cu (+ (+ (* (- b0 224) 4096) (* (- (ptr-read-u8 mb (+ mk 1)) 128) 64)) (- (ptr-read-u8 mb (+ mk 2)) 128))) (setq mk (+ mk 3)))
                                    (seq (setq cu 65533) (setq mk (+ mk 4)))))))
                            (if (< n16 250)
                                (seq
                                 (ptr-write-u8 buf (+ 16 (* n16 2)) (/ cu 256))
                                 (ptr-write-u8 buf (+ 17 (* n16 2)) (- cu (* (/ cu 256) 256)))
                                 (setq n16 (+ n16 1)))
                              0)))
                         (if (> n16 0)
                             (seq
                              (ptr-write-u8 buf 0 77) (ptr-write-u8 buf 1 n16)
                              (ptr-write-u16 buf 2 (+ 4 (/ (+ (* 2 n16) 3) 4)))
                              (ptr-write-u32 buf 4 pm) (ptr-write-u32 buf 8 gc)
                              (ptr-write-u16 buf 12 4)
                              (ptr-write-u16 buf 14 (- wh (+ 22 (* crow 16))))
                              (syscall-direct 1 fd buf (+ 16 (* 4 (/ (+ (* 2 n16) 3) 4))) 0 0 0))
                           0)))
                    0)
                  (setq ci2 (+ ci2 1))
                  (setq crow (+ crow 1)))))))
        0))))

(defun nemacs--inject-mb-binding-list (forms)
  (let ((out nil) (rest forms))
    (while rest
      (let ((cur (car rest)))
        (if (equal cur '(bufp (syscall-direct 9 0 4096 3 34 -1 0)))
            (progn
              (push cur out)
              (push '(mbap (syscall-direct 9 0 4096 3 34 -1 0)) out)
              (push '(mbpp (syscall-direct 9 0 4096 3 34 -1 0)) out)
              (push '(mbsp (syscall-direct 9 0 4096 3 34 -1 0)) out)
              (push '(mbcp (syscall-direct 9 0 4096 3 34 -1 0)) out))
          (push (nemacs--inject-mb-bindings cur) out)))
      (setq rest (cdr rest)))
    (nreverse out)))

(defun nemacs--inject-mb-bindings (form)
  (if (consp form) (nemacs--inject-mb-binding-list form) form))

(defun nemacs--seed-mb-paths (form)
  (cond
   ((and (consp form) (eq (car form) 'seq))
    (let ((out nil) (rest (cdr form)) (seeded nil))
      (while rest
        (push (nemacs--seed-mb-paths (car rest)) out)
        (when (and (not seeded) (equal (car rest) '(ptr-write-u8 bufp 15 0)))
          (setq seeded t)
          (dolist (f (nemacs--path-bytes-form 'mbap "/tmp/nemacs-minibuffer-active")) (push f out))
          (dolist (f (nemacs--path-bytes-form 'mbpp "/tmp/nemacs-minibuffer-prompt")) (push f out))
          (dolist (f (nemacs--path-bytes-form 'mbsp "/tmp/nemacs-minibuffer-state")) (push f out))
          (dolist (f (nemacs--path-bytes-form 'mbcp "/tmp/nemacs-minibuffer-candidates")) (push f out)))
        (setq rest (cdr rest)))
      (cons 'seq (nreverse out))))
   ((consp form) (mapcar #'nemacs--seed-mb-paths form))
   (t form)))

(defun nemacs--mb-copyarea-to-win-p (last)
  (and (consp last)
       (let ((s (prin1-to-string last)))
         (and (string-match-p "ptr-write-u8 buf 0 62" s)
              (string-match-p "ptr-write-u32 buf 8 win" s)))))

(defun nemacs--insert-mb-draw-in-seq (form)
  "Within the redisplay let* body seq, insert the minibuffer draw just
before the final CopyArea-to-win blit, which is a flat run of forms
starting at the LAST (ptr-write-u8 buf 0 62) followed by
(ptr-write-u32 buf 8 win)."
  (if (and (consp form) (eq (car form) 'seq))
      (let* ((body (cdr form))
             (idx -1)
             (i 0))
        ;; find the last opcode-62 form whose blit targets `win`
        (dolist (f body)
          (when (and (equal f '(ptr-write-u8 buf 0 62))
                     (let ((rest (nthcdr (1+ i) body))
                           (hit nil)
                           (k 0))
                       (while (and rest (< k 10) (not hit))
                         (when (equal (car rest) '(ptr-write-u32 buf 8 win))
                           (setq hit t))
                         (setq rest (cdr rest))
                         (setq k (1+ k)))
                       hit))
            (setq idx i))
          (setq i (1+ i)))
        (if (>= idx 0)
            (progn
              (setq nemacs-minibuffer-render-patched
                    (+ nemacs-minibuffer-render-patched 1))
              (cons 'seq
                    (append (cl-subseq body 0 idx)
                            (list (nemacs--mb-echo-area-form))
                            (nthcdr idx body))))
          (cons 'seq (mapcar #'nemacs--insert-mb-draw-in-seq body))))
    (if (consp form) (mapcar #'nemacs--insert-mb-draw-in-seq form) form)))

(defun nemacs--apply-mb-echo-area (form)
  "Inject mb path buffers + seeds, then insert the draw only inside the
redisplay let* (where gc/pm/fg/bg/wh/ww are in scope)."
  (let ((with-paths (nemacs--seed-mb-paths
                     (nemacs--inject-mb-bindings form))))
    (nemacs--patch-mb-redisplay with-paths)))

(defun nemacs--patch-mb-redisplay (form)
  (cond
   ((nemacs--redisplay-let-p form)
    (let ((bindings (cadr form))
          (body (caddr form)))
      `(let* ,bindings ,(nemacs--insert-mb-draw-in-seq body))))
   ((consp form) (mapcar #'nemacs--patch-mb-redisplay form))
   (t form)))

;; M21b echo-area separation: real Emacs has the mode line as a grey
;; bar with a SEPARATE echo/minibuffer line below it.  This GUI drew
;; the mode line in the bottom 22px and the minibuffer overwrote it.
;; Shift the original mode-line draws up by 16px (bar -> wh-38, text
;; -> wh-22), leaving the bottom 16px as a dedicated echo area where
;; the minibuffer render (wh-16 clear, wh-6 text) now lands without
;; clobbering the mode line.  Applied BEFORE the minibuffer/toolbar
;; patches so it only rewrites the original mode-line y-coordinates.

(defvar nemacs-modeline-shift-patched 0)

(defun nemacs--patch-modeline-shift (form)
  "Shift the mode-line bar (wh-22 fill) up to wh-38, and the mode-line
TEXT (wh-6 draws preceded by x=12 or x=ww/2) up to wh-22 + draw it on
the grey bar (GC bg = ml).  The local minibuffer text (wh-6, x=4) is
left in place — it belongs in the echo area below the bar."
  (cond
   ((and (consp form) (eq (car form) 'seq))
    (let ((out nil) (rest (cdr form)) (lastx nil))
      (while rest
        (let ((f (car rest)))
          (cond
           ((and (consp f) (eq (car f) 'ptr-write-u16)
                 (equal (nth 1 f) 'buf) (equal (nth 2 f) 12))
            (setq lastx (nth 3 f))
            (push (nemacs--patch-modeline-shift f) out))
           ((equal f '(ptr-write-u16 buf 14 (- wh 22)))
            (setq nemacs-modeline-shift-patched
                  (+ nemacs-modeline-shift-patched 1))
            (push '(ptr-write-u16 buf 14 (- wh 38)) out))
           ((and (equal f '(ptr-write-u16 buf 14 (- wh 6)))
                 (or (equal lastx 12) (equal lastx '(/ ww 2))))
            (setq nemacs-modeline-shift-patched
                  (+ nemacs-modeline-shift-patched 1))
            ;; mode-line text one row up onto the grey bar
            (push '(ptr-write-u16 buf 14 (- wh 22)) out))
           (t (push (nemacs--patch-modeline-shift f) out))))
        (setq rest (cdr rest)))
      (cons 'seq (nreverse out))))
   ((consp form) (mapcar #'nemacs--patch-modeline-shift form))
   (t form)))

;; M22 tool bar render: the bridge owns the tool-bar definition
;; (nemacs-toolbar = LABEL<TAB>KEYS lines); the GUI paints a strip at
;; the very top with each button's LABEL.  Click->command is the
;; follow-up (needs X11 ButtonPress, which the loop does not decode
;; yet); the KEYS half is transported now so that wiring lands without
;; a bridge change.  Labels draw via ImageText16 (Japanese-safe).

(defvar nemacs-toolbar-render-patched 0)

(defun nemacs--toolbar-draw-menu-form ()
  "IR: paint an open toolbar dropdown from /tmp/nemacs-toolbar-menu.
The bridge writes LABEL<TAB>KEYS rows for the currently open menu."
  `(seq
    ,@(nemacs--path-write-forms 'tbp "/tmp/nemacs-toolbar-menu")
    (let* ((tmlen 0))
      (seq
       ,(nemacs--mb-read-file-form 'tbp 'mb 'tmlen 1024)
       (if (> tmlen 0)
           (seq
            ;; menu panel
            (ptr-write-u8 buf 0 56) (ptr-write-u8 buf 1 0)
            (ptr-write-u16 buf 2 4) (ptr-write-u32 buf 4 gc)
            (ptr-write-u32 buf 8 4) (ptr-write-u32 buf 12 bg)
            (syscall-direct 1 fd buf 16 0 0 0)
            (ptr-write-u8 buf 0 70) (ptr-write-u8 buf 1 0)
            (ptr-write-u16 buf 2 5) (ptr-write-u32 buf 4 pm)
            (ptr-write-u32 buf 8 gc) (ptr-write-u16 buf 12 6)
            (ptr-write-u16 buf 14 18) (ptr-write-u16 buf 16 260)
            (ptr-write-u16 buf 18 112) (syscall-direct 1 fd buf 20 0 0 0)
            ;; top/left accent border
            (ptr-write-u8 buf 0 56) (ptr-write-u8 buf 1 0)
            (ptr-write-u16 buf 2 4) (ptr-write-u32 buf 4 gc)
            (ptr-write-u32 buf 8 4) (ptr-write-u32 buf 12 ml)
            (syscall-direct 1 fd buf 16 0 0 0)
            (ptr-write-u8 buf 0 70) (ptr-write-u8 buf 1 0)
            (ptr-write-u16 buf 2 5) (ptr-write-u32 buf 4 pm)
            (ptr-write-u32 buf 8 gc) (ptr-write-u16 buf 12 6)
            (ptr-write-u16 buf 14 18) (ptr-write-u16 buf 16 260)
            (ptr-write-u16 buf 18 2) (syscall-direct 1 fd buf 20 0 0 0)
            (ptr-write-u8 buf 0 70) (ptr-write-u8 buf 1 0)
            (ptr-write-u16 buf 2 5) (ptr-write-u32 buf 4 pm)
            (ptr-write-u32 buf 8 gc) (ptr-write-u16 buf 12 6)
            (ptr-write-u16 buf 14 18) (ptr-write-u16 buf 16 2)
            (ptr-write-u16 buf 18 112) (syscall-direct 1 fd buf 20 0 0 0)
            ;; menu labels, ASCII by construction
            ,(nemacs--set-gc-bg-form 'bg)
            (ptr-write-u8 buf 0 56) (ptr-write-u8 buf 1 0)
            (ptr-write-u16 buf 2 4) (ptr-write-u32 buf 4 gc)
            (ptr-write-u32 buf 8 4) (ptr-write-u32 buf 12 fg)
            (syscall-direct 1 fd buf 16 0 0 0)
            (let* ((mi 0) (row 0))
              (while (if (< mi tmlen) (< row 7) 0)
                (let* ((ls mi) (llen 0) (mk 0))
                  (seq
                   (while (if (< mi tmlen)
                              (if (= (ptr-read-u8 mb mi) 9) 0
                                (if (= (ptr-read-u8 mb mi) 10) 0 1))
                            0)
                     (setq mi (+ mi 1)))
                   (setq llen (- mi ls))
                   (if (> llen 30) (setq llen 30) 0)
                   (if (> llen 0)
                       (seq
                        (ptr-write-u8 buf 0 76) (ptr-write-u8 buf 1 llen)
                        (ptr-write-u16 buf 2 (+ 4 (/ (+ llen 3) 4)))
                        (ptr-write-u32 buf 4 pm) (ptr-write-u32 buf 8 gc)
                        (ptr-write-u16 buf 12 14)
                        (ptr-write-u16 buf 14 (+ 34 (* row 16)))
                        (while (< mk llen)
                          (seq
                           (ptr-write-u8 buf (+ 16 mk) (ptr-read-u8 mb (+ ls mk)))
                           (setq mk (+ mk 1))))
                        (syscall-direct 1 fd buf (+ 16 (* 4 (/ (+ llen 3) 4))) 0 0 0))
                     0)
                   (while (if (< mi tmlen) (if (= (ptr-read-u8 mb mi) 10) 0 1) 0)
                     (setq mi (+ mi 1)))
                   (if (if (< mi tmlen) (= (ptr-read-u8 mb mi) 10) 0)
                       (setq mi (+ mi 1))
                     0)
                   (setq row (+ row 1))))))
            ,(nemacs--set-gc-bg-form 'bg))
         0)))))

(defun nemacs--toolbar-draw-form ()
  "IR: paint the tool-bar strip (y 0..18) from /tmp/nemacs-toolbar.
Each LABEL (up to the TAB) becomes a button drawn left to right."
  `(let* ((tblen 0))
     (seq
      ,(nemacs--mb-read-file-form 'tbp 'mb 'tblen 1024)
      ;; grey strip background across the top
      (ptr-write-u8 buf 0 56) (ptr-write-u8 buf 1 0)
      (ptr-write-u16 buf 2 4) (ptr-write-u32 buf 4 gc)
      (ptr-write-u32 buf 8 4) (ptr-write-u32 buf 12 ml)
      (syscall-direct 1 fd buf 16 0 0 0)
      (ptr-write-u8 buf 0 70) (ptr-write-u8 buf 1 0)
      (ptr-write-u16 buf 2 5) (ptr-write-u32 buf 4 pm)
      (ptr-write-u32 buf 8 gc) (ptr-write-u16 buf 12 0)
      (ptr-write-u16 buf 14 0) (ptr-write-u16 buf 16 ww)
      (ptr-write-u16 buf 18 18) (syscall-direct 1 fd buf 20 0 0 0)
      ;; button labels: fg text on the grey strip (cell bg = ml)
      ,(nemacs--set-gc-bg-form 'ml)
      (ptr-write-u8 buf 0 56) (ptr-write-u8 buf 1 0)
      (ptr-write-u16 buf 2 4) (ptr-write-u32 buf 4 gc)
      (ptr-write-u32 buf 8 4) (ptr-write-u32 buf 12 fg)
      (syscall-direct 1 fd buf 16 0 0 0)
      (let* ((ti 0) (tx 6) (tcw (ptr-read-u8 cfgbuf 200)))
        (if (if (> tcw 0) (< tcw 256) 0) 0 (setq tcw 9))
        (while (< ti tblen)
          (let* ((lstart3 ti) (n16 0) (b0 0) (cu 0))
            (seq
             ;; label = bytes up to TAB
             (while (if (< ti tblen) (if (= (ptr-read-u8 mb ti) 9) 0 (if (= (ptr-read-u8 mb ti) 10) 0 1)) 0)
               (setq ti (+ ti 1)))
             ;; decode [lstart3, ti) to CHAR2B
             (let* ((mk lstart3))
               (while (< mk ti)
                 (seq
                  (setq b0 (ptr-read-u8 mb mk))
                  (if (< b0 128)
                      (seq (setq cu b0) (setq mk (+ mk 1)))
                    (if (< b0 224)
                        (seq (setq cu (+ (* (- b0 192) 64) (- (ptr-read-u8 mb (+ mk 1)) 128))) (setq mk (+ mk 2)))
                      (seq (setq cu (+ (+ (* (- b0 224) 4096) (* (- (ptr-read-u8 mb (+ mk 1)) 128) 64)) (- (ptr-read-u8 mb (+ mk 2)) 128))) (setq mk (+ mk 3)))))
                  (if (< n16 60)
                      (seq
                       (ptr-write-u8 buf (+ 16 (* n16 2)) (/ cu 256))
                       (ptr-write-u8 buf (+ 17 (* n16 2)) (- cu (* (/ cu 256) 256)))
                       (setq n16 (+ n16 1)))
                    0))))
             (if (> n16 0)
                 (seq
                  (ptr-write-u8 buf 0 77) (ptr-write-u8 buf 1 n16)
                  (ptr-write-u16 buf 2 (+ 4 (/ (+ (* 2 n16) 3) 4)))
                  (ptr-write-u32 buf 4 pm) (ptr-write-u32 buf 8 gc)
                  (ptr-write-u16 buf 12 tx) (ptr-write-u16 buf 14 13)
                  (syscall-direct 1 fd buf (+ 16 (* 4 (/ (+ (* 2 n16) 3) 4))) 0 0 0)
                  (setq tx (+ tx (+ 14 (* n16 tcw)))))
               0)
             ;; skip to next line (past TAB + KEYS + newline)
             (while (if (< ti tblen) (if (= (ptr-read-u8 mb ti) 10) 0 1) 0)
               (setq ti (+ ti 1)))
             (setq ti (+ ti 1)))))
        ;; restore the GC background so the next frame's buffer text
        ;; keeps the theme bg
        ,(nemacs--set-gc-bg-form 'bg))
      ,(nemacs--toolbar-draw-menu-form))))

(defun nemacs--inject-tb-binding-list (forms)
  (let ((out nil) (rest forms))
    (while rest
      (let ((cur (car rest)))
        (if (equal cur '(bufp (syscall-direct 9 0 4096 3 34 -1 0)))
            (progn
              (push cur out)
              (push '(tbp (syscall-direct 9 0 4096 3 34 -1 0)) out))
          (push (nemacs--inject-tb-bindings cur) out)))
      (setq rest (cdr rest)))
    (nreverse out)))
(defun nemacs--inject-tb-bindings (form)
  (if (consp form) (nemacs--inject-tb-binding-list form) form))

(defun nemacs--seed-tb-paths (form)
  (cond
   ((and (consp form) (eq (car form) 'seq))
    (let ((out nil) (rest (cdr form)) (seeded nil))
      (while rest
        (push (nemacs--seed-tb-paths (car rest)) out)
        (when (and (not seeded) (equal (car rest) '(ptr-write-u8 bufp 15 0)))
          (setq seeded t)
          (dolist (f (nemacs--path-bytes-form 'tbp "/tmp/nemacs-toolbar")) (push f out)))
        (setq rest (cdr rest)))
      (cons 'seq (nreverse out))))
   ((consp form) (mapcar #'nemacs--seed-tb-paths form))
   (t form)))

(defun nemacs--insert-toolbar-in-redisplay (form)
  "Insert the toolbar draw just before the final CopyArea-to-win blit
in the redisplay let* (drawn on top, at the y 0..18 strip)."
  (cond
   ((nemacs--redisplay-let-p form)
    (let ((bindings (cadr form))
          (body (caddr form)))
      (if (and (consp body) (eq (car body) 'seq))
          (let* ((forms (cdr body))
                 (idx -1) (i 0))
            (dolist (f forms)
              (when (and (equal f '(ptr-write-u8 buf 0 62))
                         (let ((rest (nthcdr (1+ i) forms)) (hit nil) (k 0))
                           (while (and rest (< k 10) (not hit))
                             (when (equal (car rest) '(ptr-write-u32 buf 8 win)) (setq hit t))
                             (setq rest (cdr rest)) (setq k (1+ k)))
                           hit))
                (setq idx i))
              (setq i (1+ i)))
            (if (>= idx 0)
                (progn
                  (setq nemacs-toolbar-render-patched
                        (+ nemacs-toolbar-render-patched 1))
                  `(let* ,bindings
                     ,(cons 'seq
                            (append (cl-subseq forms 0 idx)
                                    (list (nemacs--toolbar-draw-form))
                                    (nthcdr idx forms)))))
              form))
        form)))
   ((consp form) (mapcar #'nemacs--insert-toolbar-in-redisplay form))
   (t form)))

(defun nemacs--patch-toolbar-render (form)
  (nemacs--insert-toolbar-in-redisplay
   (nemacs--seed-tb-paths
    (nemacs--inject-tb-bindings form))))

(defun nemacs--ptr-write-u8-paths (form)
  (let ((paths nil))
    (cl-labels
        ((scan-list
          (forms)
          (while forms
            (if (and (nemacs--ptr-write-u8-p (car forms))
                     (= (nth 2 (car forms)) 0))
                (let ((var (nth 1 (car forms)))
                      (idx 0)
                      (rest forms)
                      (bytes nil)
                      (done nil))
                  (while (and rest
                              (not done)
                              (nemacs--ptr-write-u8-p (car rest))
                              (eq (nth 1 (car rest)) var)
                              (= (nth 2 (car rest)) idx))
                    (let ((byte (nth 3 (car rest))))
                      (push byte bytes)
                      (setq rest (cdr rest))
                      (setq idx (+ idx 1))
                      (when (= byte 0)
                        (setq done t))))
                  (when done
                    (let ((path (substring (apply #'string (nreverse bytes)) 0 -1)))
                      (when (string-prefix-p "/tmp/" path)
                        (push path paths))))
                  (setq forms rest))
              (scan (car forms))
              (setq forms (cdr forms)))))
         (scan (value)
               (when (consp value)
                 (scan-list value))))
      (scan form))
    (delete-dups paths)))

;; M21 M-x delegation: route M-x to the bridge minibuffer instead of the
;; broken GUI-local minibuffer.  The M-x cond arm currently opens a local
;; minibuffer (st[6]=1) whose RET path writes garbage; replace its body with
;; the canonical fork-to-bridge sequence carrying keyp="M-x" so the bridge
;; opens its own minibuffer (prompt + completion candidates), which the M21
;; render slice already paints.  Subsequent keys reach the bridge through the
;; normal per-key fork arms; the bridge — minibuffer now active — treats them
;; as minibuffer input (narrow / TAB / DEL / RET / C-g all handled bridge-side).
(defvar nemacs-mx-delegate-patched 0)

(defun nemacs--key-fork-forms (key)
  "Fork-to-bridge body that publishes the buffer, writes keyp=KEY (a string),
clears cmdp, forks mx.sh, reads the buffer back, and clears the C-x prefix.
The bridge receives KEY and (its minibuffer now driving) handles the rest."
  (let ((bytes (append key nil)))
    (append
     (cl-loop for b in bytes for i from 0
              collect (list 'ptr-write-u8 'mb i b))
     (list (list 'let* (list (list 'kfd '(syscall-direct 257 -100 keyp 577 438 0 0)))
                 (list 'seq (list 'syscall-direct 1 'kfd 'mb (length bytes) 0 0 0)
                       '(syscall-direct 3 kfd 0 0 0 0 0))))
     (list '(let* ((cfd (syscall-direct 257 -100 cmdp 577 438 0 0)))
              (syscall-direct 3 cfd 0 0 0 0 0))
           '(ptr-write-u64 argvb 0 shp)
           '(ptr-write-u64 argvb 8 mxp)
           '(ptr-write-u64 argvb 16 0)
           '(ptr-write-u64 envb 0 0)
           '(let* ((pid (syscall-direct 57 0 0 0 0 0 0)))
              (if (= pid 0)
                  (seq (syscall-direct 59 shp argvb envb 0 0 0)
                       (syscall-direct 60 1 0 0 0 0 0))
                (syscall-direct 61 pid 0 0 0 0 0)))
           '(let* ((rfd (syscall-direct 257 -100 bufp 0 0 0 0))
                   (rn (if (>= rfd 0) (syscall-direct 0 rfd tb 60000 0 0 0) 0)))
              (ptr-write-u16 st 2 rn))
           '(ptr-write-u8 st 10 0)))))

(defun nemacs--patch-mx-delegate (form)
  "Replace the M-x local-minibuffer arm body with the fork-to-bridge body.
Matches the unique cond arm whose test is the M-x guard and whose body opens
the local minibuffer via (ptr-write-u8 st 6 1)."
  (cond
   ((and (consp form)
         (equal (car form) '(if (= ks 120) (if (= alt 1) 1 0) 0))
         (member '(ptr-write-u8 st 6 1) form))
    (setq nemacs-mx-delegate-patched (+ nemacs-mx-delegate-patched 1))
    (cons (car form) (nemacs--key-fork-forms "M-x")))
   ((consp form)
    (mapcar #'nemacs--patch-mx-delegate form))
   (t form)))

;; M21: delegate the C-x / C-h / M-g minibuffer openers (find-file, switch-buffer,
;; kill-buffer, write-file, goto-line, describe-*, ...) to the bridge, like M-x.
;; Each opener arm body is a (seq (ptr-write-u8 st 6 1) (ptr-write-u8 st INTENT 1)
;; (ptr-write-u8 st OTHER 0) ...) that opens the broken GUI-local minibuffer.
;; Replace that seq with a fork-to-bridge carrying the opener's key sequence;
;; the bridge opens its own prompted minibuffer (e.g. "Find file: ") with
;; completion, and subsequent keys fork through the normal per-key arms.
(defvar nemacs-opener-delegate-patched 0)

(defun nemacs--opener-key-for-seq (seq)
  "Map an opener seq's intent flag (the st[N]=1 it sets) to its bridge key."
  (cond
   ((member '(ptr-write-u8 st 11 1) seq) "C-x C-f")
   ((member '(ptr-write-u8 st 12 1) seq) "C-x C-w")
   ((member '(ptr-write-u8 st 13 1) seq) "C-x C-v")
   ((member '(ptr-write-u8 st 23 1) seq) "C-x C-r")
   ((member '(ptr-write-u8 st 20 1) seq) "C-x b")
   ((member '(ptr-write-u8 st 21 1) seq) "C-x k")
   ((member '(ptr-write-u8 st 22 1) seq) "C-x i")
   ((member '(ptr-write-u8 st 18 1) seq) "C-s")
   ((member '(ptr-write-u8 st 19 1) seq) "C-r")
   ((member '(ptr-write-u8 st 25 1) seq) "M-g g")
   ((member '(ptr-write-u8 st 27 1) seq) "C-h f")
   ((member '(ptr-write-u8 st 28 1) seq) "C-h v")
   ((member '(ptr-write-u8 st 29 1) seq) "C-h k")
   (t nil)))

(defun nemacs--patch-opener-delegate (form)
  (cond
   ((and (consp form)
         (eq (car form) 'seq)
         (member '(ptr-write-u8 st 6 1) form)
         (nemacs--opener-key-for-seq form))
    (setq nemacs-opener-delegate-patched (+ nemacs-opener-delegate-patched 1))
    (cons 'seq (nemacs--key-fork-forms (nemacs--opener-key-for-seq form))))
   ((consp form)
    (mapcar #'nemacs--patch-opener-delegate form))
   (t form)))

;; M21: encode the non-printable special keys (Return / Backspace / Tab) into
;; the shared keyp encoder so they reach the bridge by name.  The legacy
;; per-key cmd-transport writes (e.g. RET -> cmd="newline") are stripped by
;; nemacs--strip-legacy-cmd-channel, but the keyp encoder had no arm for
;; ks 65293/65288/65289, so those keys forked an EMPTY key string and the
;; bridge did nothing — no newline, and minibuffer RET/DEL never executed.
;; Wrap the encoder (the M-x-case `if`, shared across all 62 fork arms) with
;; RET/DEL/TAB cases.  The bridge already maps keys="RET"/"DEL"/"TAB" to the
;; right action in both buffer and minibuffer context (verified).
(defvar nemacs-special-key-encode-patched 0)

(defun nemacs--patch-special-key-encode (form)
  (cond
   ((and (consp form)
         (eq (car form) 'if)
         (equal (cadr form) '(if (= (ptr-read-u8 st 6) 1) (= kn 0) 0)))
    (setq nemacs-special-key-encode-patched
          (+ nemacs-special-key-encode-patched 1))
    `(if (= ks 65293)
         (seq (ptr-write-u8 mb 0 82) (ptr-write-u8 mb 1 69) (ptr-write-u8 mb 2 84) (setq kn 3))
       (if (= ks 65288)
           (seq (ptr-write-u8 mb 0 68) (ptr-write-u8 mb 1 69) (ptr-write-u8 mb 2 76) (setq kn 3))
         (if (= ks 65289)
             (seq (ptr-write-u8 mb 0 84) (ptr-write-u8 mb 1 65) (ptr-write-u8 mb 2 66) (setq kn 3))
           ,(cons (car form) (mapcar #'nemacs--patch-special-key-encode (cdr form)))))))
   ((consp form)
    (mapcar #'nemacs--patch-special-key-encode form))
   (t form)))

(when (boundp 'xfont-sexp)
  (setq nemacs-direct-command-transport-dropped 0)
  (setq nemacs-redisplay-correction-dropped 0)
  (setq nemacs-hscroll-redisplay-patched 0)
  (setq nemacs-window-split-redisplay-patched 0)
  (setq nemacs-tabline-redisplay-patched 0)
  (setq nemacs-transport-paths-rewritten 0)
  (setq nemacs-face-span-redisplay-patched 0)
  (setq nemacs-face-span-color-replaced 0)
  (setq nemacs-font-openfont-patched 0)
  (setq nemacs-font-queryfont-patched 0)
  (setq nemacs-cjk-text-draw-patched 0)
  (setq nemacs-cjk-cursor-scan-patched 0)
  (setq nemacs-cjk-cursor-width-patched 0)
  (setq nemacs-cjk-font-cw-patched 0)
  (setq xfont-sexp (nemacs--strip-legacy-cmd-channel xfont-sexp))
  (setq xfont-sexp (nemacs--patch-face-span-redisplay xfont-sexp))
  (setq xfont-sexp (nemacs--patch-font-openfont xfont-sexp))
  ;; CJK text decode must precede the hscroll rewrite (it anchors on
  ;; the pristine linelen / lead-byte read shapes); the cursor cell
  ;; rewrite must FOLLOW it (it matches both cursor-x shapes).
  (setq xfont-sexp (nemacs--patch-cjk-text-draw xfont-sexp))
  (setq xfont-sexp (nemacs--patch-cjk-font-cw xfont-sexp))
  (setq xfont-sexp (nemacs--patch-hscroll-redisplay xfont-sexp))
  (setq xfont-sexp (nemacs--patch-window-split-redisplay xfont-sexp))
  (setq xfont-sexp (nemacs--patch-tabline-redisplay xfont-sexp))
  (setq xfont-sexp (nemacs--patch-cjk-cursor xfont-sexp))
  ;; P4: bake the X display number (NEMACS_X_DISPLAY_NUM, default 0) so
  ;; the binary can target a nested X (Xephyr :N) for visual tests
  (setq nemacs-x-display-patched 0)
  (setq xfont-sexp (nemacs--patch-x-display xfont-sexp))
  ;; M22: ButtonPress (et=4) tool-bar click -> fork the bridge.  Additive
  ;; on et=4 only; runs before the view read-redirect.
  (setq nemacs-buttonpress-patched 0)
  (setq xfont-sexp (nemacs--patch-buttonpress xfont-sexp))
  ;; M20: redirect the GUI's READ-ONLY opens of buf/point/window-start
  ;; to the bridge's view-slice channel (edit write-backs stay on the
  ;; original paths; the bridge splices them at view-rebase).
  ;; M20 NOTE: the view read-redirect breaks the GUI's key->bridge
  ;; dispatch (redirecting the render reads also disturbs the per-key
  ;; reload path, so no command reaches the bridge).  Disabled until
  ;; the redirect is scoped to render-only; large-file view rendering
  ;; waits on that.  The bridge still publishes the view slice.
  (setq nemacs-view-readopen-patched 0)
  ;; M20 land: the read-redirect is NARROW (only flags=0 RO opens of
  ;; bufp/gotop/wstp move to viewp/vptp/vstp; the pre-fork WRITE opens
  ;; keep flags 577 and stay on the original paths, so the fork/dispatch
  ;; sequence is untouched).  In session mode the bridge ignores those
  ;; write-backs and owns the buffer, publishing a point-tracking slice;
  ;; small buffers publish the whole text on the view channel (rebase 0),
  ;; so this stays transparent below the cap.
  (setq xfont-sexp (nemacs--patch-view-readopen xfont-sexp))
  ;; M21b echo-area separation: shift the mode line up 16px FIRST so it
  ;; only hits the original draws, then the minibuffer/echo lands in
  ;; the freed bottom strip
  (setq nemacs-modeline-shift-patched 0)
  (setq xfont-sexp (nemacs--patch-modeline-shift xfont-sexp))
  ;; M21 minibuffer render (echo area; always clears, content when
  ;; the bridge minibuffer is active)
  (setq nemacs-minibuffer-render-patched 0)
  (setq xfont-sexp (nemacs--apply-mb-echo-area xfont-sexp))
  ;; M22 tool bar render (bridge-defined, GUI-painted; click wiring TBD)
  (setq nemacs-toolbar-render-patched 0)
  (setq xfont-sexp (nemacs--patch-toolbar-render xfont-sexp))
  ;; M21: M-x opens the bridge minibuffer (not the broken local one)
  (setq nemacs-mx-delegate-patched 0)
  (setq xfont-sexp (nemacs--patch-mx-delegate xfont-sexp))
  ;; M21: the C-x / C-h / M-g minibuffer openers delegate to the bridge too
  (setq nemacs-opener-delegate-patched 0)
  (setq xfont-sexp (nemacs--patch-opener-delegate xfont-sexp))
  ;; M21: encode RET/DEL/TAB into the keyp encoder so they reach the bridge
  (setq nemacs-special-key-encode-patched 0)
  (setq xfont-sexp (nemacs--patch-special-key-encode xfont-sexp))
  ;; M21/M22: do not publish the GUI-local tb before bridge forks.  The
  ;; session bridge owns buffer state and the GUI reads the bridge result back.
  (setq nemacs-prefork-buffer-publish-dropped 0)
  (setq xfont-sexp (nemacs--drop-prefork-buffer-publish xfont-sexp))
  (let ((transport-dir (getenv "NEMACS_TRANSPORT_DIR"))
        (config-path (getenv "NEMACS_CONFIG_PATH")))
    (when (or (and transport-dir
                   (not (string= transport-dir ""))
                   (not (string= (directory-file-name transport-dir) "/tmp")))
              (and config-path
                   (not (string= config-path ""))
                   (not (string= (expand-file-name config-path)
                                 (concat "/tmp" "/nemacs.cfg")))))
      (setq xfont-sexp
            (nemacs--rewrite-transport-paths
             xfont-sexp
             (or transport-dir "/tmp"))))))

;;; nemacs-editor-transport.el ends here
