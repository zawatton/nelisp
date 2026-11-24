;;; emacs-char-table.el --- char-table substrate for standalone NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 08 §2 (2026-06-05) — Layer 2 (nemacs substrate).
;;
;; The standalone NeLisp bootstrap only had `nil'-stubs for the
;; char-table API (in `emacs-stub-bulk.el'), and the lightweight
;; case-table substrate (`case-table.el', 259-element fixed vectors)
;; is not wired into the cold-boot bundle.  That left vendor code such
;; as isearch.el's `isearch-mode-map' defvar unable to evaluate:
;;
;;     (let ((map (make-keymap)))
;;       (or (char-table-p (nth 1 map))
;;           (error "..."))                       ; <- assertion failed
;;       (set-char-table-range (nth 1 map) (cons #x100 (max-char)) ...))
;;
;; This module provides a real, sparse char-table substrate that:
;;
;;   - satisfies `char-table-p' (so the assertion passes once
;;     `make-keymap' embeds one — see `emacs-keymap.el'),
;;   - stores huge ranges such as `(#x100 . #x3FFFFF)' sparsely instead
;;     of materialising ~4M slots (which would crash or OOM), and
;;   - supplies the missing `max-char' / `char-table-subtype' /
;;     `char-table-parent' primitives.
;;
;; Representation (tagged vector — `vectorp' ops are always available,
;; unlike a hypothetical reader-level char-table type):
;;
;;   [--nemacs-char-table SUBTYPE DEFAULT PARENT ASCII-VEC RANGES EXTRA]
;;     0 TAG       = `--nemacs-char-table' (the `char-table-p' key)
;;     1 SUBTYPE   = the `make-char-table' subtype argument
;;     2 DEFAULT   = value for characters with no explicit entry
;;     3 PARENT    = parent char-table, or nil
;;     4 ASCII-VEC = 256 nil-sentinel slots, fast path for char < 256
;;     5 RANGES    = list of ((FROM . TO) . VAL), newest first, char >= 256
;;     6 EXTRA     = (make-vector N nil), with N fixed at construction
;;
;; Length is 7 so `(> (length x) 6)' holds, distinguishing a char-table
;; from incidental short vectors.
;;
;; Two-mode design (mirrors `case-table.el' / `emacs-keymap-builtins.el'):
;; under host Emacs the real C primitives win (the unprefixed names are
;; only installed when not already `fboundp'); under standalone NeLisp we
;; install our implementations unconditionally so they override the
;; earlier `emacs-stub-bulk.el' nil-stubs.

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- char-tables use the library char-table representation.
;;; Code:

(defconst emacs-char-table--tag '--nemacs-char-table
  "Symbol stored in slot 0 that identifies a NeLisp char-table.")

(defconst emacs-char-table--ascii-size 256
  "Number of fast direct-indexed slots (characters 0..255).")

(defconst emacs-char-table--extra-slots 10
  "Legacy extra-slot capacity.
New tables allocate the count specified by their subtype property.")

(defconst emacs-char-table--category-set-size 128
  "Number of category slots in a character's category set.")

(defvar emacs-char-table--standard-category-table nil
  "Standalone standard category table.")

(defvar emacs-char-table--current-category-table nil
  "Standalone current-buffer category table.
The lightweight runtime has no native per-buffer category-table slot yet;
the shared table still gives vendor libraries real mutation semantics.")

;; Count of Lisp-path char-table writes; owned by the prelude.  Every writer below
;; bumps it so a cache keyed on it sees a raw write.  Declaration plus a
;; host-load fallback only, so the name has a single definition site.
(defvar nelisp--chartab-lisp-epoch)
(unless (boundp 'nelisp--chartab-lisp-epoch) (set 'nelisp--chartab-lisp-epoch 0))

(defconst emacs-char-table--max-char #x3FFFFF
  "Largest character code (Doc 05: UTF-8 / Unicode range).")

;; Slot indices.
(defconst emacs-char-table--i-subtype 1)
(defconst emacs-char-table--i-default 2)
(defconst emacs-char-table--i-parent 3)
(defconst emacs-char-table--i-ascii 4)
(defconst emacs-char-table--i-ranges 5)
(defconst emacs-char-table--i-extra 6)
(defconst emacs-char-table--i-owner-state 7)

;; GNU's raw character-table tree uses 6 top-level bits followed by
;; subtables with 4, 5, and 7 index bits.  Keep the geometry in bits so the
;; corresponding vector sizes remain derived from the source topology.
(defconst emacs-char-table--topology-root-bits 6)
(defconst emacs-char-table--topology-level-1-bits 4)
(defconst emacs-char-table--topology-level-2-bits 5)
(defconst emacs-char-table--topology-level-3-bits 7)
(defconst emacs-char-table--topology-root-size
  (ash 1 emacs-char-table--topology-root-bits))
(defconst emacs-char-table--topology-level-1-size
  (ash 1 emacs-char-table--topology-level-1-bits))
(defconst emacs-char-table--topology-level-2-size
  (ash 1 emacs-char-table--topology-level-2-bits))
(defconst emacs-char-table--topology-level-3-size
  (ash 1 emacs-char-table--topology-level-3-bits))

;; The tag is uninterned and stable across provider reloads.  A same-named
;; interned user record therefore remains an ordinary scalar leaf.
(defvar emacs-char-table--topology-node-brand
  (make-symbol "emacs-char-table--topology-node")
  "Private record tag for allocated topology nodes.")

(defun emacs-char-table--topology-node-create (contents)
  "Return a typed topology node containing the vector CONTENTS."
  (unless (vectorp contents)
    (signal 'wrong-type-argument (list 'vectorp contents)))
  (record emacs-char-table--topology-node-brand contents))

(defun emacs-char-table--topology-node-p (object)
  "Return non-nil when OBJECT is a private topology node."
  (and (recordp object)
       (eq (type-of object) emacs-char-table--topology-node-brand)
       (= (length object) 2)))

(defun emacs-char-table--topology-node-contents (object)
  "Return the contents vector of topology node OBJECT."
  (unless (emacs-char-table--topology-node-p object)
    (signal 'wrong-type-argument
            (list 'emacs-char-table--topology-node-p object)))
  (aref object 1))

(defun emacs-char-table--topology-create (init)
  "Return the raw topology root with every top-level entry initialized to INIT."
  (make-vector emacs-char-table--topology-root-size init))

(defun emacs-char-table--topology-set-char (root char value)
  "Set VALUE at CHAR in raw topology ROOT, expanding its missing path.
This updates only ROOT and does not publish any table cache or mirror."
  (let* ((root-index
          (ash char (- (+ emacs-char-table--topology-level-1-bits
                          emacs-char-table--topology-level-2-bits
                          emacs-char-table--topology-level-3-bits))))
         (level-1-index
          (logand (ash char (- (+ emacs-char-table--topology-level-2-bits
                                 emacs-char-table--topology-level-3-bits)))
                  (1- emacs-char-table--topology-level-1-size)))
         (level-2-index
          (logand (ash char (- emacs-char-table--topology-level-3-bits))
                  (1- emacs-char-table--topology-level-2-size)))
         (leaf-index
          (logand char (1- emacs-char-table--topology-level-3-size)))
         (root-link (aref root root-index))
         (level-1-node
          (if (emacs-char-table--topology-node-p root-link)
              root-link
            (let ((node
                   (emacs-char-table--topology-node-create
                    (make-vector emacs-char-table--topology-level-1-size
                                 root-link))))
              (aset root root-index node)
              node)))
         (level-1-contents
          (emacs-char-table--topology-node-contents level-1-node))
         (level-1-link (aref level-1-contents level-1-index))
         (level-2-node
          (if (emacs-char-table--topology-node-p level-1-link)
              level-1-link
            (let ((node
                   (emacs-char-table--topology-node-create
                    (make-vector emacs-char-table--topology-level-2-size
                                 level-1-link))))
              (aset level-1-contents level-1-index node)
              node)))
         (level-2-contents
          (emacs-char-table--topology-node-contents level-2-node))
         (level-2-link (aref level-2-contents level-2-index))
         (level-3-node
          (if (emacs-char-table--topology-node-p level-2-link)
              level-2-link
            (let ((node
                   (emacs-char-table--topology-node-create
                    (make-vector emacs-char-table--topology-level-3-size
                                 level-2-link))))
              (aset level-2-contents level-2-index node)
              node)))
         (level-3-contents
          (emacs-char-table--topology-node-contents level-3-node)))
    (aset level-3-contents leaf-index value)
    value))

(defun emacs-char-table--topology-ref (root char)
  "Return raw topology ROOT's value at CHAR, without default or parent lookup."
  (let* ((root-index
          (ash char (- (+ emacs-char-table--topology-level-1-bits
                          emacs-char-table--topology-level-2-bits
                          emacs-char-table--topology-level-3-bits))))
         (level-1-index
          (logand (ash char (- (+ emacs-char-table--topology-level-2-bits
                                 emacs-char-table--topology-level-3-bits)))
                  (1- emacs-char-table--topology-level-1-size)))
         (level-2-index
          (logand (ash char (- emacs-char-table--topology-level-3-bits))
                  (1- emacs-char-table--topology-level-2-size)))
         (leaf-index
          (logand char (1- emacs-char-table--topology-level-3-size)))
         (root-link (aref root root-index)))
    (if (not (emacs-char-table--topology-node-p root-link))
        root-link
      (let* ((level-1-contents
              (aref root-link 1))
             (level-1-link (aref level-1-contents level-1-index)))
        (if (not (emacs-char-table--topology-node-p level-1-link))
            level-1-link
          (let* ((level-2-contents
                  (aref level-1-link 1))
                 (level-2-link (aref level-2-contents level-2-index)))
            (if (not (emacs-char-table--topology-node-p level-2-link))
                level-2-link
              (aref (aref level-2-link 1) leaf-index))))))))

(defun emacs-char-table--topology-ascii-cache (root)
  "Return ROOT's raw value at zero, or its typed depth-three node alias."
  (let ((entry (aref root 0))
        (depth 0))
    (while (and (< depth 3)
                (emacs-char-table--topology-node-p entry))
      (if (= depth 2)
          (setq depth 3)
        (setq entry
              (aref (emacs-char-table--topology-node-contents entry) 0)
              depth (1+ depth))))
    entry))

(defun emacs-char-table--topology-leaf-span (root char)
  "Return ROOT's scalar or leaf node for its aligned span at CHAR."
  (let* ((root-index
          (ash char (- (+ emacs-char-table--topology-level-1-bits
                          emacs-char-table--topology-level-2-bits
                          emacs-char-table--topology-level-3-bits))))
         (level-1-index
          (logand (ash char (- (+ emacs-char-table--topology-level-2-bits
                                 emacs-char-table--topology-level-3-bits)))
                  (1- emacs-char-table--topology-level-1-size)))
         (level-2-index
          (logand (ash char (- emacs-char-table--topology-level-3-bits))
                  (1- emacs-char-table--topology-level-2-size)))
         (entry (aref root root-index)))
    (when (emacs-char-table--topology-node-p entry)
      (setq entry
            (aref (emacs-char-table--topology-node-contents entry)
                  level-1-index))
      (when (emacs-char-table--topology-node-p entry)
        (setq entry
              (aref (emacs-char-table--topology-node-contents entry)
                    level-2-index))))
    entry))

(defun emacs-char-table--topology-copy-leaf-span (root char)
  "Return a fresh 128-slot vector for ROOT's aligned span at CHAR."
  (let ((entry (emacs-char-table--topology-leaf-span root char)))
    (if (emacs-char-table--topology-node-p entry)
        (copy-sequence (emacs-char-table--topology-node-contents entry))
      (make-vector emacs-char-table--topology-level-3-size entry))))

(defun emacs-char-table--topology--copy-node (node depth)
  "Return a deep topology-node copy of NODE, sharing all scalar leaves."
  (let* ((contents (emacs-char-table--topology-node-contents node))
         (copied (copy-sequence contents))
         (index 0))
    ;; Depth-three contents are character values.  They can be records that
    ;; resemble topology nodes, or native sub-char-table values; copy the
    ;; vector but preserve those payloads by identity.
    (when (< depth 3)
      (while (< index (length copied))
        (let ((entry (aref copied index)))
          (when (emacs-char-table--topology-node-p entry)
            (aset copied index
                  (emacs-char-table--topology--copy-node entry (1+ depth))))
          (setq index (1+ index)))))
    (emacs-char-table--topology-node-create copied)))

(defun emacs-char-table--topology-copy (root)
  "Return a deep topology copy of ROOT, preserving scalar value identities."
  (let ((copied (copy-sequence root))
        (index 0))
    (while (< index (length copied))
      (let ((entry (aref copied index)))
        (when (emacs-char-table--topology-node-p entry)
          (aset copied index
                  (emacs-char-table--topology--copy-node entry 1))))
      (setq index (1+ index)))
    copied))

(defun emacs-char-table--topology--set-range-contents
    (contents base depth from to value)
  "Update intersecting CONTENTS blocks in raw inclusive range FROM..TO."
  (let* ((block-size (cond ((= depth 0)
                            (ash 1 (+ emacs-char-table--topology-level-1-bits
                                      emacs-char-table--topology-level-2-bits
                                      emacs-char-table--topology-level-3-bits)))
                           ((= depth 1)
                            (ash 1 (+ emacs-char-table--topology-level-2-bits
                                      emacs-char-table--topology-level-3-bits)))
                           ((= depth 2)
                            (ash 1 emacs-char-table--topology-level-3-bits))
                           (t 1)))
         (index (max 0 (/ (max 0 (- from base)) block-size)))
         (length (length contents)))
    (while (and (< index length)
                (<= (+ base (* index block-size)) to))
      (let* ((block-start (+ base (* index block-size)))
             (block-end (+ block-start block-size -1)))
        (if (and (<= from block-start) (<= block-end to))
            (aset contents index value)
          (let* ((old-entry (aref contents index))
                 (node
                  (if (emacs-char-table--topology-node-p old-entry)
                      old-entry
                    (let* ((child-size
                            (cond ((= depth 0)
                                   emacs-char-table--topology-level-1-size)
                                  ((= depth 1)
                                   emacs-char-table--topology-level-2-size)
                                  (t emacs-char-table--topology-level-3-size)))
                           (child
                            (emacs-char-table--topology-node-create
                             (make-vector child-size old-entry))))
                      (aset contents index child)
                      child)))
                 (child-contents
                  (emacs-char-table--topology-node-contents node)))
            (emacs-char-table--topology--set-range-contents
             child-contents block-start (1+ depth) from to value))))
      (setq index (1+ index)))))

(defun emacs-char-table--topology-set-range (root from to value)
  "Set raw ROOT's inclusive character range FROM..TO to VALUE.
The private caller supplies valid ordered character bounds."
  (if (= from to)
      (emacs-char-table--topology-set-char root from value)
    (emacs-char-table--topology--set-range-contents
     root 0 0 from to value)
    value))

;; The owner's cache can intentionally retain a detached GNU ASCII subtree
;; after an interrupted optimizer traversal.  Keep it beside the raw root.
(defconst emacs-char-table--owner-state-version 2)

(defun emacs-char-table--owner-state-create (root cache)
  (record 'emacs-char-table--owner-state root cache
          emacs-char-table--owner-state-version))

(defun emacs-char-table--owner-state-p (object)
  (and (recordp object)
       (eq (type-of object) 'emacs-char-table--owner-state)
       (or (= (length object) 3)
           (= (length object) 4))))

(defun emacs-char-table--topology-legacy-node-p (object depth)
  "Return non-nil when OBJECT has the old node shape for DEPTH."
  (and (recordp object)
       (eq (type-of object) 'emacs-char-table--topology-node)
       (= (length object) 2)
       (vectorp (aref object 1))
       (= (length (aref object 1))
          (cond ((= depth 1) emacs-char-table--topology-level-1-size)
                ((= depth 2) emacs-char-table--topology-level-2-size)
                ((= depth 3) emacs-char-table--topology-level-3-size)
                (t -1)))))

(defun emacs-char-table--topology-upgrade-legacy-node (object depth node-map)
  "Copy one old topology node at DEPTH into the private-tagged format."
  (if (not (emacs-char-table--topology-legacy-node-p object depth))
      object
    (let ((contents (copy-sequence (aref object 1))))
      (when (< depth 3)
        (let ((index 0))
          (while (< index (length contents))
            (let ((child (aref contents index)))
              (when (emacs-char-table--topology-legacy-node-p child
                                                              (1+ depth))
                (aset contents index
                      (emacs-char-table--topology-upgrade-legacy-node
                       child (1+ depth) node-map))))
            (setq index (1+ index)))))
      (let ((node (emacs-char-table--topology-node-create contents)))
        (aset node-map 0 (cons (cons object node) (aref node-map 0)))
        node))))

(defun emacs-char-table--topology-upgrade-legacy-root (root node-map)
  "Copy ROOT's structurally valid old nodes into the private-tagged format."
  (let ((upgraded (copy-sequence root))
        (index 0))
    (while (< index (length upgraded))
      (let ((child (aref upgraded index)))
        (when (emacs-char-table--topology-legacy-node-p child 1)
          (aset upgraded index
                (emacs-char-table--topology-upgrade-legacy-node
                 child 1 node-map))))
      (setq index (1+ index)))
    upgraded))

(defun emacs-char-table--owned-p (table)
  (and (vectorp table)
       (> (length table) emacs-char-table--i-owner-state)
       (eq (emacs-char-table--raw-ref table 0) emacs-char-table--tag)
       (emacs-char-table--owner-state-p
        (emacs-char-table--raw-ref table emacs-char-table--i-owner-state))))

(defun emacs-char-table--owner-current-state (table)
  "Return TABLE's current owner state, upgrading a legacy tree once."
  (let ((state (emacs-char-table--raw-ref
                table emacs-char-table--i-owner-state)))
    (when (or (= (length state) 3)
              (< (aref state 3) emacs-char-table--owner-state-version))
      (let* ((node-map (vector nil))
             (root (emacs-char-table--topology-upgrade-legacy-root
                    (aref state 1) node-map))
             (old-cache (aref state 2))
             (mapped-cache (cdr (assq old-cache (aref node-map 0))))
             (cache (or mapped-cache
                        (if (emacs-char-table--topology-legacy-node-p
                             old-cache 3)
                            (emacs-char-table--topology-upgrade-legacy-node
                             old-cache 3 node-map)
                          old-cache)))
             (new-state (emacs-char-table--owner-state-create root cache)))
        (emacs-char-table--raw-set table emacs-char-table--i-owner-state
                                   new-state)
        (setq state new-state)))
    state))

(defun emacs-char-table--owner-root (table)
  (aref (emacs-char-table--owner-current-state table) 1))

(defun emacs-char-table--owner-cache (table)
  (aref (emacs-char-table--owner-current-state table) 2))

(defun emacs-char-table--owner-set-cache (table cache)
  (aset (emacs-char-table--owner-current-state table) 2 cache))

(defun emacs-char-table--owner-cache-ref (table char)
  (let ((cache (emacs-char-table--owner-cache table)))
    (if (emacs-char-table--topology-node-p cache)
        (aref (aref cache 1) char)
      cache)))

;; An owned table's topology root is its only storage above the ASCII
;; slots.  Its `emacs-char-table--i-ranges' slot is a derived view, built on
;; demand by `emacs-char-table--ranges'; a write only marks it stale.  Keeping
;; the view current on every write rescanned the whole list and made
;; scattered writes quadratic.  The marker is an interned private symbol (it
;; survives a dump); a reader that bypasses the accessor fails on it loudly.
(defconst emacs-char-table--stale-ranges 'emacs-char-table--stale-ranges
  "Marker in an owned table's ranges slot: rebuild it from the topology.")

(defun emacs-char-table--topology-runs (root)
  "Return ROOT's non-nil runs above the ASCII slots as a ranges view.
The list holds ((FROM . TO) . VALUE) entries, descending and
nonoverlapping, adjacent `eq' values merged; one walk over ROOT's nodes."
  (let* ((lo emacs-char-table--ascii-size)
         (span-3 emacs-char-table--topology-level-3-size)
         (span-2 (* emacs-char-table--topology-level-2-size span-3))
         (span-1 (* emacs-char-table--topology-level-1-size span-2))
         ;; Pending run: (FROM . VALUE); RESULT collects closed runs.
         (state (vector nil nil nil))
         (emit (lambda (from value)
                 (unless (and (aref state 0) (eq value (aref state 1)))
                   (when (and (aref state 0) (aref state 1))
                     (aset state 2 (cons (cons (cons (aref state 0) (1- from))
                                               (aref state 1))
                                         (aref state 2))))
                   (aset state 0 from)
                   (aset state 1 value))))
         (i1 0))
    (while (< i1 (length root))
      (let ((l1 (aref root i1)) (s1 (* i1 span-1)))
        (if (not (emacs-char-table--topology-node-p l1))
            (funcall emit (max s1 lo) l1)
          (let ((c1 (aref l1 1)) (i2 0))
            (while (< i2 (length c1))
              (let ((l2 (aref c1 i2)) (s2 (+ s1 (* i2 span-2))))
                (if (not (emacs-char-table--topology-node-p l2))
                    (funcall emit (max s2 lo) l2)
                  (let ((c2 (aref l2 1)) (i3 0))
                    (while (< i3 (length c2))
                      (let ((l3 (aref c2 i3)) (s3 (+ s2 (* i3 span-3))))
                        (cond
                         ((not (emacs-char-table--topology-node-p l3))
                          (when (> (+ s3 span-3) lo)
                            (funcall emit (max s3 lo) l3)))
                         ((> (+ s3 span-3) lo)
                          (let ((c3 (aref l3 1))
                                (i4 (max 0 (- lo s3))))
                            (while (< i4 (length c3))
                              (funcall emit (+ s3 i4) (aref c3 i4))
                              (setq i4 (1+ i4)))))))
                      (setq i3 (1+ i3))))))
              (setq i2 (1+ i2))))))
      (setq i1 (1+ i1)))
    (when (and (aref state 0) (aref state 1))
      (aset state 2 (cons (cons (cons (aref state 0) emacs-char-table--max-char)
                                (aref state 1))
                          (aref state 2))))
    (aref state 2)))

(defun emacs-char-table--ranges (table)
  "Return TABLE's ranges view, derived from an owned table's topology.
The view is not cached in the table: the native `equal' compares a
tagged table slot by slot, so a cache filled on one of two equal tables
would make them unequal."
  (let ((ranges (emacs-char-table--raw-ref table emacs-char-table--i-ranges)))
    (if (eq ranges emacs-char-table--stale-ranges)
        (emacs-char-table--topology-runs (emacs-char-table--owner-root table))
      ranges)))

(defun emacs-char-table--owner-publish-span (table from to)
  "Publish the already-written raw span FROM..TO into compatibility views."
  (let ((ascii (emacs-char-table--raw-ref table emacs-char-table--i-ascii))
        (lo (max 0 from)) (hi (min 255 to)))
    (while (<= lo hi)
      (emacs-char-table--raw-set
       ascii lo
       (if (< lo 128)
           (emacs-char-table--owner-cache-ref table lo)
         (emacs-char-table--topology-ref (emacs-char-table--owner-root table)
                                         lo)))
      (setq lo (1+ lo)))
    (when (> to 255)
      (emacs-char-table--raw-set table emacs-char-table--i-ranges
                                 emacs-char-table--stale-ranges))))

(defun emacs-char-table--owner-refresh-ascii-cache (table)
  "Refresh TABLE's raw ASCII cache alias and publish its complete view."
  (emacs-char-table--owner-set-cache
   table (emacs-char-table--topology-ascii-cache
          (emacs-char-table--owner-root table)))
  (let* ((ascii (emacs-char-table--raw-ref table emacs-char-table--i-ascii))
         (span (emacs-char-table--topology-leaf-span
                (emacs-char-table--owner-root table) 0))
         (node-p (emacs-char-table--topology-node-p span))
         (contents (and node-p
                        (emacs-char-table--topology-node-contents span)))
         (native-bridge (emacs-char-table--native-vector-bridge-p))
         (char 0))
    (if native-bridge
        (while (< char 128)
          (nelisp--raw-aset ascii char (if node-p (aref contents char) span))
          (setq char (1+ char)))
      (while (< char 128)
        (funcall emacs-char-table--raw-aset
                 ascii char (if node-p (aref contents char) span))
        (setq char (1+ char))))))

(defun emacs-char-table--owner-optimize-node
    (table node base span depth test)
  "Optimize NODE in TABLE over BASE..BASE+SPAN-1 using TEST.
Return the collapsed scalar when every slot compares equal, otherwise NODE.
Publish each collapsed child only after its parent slot has been updated."
  (let* ((contents (emacs-char-table--topology-node-contents node))
         (slot-span (/ span (length contents)))
         (first (aref contents 0))
         (uniform t)
         (index 0))
    (when (and (< depth 3)
               (emacs-char-table--topology-node-p first))
      (setq first
            (emacs-char-table--owner-optimize-node
             table first base slot-span (1+ depth) test))
      (aset contents 0 first)
      (unless (emacs-char-table--topology-node-p first)
        (emacs-char-table--owner-publish-span
         table base (+ base slot-span -1))))
    (setq uniform (not (emacs-char-table--topology-node-p first))
          index 1)
    (while (< index (length contents))
      (let* ((slot-base (+ base (* index slot-span)))
             (current (aref contents index)))
        (when (and (< depth 3)
                   (emacs-char-table--topology-node-p current))
          (setq current
                (emacs-char-table--owner-optimize-node
                 table current slot-base slot-span (1+ depth) test))
          (aset contents index current)
          (unless (emacs-char-table--topology-node-p current)
            (emacs-char-table--owner-publish-span
             table slot-base (+ slot-base slot-span -1))))
        (when (and uniform (not (funcall test current first)))
          (setq uniform nil)))
      (setq index (1+ index)))
    (if uniform first node)))

(defun emacs-char-table--owner-optimize (table test)
  "Optimize owned TABLE with the explicit comparison function TEST.
Mutations and compatibility views follow GNU's recursive slot order."
  (let* ((root (emacs-char-table--owner-root table))
         (slot-span (/ (1+ emacs-char-table--max-char) (length root)))
         (index 0))
    (while (< index (length root))
      (let* ((base (* index slot-span))
             (entry (aref root index)))
        (when (emacs-char-table--topology-node-p entry)
          (setq entry
                (emacs-char-table--owner-optimize-node
                 table entry base slot-span 1 test))
          (aset root index entry)
          (unless (emacs-char-table--topology-node-p entry)
            (emacs-char-table--owner-publish-span
             table base (+ base slot-span -1))))
        (setq index (1+ index))))
    (emacs-char-table--owner-refresh-ascii-cache table)
    nil))

(defun emacs-char-table--unicode-property-p (table)
  "Return non-nil for an owned table with the GNU Unicode layout."
  (and (vectorp table)
       (eq (emacs-char-table--raw-ref table emacs-char-table--i-subtype)
           'char-code-property-table)
       (emacs-char-table--owned-p table)
       (eq (emacs-char-table-extra-slot-count table) 5)
       (fboundp 'emacs-cc-chartab-1--uncompress)))

(defun emacs-char-table--owner-set-char (table char value)
  "Apply GNU's single-character write path to owned TABLE."
  (setq nelisp--chartab-lisp-epoch (1+ nelisp--chartab-lisp-epoch))
  (when (and (>= char 128) (emacs-char-table--unicode-property-p table))
    (emacs-cc-chartab-1--uncompress-at table char))
  (let ((cache (emacs-char-table--owner-cache table)))
    (if (and (< char 128) (emacs-char-table--topology-node-p cache))
        (aset (emacs-char-table--topology-node-contents cache) char value)
      (emacs-char-table--topology-set-char
       (emacs-char-table--owner-root table) char value)
      (when (< char 128)
        (emacs-char-table--owner-refresh-ascii-cache table)))
    (emacs-char-table--owner-publish-span table char char))
  value)

(defun emacs-char-table--owner-set-range (table from to value)
  "Apply a character range write to owned TABLE and return VALUE."
  (setq nelisp--chartab-lisp-epoch (1+ nelisp--chartab-lisp-epoch))
  (cond
   ((= from to)
    (emacs-char-table--owner-set-char table from value))
   ((> from to)
    (when (< from 128)
      (emacs-char-table--owner-refresh-ascii-cache table))
    value)
   (t
    (when (emacs-char-table--unicode-property-p table)
      (unless (= (logand from 127) 0)
        (emacs-cc-chartab-1--uncompress-at table from))
      (unless (= (logand to 127) 127)
        (emacs-cc-chartab-1--uncompress-at table to)))
    (emacs-char-table--topology-set-range
     (emacs-char-table--owner-root table) from to value)
    (when (< from 128)
      (emacs-char-table--owner-refresh-ascii-cache table))
    (when (>= to 128)
      (emacs-char-table--owner-publish-span table (max 128 from) to))
    value)))

(defun emacs-char-table--copy-storage (table)
  "Copy owned TABLE's raw storage without copying detached cache history."
  (let* ((root (emacs-char-table--topology-copy
                (emacs-char-table--owner-root table)))
         (copy (make-vector (length table) nil))
         (i 0))
    (while (< i (length table))
      (emacs-char-table--raw-set copy i (emacs-char-table--raw-ref table i))
      (setq i (1+ i)))
    (emacs-char-table--raw-set
     copy emacs-char-table--i-extra
     (copy-sequence (emacs-char-table--raw-ref table emacs-char-table--i-extra)))
    (emacs-char-table--raw-set
     copy emacs-char-table--i-ascii
     (vconcat (emacs-char-table--topology-copy-leaf-span root 0)
              (emacs-char-table--topology-copy-leaf-span root 128)))
    ;; The copied topology is the storage; derive the view from it on demand.
    (emacs-char-table--raw-set
     copy emacs-char-table--i-ranges emacs-char-table--stale-ranges)
    (emacs-char-table--raw-set
     copy emacs-char-table--i-owner-state
     (emacs-char-table--owner-state-create
      root (emacs-char-table--topology-ascii-cache root)))
    copy))

;; Capture the array primitives before the standalone compatibility bridge is
;; installed.  `defvar' deliberately preserves the original functions when
;; this file is reloaded after `aref' and `aset' have been wrapped.
(defvar emacs-char-table--raw-aref (symbol-function 'aref))
(defvar emacs-char-table--raw-aset (symbol-function 'aset))

;; Native char-table accessors, captured before this file installs its own
;; definitions over the same names (nil when the runtime has none).
(defvar emacs-char-table--native-subtype-fn
  (and (fboundp 'char-table-subtype) (symbol-function 'char-table-subtype)))
(defvar emacs-char-table--native-parent-fn
  (and (fboundp 'char-table-parent) (symbol-function 'char-table-parent)))
(defvar emacs-char-table--native-extra-slot-fn
  (and (fboundp 'char-table-extra-slot) (symbol-function 'char-table-extra-slot)))
(defvar emacs-char-table--native-set-extra-slot-fn
  (and (fboundp 'set-char-table-extra-slot)
       (symbol-function 'set-char-table-extra-slot)))
(defvar emacs-char-table--native-set-parent-fn
  (and (fboundp 'set-char-table-parent)
       (symbol-function 'set-char-table-parent)))

(defun emacs-char-table--native-vector-bridge-p ()
  "Return non-nil when NeLisp dispatches tagged vectors natively."
  (and (fboundp 'nelisp--char-table-vector-bridge-p)
       (nelisp--char-table-vector-bridge-p)))

(defun emacs-char-table--primitive-ref (array index)
  "Read ARRAY's physical slot INDEX, without char-table dispatch."
  (if (emacs-char-table--native-vector-bridge-p)
      (nelisp--raw-aref array index)
    (funcall emacs-char-table--raw-aref array index)))

(defun emacs-char-table--native-syntax-p (object)
  "Recognize the runtime's bootstrap syntax-table record."
  (and (recordp object)
       (= (length object) 3)
       (eq (emacs-char-table--primitive-ref object 0) 'nelisp--syntax-table)
       (hash-table-p (emacs-char-table--primitive-ref object 1))))

(defvar emacs-char-table--native-syntax-storage
  (make-hash-table :test 'eq :weakness 'key)
  "Canonical sparse storage for retained bootstrap syntax-table records.")

(defun emacs-char-table--syntax-entry (class char)
  "Translate the bootstrap CLASS character for CHAR into a syntax entry."
  (if (consp class) class
    (let ((code 0) (spec " .w_()'\"$\\/<>@!|"))
      (while (and (< code (length spec)) (/= class (aref spec code)))
        (setq code (1+ code)))
      (cons (if (< code (length spec)) code 2)
            (cdr (assq char '((40 . 41) (41 . 40) (91 . 93) (93 . 91)
                             (123 . 125) (125 . 123))))))))

;; A standalone binary can retain real native char-tables (for example the
;; syntax tables of modes loaded before this library).  They are neither
;; tagged vectors nor bootstrap records; the only portable signature is
;; their length.  Host Emacs never takes these paths: its char-tables are
;; accepted by the host's own primitives, which this layer does not replace.
(defun emacs-char-table--native-p (object)
  "Return non-nil when OBJECT is a retained native char-table (standalone)."
  (and (not (vectorp object))
       (not (recordp object))
       (not (consp object))
       (not (stringp object))
       (not (null object))
       (emacs-char-table--standalone-p)
       (condition-case nil
           (>= (length object) emacs-char-table--max-char)
         (error nil))))

(defun emacs-char-table--native-ref (ct char)
  "Read CHAR from the native char-table CT using the original `aref'."
  (and (integerp char) (<= 0 char emacs-char-table--max-char)
       (funcall emacs-char-table--raw-aref ct char)))

(defun emacs-char-table--native-set (ct char value)
  "Store VALUE for CHAR in the native char-table CT using the original `aset'."
  (funcall emacs-char-table--raw-aset ct char value))

(defun emacs-char-table--storage (table)
  "Return TABLE's sparse storage, retaining bootstrap record identity.
Bootstrap entries are class characters; library entries include matching
characters and flags.  A retained record gets one shared sparse view rather
than being discarded in favor of the standard table."
  (if (not (emacs-char-table--native-syntax-p table)) table
    (or (gethash table emacs-char-table--native-syntax-storage)
        (let* ((parent (emacs-char-table--primitive-ref table 2))
               (view (emacs-char-table-make 'syntax-table
                                            (unless parent (cons 2 nil)))))
          (puthash table view emacs-char-table--native-syntax-storage)
          (emacs-char-table--raw-set view emacs-char-table--i-parent parent)
          ;; Preserve the bootstrap representation's implicit classes.  Its
          ;; default is word outside ASCII, word for ASCII letters/digits,
          ;; whitespace for these five characters, and symbol otherwise.
          (unless parent
            (let ((char 0))
              (while (< char 128)
                (emacs-char-table-set
                 view char
                 (emacs-char-table--syntax-entry
                  (cond ((memq char '(9 10 12 13 32)) 32)
                        ((or (<= 48 char 57) (<= 65 char 90)
                             (<= 97 char 122)) 119)
                        (t 95)) char))
                (setq char (1+ char)))))
          (maphash (lambda (char class)
                     (emacs-char-table-set
                      view char (emacs-char-table--syntax-entry class char)))
                   (emacs-char-table--primitive-ref table 1))
          view))))

(defun emacs-char-table--raw-ref (array index)
  "Read storage slot INDEX of ARRAY, including bootstrap syntax tables."
  (when (recordp array) (setq array (emacs-char-table--storage array)))
  (if (emacs-char-table--native-vector-bridge-p)
      (nelisp--raw-aref array index)
    (funcall emacs-char-table--raw-aref array index)))

(defun emacs-char-table--raw-set (array index value)
  "Write storage slot INDEX of ARRAY, including bootstrap syntax tables."
  (setq nelisp--chartab-lisp-epoch (1+ nelisp--chartab-lisp-epoch))
  (when (recordp array) (setq array (emacs-char-table--storage array)))
  (if (emacs-char-table--native-vector-bridge-p)
      (nelisp--raw-aset array index value)
    (funcall emacs-char-table--raw-aset array index value)))

(defun emacs-char-table--standalone-p ()
  "Return non-nil under standalone NeLisp.
The NeLisp reader binds `emacs-version' just like host Emacs, so a bare
`(not (boundp 'emacs-version))' test misfires there.  Detect the
standalone path by a NeLisp-only primitive (`nl-write-file'), matching
the marker used in `emacs-keymap.el' / `emacs-fileio-builtins.el'."
  (or (fboundp 'nl-write-file)
      (not (boundp 'emacs-version))))

(defun emacs-char-table--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this substrate."
  (if (emacs-char-table--standalone-p)
      t
    (not (fboundp symbol))))

;;;; --- constructor / predicate ----------------------------------------

(defun emacs-char-table--register-display-table-extra-slots ()
  "Register GNU's six display-table slots when no declaration exists."
  (when (not (plist-member (symbol-plist 'display-table)
                           'char-table-extra-slots))
    (put 'display-table 'char-table-extra-slots 6)))

(emacs-char-table--register-display-table-extra-slots)

(defun emacs-char-table-make (subtype &optional init)
  "Return a fresh char-table with symbol SUBTYPE and initial value INIT.
The subtype's `char-table-extra-slots' property determines its extra
slot count, which remains fixed for the lifetime of the table."
  (unless (symbolp subtype)
    (signal 'wrong-type-argument (list 'symbolp subtype)))
  (let* ((properties (symbol-plist subtype))
         (declared-p (plist-member properties 'char-table-extra-slots))
         (declared (get subtype 'char-table-extra-slots))
         (slots (cond
                 (declared-p (if (null declared) 0 declared))
                 ;; A removed display-table declaration means zero slots;
                 ;; the six-slot default is installed only at bootstrap.
                 ((eq subtype 'display-table) 0)
                 ;; The standalone does not preload these symbol properties.
                 ((assq subtype '((case-table . 3) (category-table . 2)))
                  (cdr (assq subtype '((case-table . 3)
                                       (category-table . 2)))))
                 ;; Subtypes without a declared count keep the legacy
                 ;; fixed capacity that existing callers rely on.
                 (t emacs-char-table--extra-slots))))
    (unless (and (integerp slots) (>= slots 0))
      (signal 'wrong-type-argument (list 'wholenump slots)))
    (let ((ct (make-vector 8 nil))
          (ascii (make-vector emacs-char-table--ascii-size init)))
      (emacs-char-table--raw-set ct 0 emacs-char-table--tag)
      (emacs-char-table--raw-set ct emacs-char-table--i-subtype subtype)
      (emacs-char-table--raw-set ct emacs-char-table--i-default init)
      (emacs-char-table--raw-set ct emacs-char-table--i-parent nil)
      (emacs-char-table--raw-set ct emacs-char-table--i-ascii ascii)
      ;; Owned: the ranges view is derived from the topology on demand.
      (emacs-char-table--raw-set
       ct emacs-char-table--i-ranges emacs-char-table--stale-ranges)
      (emacs-char-table--raw-set ct emacs-char-table--i-extra
                                 (make-vector slots nil))
      (emacs-char-table--raw-set
       ct emacs-char-table--i-owner-state
       (emacs-char-table--owner-state-create
        (emacs-char-table--topology-create init) init))
      ct)))

(defun emacs-char-table-p (object)
  "Return non-nil when OBJECT is a NeLisp char-table."
  ;; Tagged vectors are excluded by both native predicates below.  Check
  ;; their existing structural tag first to avoid repeating those class
  ;; checks on every access through the public `aref' bridge.
  ;; gnu562: dispatch on the representation once.  The three tests exclude
  ;; each other (a vector is never a record and `--native-p' rejects both),
  ;; so this answers as the former `or' did, without the record tests that
  ;; cost every `type-of' and struct predicate about 2 us.
  (cond ((vectorp object)
         (and (> (length object) 6)
              (eq (emacs-char-table--raw-ref object 0) emacs-char-table--tag)))
        ;; Only the bootstrap syntax record is a record char-table: test its
        ;; tag before the full check, which costs a buffer or struct ~1 us.
        ((recordp object)
         (and (eq (aref object 0) 'nelisp--syntax-table)
              (emacs-char-table--native-syntax-p object)))
        (t (emacs-char-table--native-p object))))

(defun emacs-char-table-same-p (first second)
  "Compare FIRST and SECOND by their logical char-table identity.
A retained bootstrap syntax record and its canonical sparse view name the
same table.  Host char-tables and other designators retain ordinary `eq'
identity.  This query does not modify either table."
  (eq (emacs-char-table--storage first) (emacs-char-table--storage second)))

(defun emacs-char-table-ascii-vector (ct)
  "Return CT's raw 256-slot ASCII vector (characters 0..255).
For a retained native char-table (one the native image built, such as
GNU `global-map's) return a fresh vector of its values for 0..255; it is
a copy, so writes to it do not reach CT."
  (if (emacs-char-table--native-p ct)
      (let ((vec (make-vector 256 nil)))
        (dotimes (i 256) (aset vec i (aref ct i)))
        vec)
    (emacs-char-table--raw-ref ct emacs-char-table--i-ascii)))

;;;; --- single-character access ----------------------------------------

(defun emacs-char-table--range-lookup (ct char)
  "Return the stored value for CHAR from CT's RANGES, or the symbol
`emacs-char-table--unset' when no range covers CHAR."
  (let ((ranges (emacs-char-table--raw-ref ct emacs-char-table--i-ranges))
        (result 'emacs-char-table--unset))
    (while (and ranges (eq result 'emacs-char-table--unset))
      (let* ((entry (car ranges))
             (key (car entry)))
        (when (and (<= (car key) char) (<= char (cdr key)))
          (setq result (cdr entry))))
      (setq ranges (cdr ranges)))
    result))

(defun emacs-char-table--fallback (ct char)
  "Return CT's own default or its parent's value for CHAR."
  (let ((default
         (emacs-char-table--raw-ref ct emacs-char-table--i-default))
        (parent
         (emacs-char-table--raw-ref ct emacs-char-table--i-parent)))
    (cond
     (default default)
     (parent (emacs-char-table-ref parent char))
     (t nil))))

(defvar emacs-char-table--fast-aref-p
  (and (fboundp 'emacs-char-table--native-vector-bridge-p)
       (emacs-char-table--native-vector-bridge-p))
  "Non-nil when `aref' on an owned char-table is dispatched natively.")

(defun emacs-char-table--owned-ascii-ref (ct char)
  "CT's cached value for CHAR below 128 when CT is an owned vector table.
The checks of `emacs-char-table--owned-p' and the read of
`emacs-char-table--owner-cache-ref', done once with direct slot reads where
the native vector bridge is active.  Answers `emacs-char-table--unset' when
that does not apply, leaving CT to the general path."
  (if (and (vectorp ct)
           (> (length ct) emacs-char-table--i-owner-state)
           (emacs-char-table--native-vector-bridge-p)
           (eq (nelisp--raw-aref ct 0) emacs-char-table--tag))
      ;; The two record tests are `emacs-char-table--owner-state-p' and
      ;; `emacs-char-table--topology-node-p' with the record's type slot
      ;; read directly (both types are symbols, so `type-of' answers the same).
      (let ((state (nelisp--raw-aref ct emacs-char-table--i-owner-state)))
        (if (and (recordp state)
                 (eq (nelisp--record-type state) 'emacs-char-table--owner-state)
                 (memq (length state) '(3 4)))
            (let* ((current (if (and (= (length state) 4)
                                     (>= (aref state 3)
                                         emacs-char-table--owner-state-version))
                                state
                              (emacs-char-table--owner-current-state ct)))
                   (cache (aref current 2)))
              (if (and (recordp cache)
                       (eq (nelisp--record-type cache)
                           emacs-char-table--topology-node-brand)
                       (= (length cache) 2))
                  (aref (aref cache 1) char)
                cache))
          'emacs-char-table--unset))
    'emacs-char-table--unset))

(defun emacs-char-table-ref (ct char)
  "Return CT's value for character CHAR (with default / parent fallback).
An owned table's ASCII entry goes through the native `aref' bridge, which
declines to `emacs-char-table-aref' and then `emacs-char-table-ref-slow'
(never back to this function), so there is no recursion."
  (if (and emacs-char-table--fast-aref-p
           (vectorp ct) (integerp char) (>= char 0) (< char 128)
           (= (length ct) 8)
           (eq (nelisp--raw-aref ct 0) emacs-char-table--tag))
      (aref ct char)
    (emacs-char-table-ref-slow ct char)))

(defun emacs-char-table-ref-slow (ct char)
  "Library implementation of `emacs-char-table-ref'."
  (cond
   ((emacs-char-table--native-p ct) (emacs-char-table--native-ref ct char))
   ((not (integerp char)) (emacs-char-table--raw-ref ct emacs-char-table--i-default))
   ((and (>= char 0) (< char emacs-char-table--ascii-size))
    (let ((v (let ((fast (if (< char 128)
                             (emacs-char-table--owned-ascii-ref ct char)
                           'emacs-char-table--unset)))
               (if (not (eq fast 'emacs-char-table--unset))
                   fast
                 (if (and (< char 128) (emacs-char-table--owned-p ct))
                     (emacs-char-table--owner-cache-ref ct char)
                   (emacs-char-table--raw-ref
                    (emacs-char-table--raw-ref ct emacs-char-table--i-ascii)
                    char))))))
      (when (and (>= char 128) (stringp v)
                 (emacs-char-table--unicode-property-p ct))
        (setq v (emacs-cc-chartab-1--uncompress ct char v)))
      (if v v (emacs-char-table--fallback ct char))))
   ((and (integerp char) (>= char emacs-char-table--ascii-size)
         (emacs-char-table--owned-p ct))
    (let ((v (emacs-char-table--topology-ref
              (emacs-char-table--owner-root ct) char)))
      (when (and (stringp v) (emacs-char-table--unicode-property-p ct))
        (setq v (emacs-cc-chartab-1--uncompress ct char v)))
      (if v v (emacs-char-table--fallback ct char))))
   (t
    (let ((v (emacs-char-table--range-lookup ct char)))
      (if (and (not (eq v 'emacs-char-table--unset)) v)
          v
        (emacs-char-table--fallback ct char))))))

(defun emacs-char-table-ascii-values (ct)
  "Return a fresh 128-element vector equal to (aref CT C) for C in 0..127, or nil.
Exact for an owned vector table, including its default and the parent chain
\(a parent that is not owned is read per char with `emacs-char-table-ref').
Answers nil for every other representation, so the caller keeps its per-char
`aref' there.  One pass replaces 128 bridged reads."
  (when (and (vectorp ct) (emacs-char-table--owned-p ct))
    (let* ((cache (emacs-char-table--owner-cache ct))
           (out (if (emacs-char-table--topology-node-p cache)
                    (copy-sequence (aref cache 1))
                  (make-vector 128 cache)))
           (default (emacs-char-table--raw-ref ct emacs-char-table--i-default))
           (parent (emacs-char-table--raw-ref ct emacs-char-table--i-parent)))
      (when (or default parent)
        (let ((pvals (and (not default)
                          (emacs-char-table-ascii-values parent)))
              (c 0))
          (while (< c 128)
            (unless (aref out c)
              (aset out c (cond (default default)
                                (pvals (aref pvals c))
                                (t (emacs-char-table-ref parent c)))))
            (setq c (1+ c)))))
      out)))

(defun emacs-char-table-set (ct char value)
  "Set CT's value for a single character CHAR to VALUE."
  (setq nelisp--chartab-lisp-epoch (1+ nelisp--chartab-lisp-epoch))
  (if (emacs-char-table--native-p ct)
      (emacs-char-table--native-set ct char value)
   (if (and (emacs-char-table--owned-p ct)
            (integerp char) (<= 0 char emacs-char-table--max-char))
       (emacs-char-table--owner-set-char ct char value)
    (if (and (integerp char) (>= char 0) (< char emacs-char-table--ascii-size))
      (emacs-char-table--raw-set
       (emacs-char-table--raw-ref ct emacs-char-table--i-ascii) char value)
    (emacs-char-table--raw-set
     ct emacs-char-table--i-ranges
     (cons (cons (cons char char) value)
           (emacs-char-table--raw-ref ct emacs-char-table--i-ranges))))))
  value)

;;;; --- category-table integration ------------------------------------

(defun emacs-char-table-make-category-table ()
  "Construct an empty standalone category table.
Each character maps to a 128-slot category set, matching the index space
of host Emacs's bool-vector returned by `char-category-set'."
  (let ((table (emacs-char-table-make
                'category-table
                (make-vector emacs-char-table--category-set-size nil))))
    ;; Extra slot 0 holds the 95 category docstrings (` ' through `~'), as
    ;; category.c does; the category primitives index it directly.
    (emacs-char-table-set-extra-slot table 0 (make-vector 95 nil))
    table))

(defun emacs-char-table-category-table-p (object)
  "Return non-nil when OBJECT is a standalone category table."
  (and (emacs-char-table-p object)
       (eq (emacs-char-table-subtype object) 'category-table)))

(defun emacs-char-table-standard-category-table ()
  "Return the standalone standard category table."
  (or emacs-char-table--standard-category-table
      (setq emacs-char-table--standard-category-table
            (emacs-char-table-make-category-table))))

(defun emacs-char-table-category-table ()
  "Return the standalone current category table."
  (or emacs-char-table--current-category-table
      (setq emacs-char-table--current-category-table
            (emacs-char-table-standard-category-table))))

(defun emacs-char-table-set-category-table (table)
  "Use category TABLE for the standalone current buffer and return TABLE."
  (unless (emacs-char-table-category-table-p table)
    (signal 'wrong-type-argument (list 'category-table-p table)))
  (setq emacs-char-table--current-category-table table))

(defun emacs-char-table-copy-category-table (&optional table)
  "Return a copy of category TABLE, defaulting to the standard table."
  (let* ((source (or table (emacs-char-table-standard-category-table)))
         (_ (unless (emacs-char-table-category-table-p source)
              (signal 'wrong-type-argument (list 'category-table-p source))))
         (copy (if (emacs-char-table--owned-p source)
                   (emacs-char-table--copy-storage source)
                 (emacs-char-table-copy
                  source
                  (lambda (value)
                    (if value (copy-sequence value) value)))))
         )
    ;; The default value is not visited by `emacs-char-table-copy's VALFN.
    ;; Copy it separately so direct `char-category-set' mutation in the copy
    ;; cannot alter the source table's default set.
    (let ((default (emacs-char-table--raw-ref copy emacs-char-table--i-default)))
      (when default
        (emacs-char-table--raw-set
         copy emacs-char-table--i-default (copy-sequence default))))
    ;; char-table copying duplicates the extra-slot vector but not values
    ;; stored inside it.  Category docstrings live in slot 0 and must remain
    ;; independent in the copied table.
    (let ((docs (emacs-char-table-extra-slot source 0)))
      (when docs
        (emacs-char-table-set-extra-slot copy 0 (copy-sequence docs))))
    (when (emacs-char-table--owned-p copy)
      (emacs-char-table-map
       (lambda (key value)
         (when value
           (let ((copied-value (copy-sequence value)))
             (cond
              ((integerp key)
               (emacs-char-table--owner-set-char copy key copied-value))
              ((consp key)
               (emacs-char-table--owner-set-range
                copy (car key) (cdr key) copied-value))
              (t
               (error "Invalid category-table map key: %S" key))))))
       copy))
    copy))

(defun emacs-char-table--category-set (table character)
  "Return TABLE's category set for CHARACTER, creating a valid default."
  (unless (and (integerp character)
               (>= character 0)
               (<= character emacs-char-table--max-char))
    (signal 'wrong-type-argument (list 'characterp character)))
  (let ((set (emacs-char-table-ref table character)))
    (if (and (vectorp set)
             (= (length set) emacs-char-table--category-set-size))
        set
      (let ((new (make-vector emacs-char-table--category-set-size nil)))
        (emacs-char-table-set table character new)
        new))))

(defun emacs-char-table-char-category-set (character)
  "Return CHARACTER's 128-slot standalone category set."
  (emacs-char-table--category-set (emacs-char-table-category-table) character))

(defun emacs-char-table-modify-category-entry
    (character category &optional table reset)
  "Add CATEGORY to CHARACTER's category set in TABLE.
CHARACTER may be one character or an inclusive (FROM . TO) range;
RESET removes CATEGORY.  CATEGORY must be defined in TABLE."
  (let (from to)
    (if (integerp character)
        (setq from character to character)
      (unless (consp character)
        (signal 'wrong-type-argument (list 'consp character)))
      (setq from (car character) to (cdr character)))
    (dolist (c (list from to))
      (unless (and (integerp c) (<= 0 c) (<= c emacs-char-table--max-char))
        (signal 'wrong-type-argument (list 'characterp c))))
    (unless (and (integerp category) (>= category #x20) (<= category #x7e))
      (signal 'wrong-type-argument (list 'categoryp category)))
    (let ((table (or table (emacs-char-table-category-table))))
      (unless (emacs-char-table-category-table-p table)
        (signal 'wrong-type-argument (list 'category-table-p table)))
      ;; Read the library's own docstring store when it is loaded: a host
      ;; Emacs `category-docstring' rejects this char-table representation.
      ;; Otherwise the host builtin only understands host category tables;
      ;; a standalone table carries no registered docstrings to validate.
      (unless (if (fboundp 'emacs-cc-category-1--docs-for)
                  (or (aref (emacs-cc-category-1--docs-for table)
                            (- category #x20))
                      (= category ?A))
                (or (not (fboundp 'category-docstring))
                    (not (category-table-p table))
                    (category-docstring category table)))
        (error "Undefined category: %c" category))
      (while (<= from to)
        ;; Copy the shared default before modifying an individual character.
        (let ((set (copy-sequence
                    (emacs-char-table--category-set table from))))
          (aset set category (not reset))
          (emacs-char-table-set table from set))
        (setq from (1+ from))))
    nil))

;;;; --- range access ---------------------------------------------------

(defun emacs-char-table--fill-ascii (ct from to value)
  "Store VALUE into CT's ASCII vector for chars FROM..TO (clamped 0..255)."
  (let ((i (max from 0))
        (hi (min to (1- emacs-char-table--ascii-size)))
        (vec (emacs-char-table--raw-ref ct emacs-char-table--i-ascii)))
    (while (<= i hi)
      (emacs-char-table--raw-set vec i value)
      (setq i (1+ i)))))

(defun emacs-char-table--owned-set-range (ct range value)
  "Apply RANGE to owned char-table CT, then return VALUE."
  (setq nelisp--chartab-lisp-epoch (1+ nelisp--chartab-lisp-epoch))
  (cond
   ((null range)
    (emacs-char-table--raw-set ct emacs-char-table--i-default value))
   ((eq range t)
    (let ((root (emacs-char-table--owner-root ct))
          (index 0))
      (while (< index emacs-char-table--topology-root-size)
        (aset root index value)
        (setq index (1+ index)))
      (emacs-char-table--owner-set-cache ct value)
      (emacs-char-table--owner-publish-span
       ct 0 emacs-char-table--max-char)))
   ((and (integerp range) (<= 0 range)
         (<= range emacs-char-table--max-char))
    (emacs-char-table-set ct range value))
   ((consp range)
    (let ((from (car range))
          (to (cdr range)))
      (dolist (character (list from to))
        (unless (and (integerp character) (<= 0 character)
                     (<= character emacs-char-table--max-char))
          (signal 'wrong-type-argument (list 'characterp character))))
      (emacs-char-table--owner-set-range ct from to value)))
   (t (error "Invalid RANGE argument to ‘set-char-table-range’")))
  value)

(defun emacs-char-table-set-range (ct range value)
  "Set CT entries selected by RANGE to VALUE.
RANGE is nil (the default value), t (the whole table), a character,
or a cons (FROM . TO).  Large supra-ASCII ranges are stored sparsely
rather than materialised."
  (setq nelisp--chartab-lisp-epoch (1+ nelisp--chartab-lisp-epoch))
  (emacs-char-table--check-table ct)
  (if (emacs-char-table--owned-p ct)
      (emacs-char-table--owned-set-range ct range value)
    (progn
  (cond
   ((emacs-char-table--native-p ct)
    ;; Native tables are written element-wise; only single characters and
    ;; bounded ranges are supported (the default slot is not reachable).
    (let ((from (cond ((integerp range) range) ((consp range) (car range))))
          (to (cond ((integerp range) range) ((consp range) (cdr range)))))
      (unless (and from to (<= 0 from) (<= to emacs-char-table--max-char))
        (error "Unsupported RANGE for a native char-table: %S" range))
      (while (<= from to)
        (emacs-char-table--native-set ct from value)
        (setq from (1+ from)))))
   ((null range)
    (emacs-char-table--raw-set ct emacs-char-table--i-default value))
   ((eq range t)
    (emacs-char-table--fill-ascii ct 0 (1- emacs-char-table--ascii-size) value)
    (emacs-char-table--raw-set
     ct emacs-char-table--i-ranges
     (list (cons (cons emacs-char-table--ascii-size
                       emacs-char-table--max-char) value))))
   ((and (integerp range) (<= 0 range)
         (<= range emacs-char-table--max-char))
    (emacs-char-table-set ct range value))
   ((consp range)
    (let ((from (car range))
          (to (cdr range)))
      (dolist (character (list from to))
        (unless (and (integerp character) (<= 0 character)
                     (<= character emacs-char-table--max-char))
          (signal 'wrong-type-argument (list 'characterp character))))
      (when (<= from to)
        (emacs-char-table--fill-ascii ct from to value)
        (when (>= to emacs-char-table--ascii-size)
          (emacs-char-table--raw-set
           ct emacs-char-table--i-ranges
           (cons (cons (cons (max from emacs-char-table--ascii-size) to)
                       value)
                 (emacs-char-table--raw-ref
                  ct emacs-char-table--i-ranges)))))))
   (t (error "Invalid RANGE argument to ‘set-char-table-range’")))
  value)))

(defun emacs-char-table-range (ct range)
  "Return CT's default or its value at RANGE's first character."
  (emacs-char-table--check-table ct)
  (cond
   ((and (emacs-char-table--native-p ct) (null range))
    (and (fboundp 'nelisp--chartab-char-table-range)
         (nelisp--chartab-char-table-range ct nil)))
   ((null range) (emacs-char-table--raw-ref ct emacs-char-table--i-default))
   ((and (integerp range) (<= 0 range)
         (<= range emacs-char-table--max-char))
    (emacs-char-table-ref ct range))
   ((consp range)
    (dolist (c (list (car range) (cdr range)))
      (unless (and (integerp c) (<= 0 c) (<= c emacs-char-table--max-char))
        (signal 'wrong-type-argument (list 'characterp c))))
    (emacs-char-table-ref ct (car range)))
   (t (error "Invalid RANGE argument to ‘char-table-range’"))))

;;;; --- parent / subtype / extra slots ---------------------------------

(defun emacs-char-table--check-table (ct)
  "Signal a type error unless CT is a char-table."
  (unless (emacs-char-table-p ct)
    (signal 'wrong-type-argument (list 'char-table-p ct))))

(defun emacs-char-table-parent (ct)
  "Return CT's parent char-table, or nil."
  (emacs-char-table--check-table ct)
  (if (emacs-char-table--native-p ct)
      (and emacs-char-table--native-parent-fn
           (funcall emacs-char-table--native-parent-fn ct))
    (emacs-char-table--raw-ref ct emacs-char-table--i-parent)))

(defun emacs-char-table-set-parent (ct parent)
  "Set CT's parent to PARENT (a char-table or nil).  Returns PARENT."
  (catch 'emacs-char-table--set-parent-done
  (setq nelisp--chartab-lisp-epoch (1+ nelisp--chartab-lisp-epoch))
  (emacs-char-table--check-table ct)
  (when (emacs-char-table--native-p ct)
    ;; A native table (e.g. a case table or GNU global-map's) takes a
    ;; native parent or nil through the core primitive, which bumps the
    ;; native char-table epoch itself.
    (if (and emacs-char-table--native-set-parent-fn
             (or (null parent) (emacs-char-table--native-p parent)))
        (progn (funcall emacs-char-table--native-set-parent-fn ct parent)
               (throw 'emacs-char-table--set-parent-done parent))
      (error "Cannot set a non-native parent on a native char-table")))
  (when parent
    (unless (emacs-char-table-p parent)
      (signal 'wrong-type-argument (list 'char-table-p parent)))
    ;; Bootstrap records and their sparse views have one logical identity.
    ;; Compare that identity, and reject a previously corrupted parent chain
    ;; rather than looping while attaching a new child.
    (let ((target (emacs-char-table--storage ct))
          (ancestor parent) (seen nil))
      (while (and ancestor (emacs-char-table-p ancestor)
                  (not (emacs-char-table--native-p ancestor)))
        (setq ancestor (emacs-char-table--storage ancestor))
        (when (eq ancestor target)
          (error "Attempt to make a chartable be its own parent"))
        (when (memq ancestor seen)
          (error "Cyclic char-table parent chain"))
        (push ancestor seen)
        (setq ancestor (emacs-char-table--raw-ref
                        ancestor emacs-char-table--i-parent)))))
  (emacs-char-table--raw-set ct emacs-char-table--i-parent parent)
  parent))

(defun emacs-char-table-subtype (ct)
  "Return CT's subtype."
  (emacs-char-table--check-table ct)
  (if (emacs-char-table--native-p ct)
      (and emacs-char-table--native-subtype-fn
           (funcall emacs-char-table--native-subtype-fn ct))
    (emacs-char-table--raw-ref ct emacs-char-table--i-subtype)))

(defun emacs-char-table-extra-slot (ct n)
  "Return CT's extra slot N."
  (emacs-char-table--check-table ct)
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'fixnump n)))
  (if (emacs-char-table--native-p ct)
      (if emacs-char-table--native-extra-slot-fn
          (funcall emacs-char-table--native-extra-slot-fn ct n)
        (signal 'args-out-of-range (list ct n)))
    (let ((slots (length (emacs-char-table--raw-ref
                          ct emacs-char-table--i-extra))))
      (unless (and (<= 0 n) (< n slots))
        (signal 'args-out-of-range (list ct n))))
    (emacs-char-table--raw-ref
     (emacs-char-table--raw-ref ct emacs-char-table--i-extra) n)))

(defun emacs-char-table-extra-slot-count (ct)
  "Return CT's exact allocated extra-slot count, or nil if unavailable.
Owned tagged-vector tables retain their allocated count in their storage;
native tables expose no count accessor, so this function does not infer it
from the mutable subtype property."
  (emacs-char-table--check-table ct)
  (unless (emacs-char-table--native-p ct)
    (length (emacs-char-table--raw-ref ct emacs-char-table--i-extra))))

(defun emacs-char-table-set-extra-slot (ct n value)
  "Set CT's extra slot N to VALUE."
  (emacs-char-table--check-table ct)
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'fixnump n)))
  (if (emacs-char-table--native-p ct)
      ;; Mirror `emacs-char-table-extra-slot': the runtime owns a native
      ;; table's slots and checks the index itself.
      (if emacs-char-table--native-set-extra-slot-fn
          (funcall emacs-char-table--native-set-extra-slot-fn ct n value)
        (signal 'args-out-of-range (list ct n)))
    (let ((slots (emacs-char-table--raw-ref ct emacs-char-table--i-extra)))
      (unless (and (<= 0 n) (< n (length slots)))
        (signal 'args-out-of-range (list ct n))))
    (emacs-char-table--raw-set
     (emacs-char-table--raw-ref ct emacs-char-table--i-extra) n value)))

;;;; --- iteration ------------------------------------------------------

(defun emacs-char-table--map-boundaries (ct)
  "Return sorted character boundaries of CT and its parent tables.
Only ASCII slots and sparse range endpoints are examined; the full
character space is never enumerated."
  (let ((boundaries (list 0 (1+ emacs-char-table--max-char)))
        (table ct))
    (while table
      (let* ((vec (emacs-char-table--raw-ref table emacs-char-table--i-ascii))
             (ranges (emacs-char-table--ranges table))
             ;; gnu562: resolve the storage once and read it with the same
             ;; primitive `emacs-char-table--raw-ref' would use per slot
             ;; (two of its calls per slot made an empty table's map cost
             ;; ~450 us).
             (store (if (recordp vec) (emacs-char-table--storage vec) vec))
             (bridge (emacs-char-table--native-vector-bridge-p))
             (raw emacs-char-table--raw-aref)
             (prev (if bridge (nelisp--raw-aref store 0) (funcall raw store 0)))
             (i 1))
        (while (< i emacs-char-table--ascii-size)
          (let ((cur (if bridge (nelisp--raw-aref store i) (funcall raw store i))))
            (unless (eq prev cur)
              (push i boundaries))
            (setq prev cur))
          (setq i (1+ i)))
        (push emacs-char-table--ascii-size boundaries)
        (dolist (entry ranges)
          (let ((from (max emacs-char-table--ascii-size (car (car entry))))
                (to (min emacs-char-table--max-char (cdr (car entry)))))
            (when (<= from to)
              (push from boundaries)
              (push (1+ to) boundaries)))))
      ;; A non-nil local default masks every parent entry.
      (setq table
            (unless (emacs-char-table--raw-ref table emacs-char-table--i-default)
              (emacs-char-table--raw-ref table emacs-char-table--i-parent))))
    (sort boundaries #'<)))

(defun emacs-char-table--map-run (function key from to value unicode-values)
  "Call FUNCTION for a non-nil run FROM..TO with VALUE.
Reuse KEY for ranges, as GNU Emacs does; callers must copy retained keys.
If UNICODE-VALUES is non-nil, decode VALUE before passing it to FUNCTION."
  (let ((decoded (if (and value unicode-values)
                     (emacs-cc-chartab-1--decode unicode-values value)
                   value)))
    (if (= from to)
        (when value (funcall function from decoded))
      (setcar key from)
      (setcdr key to)
      (when value (funcall function key decoded)))))

(defun emacs-char-table-map (function ct)
  "Call FUNCTION with (KEY VALUE) for each non-nil effective run of CT.
Adjacent characters with `eq' values form a range, including values
inherited from defaults and parents.  Return nil.
A retained native char-table (one the native image built, such as GNU
`global-map's) is mapped by the core's chartab.c port."
  (emacs-char-table--check-table ct)
  (if (and (emacs-char-table--native-p ct)
           (fboundp 'nelisp--chartab-map-char-table))
      (nelisp--chartab-map-char-table function ct)
  (let ((unicode-values (when (emacs-char-table--unicode-property-p ct)
                          (emacs-cc-chartab-1--prepare-map ct)))
        (boundaries (emacs-char-table--map-boundaries ct))
        (key (cons nil nil))
        (from 0)
        ;; GNU seeds the pending run from the first stored slot or parent,
        ;; before traversal applies the local default to nil slots.
        (value (let ((first (emacs-char-table--raw-ref
                             (emacs-char-table--raw-ref
                              ct emacs-char-table--i-ascii) 0))
                     (parent (emacs-char-table--raw-ref
                              ct emacs-char-table--i-parent)))
                 (or first
                     (if parent
                         (emacs-char-table-ref parent 0)
                       (emacs-char-table--raw-ref
                        ct emacs-char-table--i-default))))))
    (while boundaries
      (let ((next (car boundaries)))
        (when (and (>= next from) (<= next emacs-char-table--max-char))
          (let ((next-value (emacs-char-table-ref ct next)))
            (unless (eq value next-value)
              (emacs-char-table--map-run function key from (1- next) value
                                        unicode-values)
              (setq from next value next-value)))))
      (setq boundaries (cdr boundaries)))
    (emacs-char-table--map-run function key from emacs-char-table--max-char value
                              unicode-values)))
  nil)

(defun emacs-char-table-copy (ct &optional valfn)
  "Return a copy of CT.
When VALFN is non-nil it transforms each non-nil value (used by
`copy-keymap' to recurse into nested keymaps)."
  (if (emacs-char-table--native-p ct)
      ;; Re-home the 8-bit part of a native table into a library table.
      (let ((new (emacs-char-table-make 'syntax-table))
            (c 0))
        (while (< c emacs-char-table--ascii-size)
          (let ((v (emacs-char-table--native-ref ct c)))
            (when v
              (emacs-char-table-set new c (if valfn (funcall valfn v) v))))
          (setq c (1+ c)))
        new)
   (if (emacs-char-table--owned-p ct)
       (if valfn
           (error "VALFN copy of owned char-tables requires caller-specific mapping")
         (emacs-char-table--copy-storage ct))
   (let ((new (emacs-char-table-make
              (emacs-char-table--raw-ref ct emacs-char-table--i-subtype)
              (emacs-char-table--raw-ref ct emacs-char-table--i-default))))
    (emacs-char-table--raw-set
     new emacs-char-table--i-parent
     (emacs-char-table--raw-ref ct emacs-char-table--i-parent))
    (let ((src (emacs-char-table--raw-ref ct emacs-char-table--i-ascii))
          (dst (emacs-char-table--raw-ref new emacs-char-table--i-ascii))
          (i 0))
      (while (< i emacs-char-table--ascii-size)
        (let ((v (emacs-char-table--raw-ref src i)))
          (emacs-char-table--raw-set
           dst i (if (and valfn v) (funcall valfn v) v)))
        (setq i (1+ i))))
    (emacs-char-table--raw-set
     new emacs-char-table--i-ranges
     (mapcar (lambda (e)
               (cons (car e)
                     (if (and valfn (cdr e)) (funcall valfn (cdr e)) (cdr e))))
             (emacs-char-table--raw-ref ct emacs-char-table--i-ranges)))
    (emacs-char-table--raw-set
     new emacs-char-table--i-extra
     (copy-sequence
      (emacs-char-table--raw-ref ct emacs-char-table--i-extra)))
    new))))

;;;; --- public array bridge -------------------------------------------

(defun emacs-char-table--check-character (index)
  "Signal the host-compatible type error when INDEX is not a character."
  (unless (integerp index)
    (signal 'wrong-type-argument (list 'fixnump index)))
  (unless (and (>= index 0) (<= index emacs-char-table--max-char))
    (signal 'wrong-type-argument (list 'characterp index))))

(defun emacs-char-table-aref (array index)
  "Return ARRAY's element at INDEX, including sparse NeLisp char-tables."
  (if (emacs-char-table-p array)
      (progn
        (emacs-char-table--check-character index)
        (emacs-char-table-ref-slow array index))
    (emacs-char-table--raw-ref array index)))

(defun emacs-char-table-aset (array index value)
  "Set ARRAY's element at INDEX to VALUE, including NeLisp char-tables."
  (if (emacs-char-table-p array)
      (progn
        (emacs-char-table--check-character index)
        (emacs-char-table-set array index value))
    (emacs-char-table--raw-set array index value)))

(defun emacs-char-table-max-char (&optional unicode)
  "Return the largest character code, or Unicode maximum when UNICODE is non-nil."
  (if unicode #x10FFFF emacs-char-table--max-char))

(defun emacs-char-table--bootstrap-syntax-lookup (table char)
  "Return the class character for CHAR in either syntax-table representation."
  (let ((entry (emacs-char-table-ref table char)))
    (aref " .w_()'\"$\\/<>@!|" (logand (if entry (car entry) 2) 255))))

(defun emacs-char-table--bootstrap-syntax-put (table char class)
  "Store bootstrap CLASS in TABLE's canonical syntax storage."
  (emacs-char-table-set table char (emacs-char-table--syntax-entry class char)))

;;;; --- install unprefixed names ---------------------------------------

(when (emacs-char-table--standalone-p)
  ;; Keep ordinary arrays on the native bridge.  Syntax-table installation
  ;; normalizes retained records to their sparse views before exposing them
  ;; as the active table; the prefixed accessors also accept legacy records.
  (unless (emacs-char-table--native-vector-bridge-p)
    (fset 'aref #'emacs-char-table-aref)
    (fset 'aset #'emacs-char-table-aset))
  (when (fboundp 'nelisp--syntax-lookup)
    (fset 'nelisp--syntax-lookup #'emacs-char-table--bootstrap-syntax-lookup)
    (fset 'nelisp--syntax-put #'emacs-char-table--bootstrap-syntax-put))
  (fset 'char-table-p #'emacs-char-table-p)
  (fset 'make-char-table #'emacs-char-table-make)
  (fset 'char-table-range #'emacs-char-table-range)
  (fset 'set-char-table-range #'emacs-char-table-set-range)
  (fset 'char-table-parent #'emacs-char-table-parent)
  (fset 'set-char-table-parent #'emacs-char-table-set-parent)
  (fset 'char-table-subtype #'emacs-char-table-subtype)
  (fset 'char-table-extra-slot #'emacs-char-table-extra-slot)
  (fset 'set-char-table-extra-slot #'emacs-char-table-set-extra-slot)
  (fset 'map-char-table #'emacs-char-table-map)
  (fset 'max-char #'emacs-char-table-max-char)
  (fset 'make-category-table #'emacs-char-table-make-category-table)
  (fset 'category-table-p #'emacs-char-table-category-table-p)
  (fset 'category-table #'emacs-char-table-category-table)
  (fset 'standard-category-table #'emacs-char-table-standard-category-table)
  (fset 'set-category-table #'emacs-char-table-set-category-table)
  (fset 'copy-category-table #'emacs-char-table-copy-category-table)
  (fset 'char-category-set #'emacs-char-table-char-category-set)
  (fset 'modify-category-entry #'emacs-char-table-modify-category-entry))

(when (emacs-char-table--install-function-p 'char-table-p)
  (defalias 'char-table-p #'emacs-char-table-p))
(when (emacs-char-table--install-function-p 'make-char-table)
  (defalias 'make-char-table #'emacs-char-table-make))
(when (emacs-char-table--install-function-p 'char-table-range)
  (defalias 'char-table-range #'emacs-char-table-range))
(when (emacs-char-table--install-function-p 'set-char-table-range)
  (defalias 'set-char-table-range #'emacs-char-table-set-range))
(when (emacs-char-table--install-function-p 'char-table-parent)
  (defalias 'char-table-parent #'emacs-char-table-parent))
(when (emacs-char-table--install-function-p 'set-char-table-parent)
  (defalias 'set-char-table-parent #'emacs-char-table-set-parent))
(when (emacs-char-table--install-function-p 'char-table-subtype)
  (defalias 'char-table-subtype #'emacs-char-table-subtype))
(when (emacs-char-table--install-function-p 'char-table-extra-slot)
  (defalias 'char-table-extra-slot #'emacs-char-table-extra-slot))
(when (emacs-char-table--install-function-p 'set-char-table-extra-slot)
  (defalias 'set-char-table-extra-slot #'emacs-char-table-set-extra-slot))
(when (emacs-char-table--install-function-p 'map-char-table)
  (defalias 'map-char-table #'emacs-char-table-map))
(when (emacs-char-table--install-function-p 'max-char)
  (defalias 'max-char #'emacs-char-table-max-char))
(when (emacs-char-table--install-function-p 'make-category-table)
  (defalias 'make-category-table #'emacs-char-table-make-category-table))
(when (emacs-char-table--install-function-p 'category-table-p)
  (defalias 'category-table-p #'emacs-char-table-category-table-p))
(when (emacs-char-table--install-function-p 'category-table)
  (defalias 'category-table #'emacs-char-table-category-table))
(when (emacs-char-table--install-function-p 'standard-category-table)
  (defalias 'standard-category-table #'emacs-char-table-standard-category-table))
(when (emacs-char-table--install-function-p 'set-category-table)
  (defalias 'set-category-table #'emacs-char-table-set-category-table))
(when (emacs-char-table--install-function-p 'copy-category-table)
  (defalias 'copy-category-table #'emacs-char-table-copy-category-table))
(when (emacs-char-table--install-function-p 'char-category-set)
  (defalias 'char-category-set #'emacs-char-table-char-category-set))
(when (emacs-char-table--install-function-p 'modify-category-entry)
  (defalias 'modify-category-entry #'emacs-char-table-modify-category-entry))

;;;; --- equal ---------------------------------------------------------

(defun emacs-char-table--tagged-p (object)
  "Return non-nil when OBJECT is a tagged (library) char-table vector."
  (and (vectorp object)
       (> (length object) emacs-char-table--i-extra)
       (eq (emacs-char-table--raw-ref object 0) emacs-char-table--tag)))

(defun emacs-char-table--tagged-equal (a b)
  "GNU fns.c internal_equal for two tagged char-tables A and B.
Every slot is compared: subtype, default, parent, extra slots and the
contents tree element by element (an owned table's topology has GNU's
sub-char-table layout).  The derived ASCII cache and ranges view are not
compared; they are functions of the contents."
  (and (eq (emacs-char-table--raw-ref a emacs-char-table--i-subtype)
           (emacs-char-table--raw-ref b emacs-char-table--i-subtype))
       (equal (emacs-char-table--raw-ref a emacs-char-table--i-default)
              (emacs-char-table--raw-ref b emacs-char-table--i-default))
       (equal (emacs-char-table--raw-ref a emacs-char-table--i-parent)
              (emacs-char-table--raw-ref b emacs-char-table--i-parent))
       (equal (emacs-char-table--raw-ref a emacs-char-table--i-extra)
              (emacs-char-table--raw-ref b emacs-char-table--i-extra))
       (let ((owned-a (emacs-char-table--owned-p a))
             (owned-b (emacs-char-table--owned-p b)))
         (cond
          ((and owned-a owned-b)
           (equal (emacs-char-table--owner-root a)
                  (emacs-char-table--owner-root b)))
          ((or owned-a owned-b)
           ;; One sparse and one owned table: compare what they store.
           (and (equal (emacs-char-table-ascii-vector a)
                       (emacs-char-table-ascii-vector b))
                (equal (emacs-char-table--ranges a)
                       (emacs-char-table--ranges b))))
          (t
           (and (equal (emacs-char-table--raw-ref a emacs-char-table--i-ascii)
                       (emacs-char-table--raw-ref b emacs-char-table--i-ascii))
                (equal (emacs-char-table--ranges a)
                       (emacs-char-table--ranges b))))))))

;; The prelude's `equal' hands two char-tables to `nelisp--char-table-equal',
;; which reads native char-tables only.  Tagged tables compare here.
(when (fboundp 'nelisp--char-table-equal)
  (unless (fboundp 'emacs-char-table--native-char-table-equal)
    (defalias 'emacs-char-table--native-char-table-equal
      (symbol-function 'nelisp--char-table-equal)))
  (defun nelisp--char-table-equal (a b)
    "Return non-nil when char-tables A and B are `equal' (fns.c)."
    (let ((tagged-a (emacs-char-table--tagged-p a))
          (tagged-b (emacs-char-table--tagged-p b)))
      (cond
       ((and tagged-a tagged-b) (emacs-char-table--tagged-equal a b))
       ((or tagged-a tagged-b) nil)
       (t (emacs-char-table--native-char-table-equal a b))))))

(provide 'emacs-char-table)

;;; emacs-char-table.el ends here
