#!/usr/bin/env python3
"""Frontend-independent GNU tty / NeLisp glyph-matrix comparison (S2.1).

Run from the lane root.  Only Python's standard library is needed.  Artifacts
include every -l corpus input, tty stream, cells, face runs and first differences.
The GNU oracle is real `emacs -Q -nw`, never format-mode-line in batch mode.
NeLisp starts from the identity-checked heap image, then loads the full existing
redisplay engine (the normal first-frame bundle contains its reduced core).
No oracle screen, window-start or calculated layout is fed back into NeLisp.
"""
import argparse
import codecs
import fcntl
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import pty
import re
import resource
import select
import signal
import struct
import subprocess
import sys
import termios
import time
import unicodedata

MARKER = b'\x1b]777;REDISPLAY-PARITY-DONE\x07'
# Canonical attributes: bold=1, italic=2, underline=4, inverse=8.
# vt100 deliberately provides a monochrome oracle; unknown SGR fails closed.


def char_width(char):
    if unicodedata.combining(char):
        return 0
    return 2 if unicodedata.east_asian_width(char) in ('W', 'F') else 1


class Screen:
    """Small streaming VT100/ANSI emulator, with cells and SGR attributes."""
    def __init__(self, width, height):
        self.width, self.height = width, height
        self.grid = [[[' ', 0] for _ in range(width)] for _ in range(height)]
        self.row = self.col = self.attr = 0
        self.top, self.bottom = 0, height - 1
        self.wrap = True
        self.pending_wrap = self.insert = False
        self.saved = (0, 0, 0)
        self.acs = False
        self.decoder = codecs.getincrementaldecoder('utf-8')('strict')
        self.pending = ''
        self.unknown = set()

    def blank(self):
        return [[' ', self.attr] for _ in range(self.width)]

    def newline(self):
        self.pending_wrap = False
        if self.row == self.bottom:
            self.grid.pop(self.top)
            self.grid.insert(self.bottom, self.blank())
        else:
            self.row = min(self.height - 1, self.row + 1)

    def csi(self, params, command):
        private = params.startswith('?')
        params = params.lstrip('?=>')
        values = [int(p or 0) for p in params.split(';')] if params else [0]
        n = values[0] or 1
        if command in 'Hf':
            self.row = min(self.height - 1, max(0, n - 1))
            self.col = min(self.width - 1, max(0, (values[1] or 1) - 1)) if len(values) > 1 else 0
        elif command == 'A': self.row = max(0, self.row - n)
        elif command == 'B': self.row = min(self.height - 1, self.row + n)
        elif command == 'C': self.col = min(self.width - 1, self.col + n)
        elif command == 'D': self.col = max(0, self.col - n)
        elif command == 'E': self.row, self.col = min(self.height - 1, self.row + n), 0
        elif command == 'F': self.row, self.col = max(0, self.row - n), 0
        elif command == 'G': self.col = min(self.width - 1, n - 1)
        elif command == 'd': self.row = min(self.height - 1, n - 1)
        elif command in 'JK':
            mode = values[0]
            for r in range(self.height):
                for c in range(self.width):
                    selected = (command == 'J' and (mode == 2 or (mode == 0 and (r, c) >= (self.row, self.col)) or (mode == 1 and (r, c) <= (self.row, self.col)))) or (command == 'K' and r == self.row and (mode == 2 or (mode == 0 and c >= self.col) or (mode == 1 and c <= self.col)))
                    if selected: self.grid[r][c] = [' ', self.attr]
        elif command == 'm':
            for value in values:
                if value == 0: self.attr = 0
                elif value == 1: self.attr |= 1
                elif value == 3: self.attr |= 2
                elif value == 4: self.attr |= 4
                elif value == 7: self.attr |= 8
                elif value == 22: self.attr &= ~1
                elif value == 23: self.attr &= ~2
                elif value == 24: self.attr &= ~4
                elif value == 27: self.attr &= ~8
                else: self.unknown.add('SGR ' + str(value))
        elif command == 'r':
            self.top = max(0, n - 1)
            self.bottom = min(self.height - 1, (values[1] or self.height) - 1) if len(values) > 1 else self.height - 1
            self.row = self.col = 0
        elif command == '@':
            row = self.grid[self.row]
            row[self.col:self.col] = [[' ', self.attr] for _ in range(n)]
            del row[self.width:]
        elif command == 'P':
            row = self.grid[self.row]
            del row[self.col:self.col+n]
            row.extend([[' ', self.attr] for _ in range(self.width - len(row))])
        elif command == 'X':
            for c in range(self.col, min(self.width, self.col+n)):
                self.grid[self.row][c] = [' ', self.attr]
        elif command in 'LMST':
            start = self.row if command in 'LM' else self.top
            for _ in range(n):
                if command in 'MS':
                    self.grid.pop(start); self.grid.insert(self.bottom, self.blank())
                else:
                    self.grid.pop(self.bottom); self.grid.insert(start, self.blank())
        elif command in 'hl':
            if private and 7 in values: self.wrap = command == 'h'
            if not private and 4 in values: self.insert = command == 'h'
            if not private and any(v != 4 for v in values): self.unknown.add('mode ' + params)
        elif command == 's': self.saved = (self.row, self.col, self.attr)
        elif command == 'u': self.row, self.col, self.attr = self.saved
        else: self.unknown.add('CSI ' + params + command)
        if command != 'm': self.pending_wrap = False

    def feed(self, data):
        text = self.pending + self.decoder.decode(data)
        i = 0
        while i < len(text):
            ch = text[i]
            if ch == '\x1b':
                if i + 1 >= len(text): break
                nxt = text[i+1]
                if nxt == '[':
                    match = re.match(r'\x1b\[([0-?]*)([ -/]*)([@-~])', text[i:])
                    if not match: break
                    self.csi(match[1], match[3]); i += len(match[0]); continue
                if nxt == ']':
                    match = re.search(r'\x07|\x1b\\', text[i+2:])
                    if not match: break
                    i += 2 + match.end(); continue
                if nxt in '()':
                    if i + 2 >= len(text): break
                    if nxt == '(': self.acs = text[i+2] == '0'
                    i += 3; continue
                if nxt == '7': self.saved = (self.row, self.col, self.attr)
                elif nxt == '8': self.row, self.col, self.attr = self.saved
                elif nxt == 'D': self.newline()
                elif nxt == 'E': self.newline(); self.col = 0
                elif nxt == 'M':
                    if self.row == self.top: self.grid.pop(self.bottom); self.grid.insert(self.top, self.blank())
                    else: self.row = max(0, self.row - 1)
                elif nxt not in '=>': self.unknown.add('ESC ' + nxt)
                i += 2; continue
            if ch == '\r': self.col = 0; self.pending_wrap = False
            elif ch == '\n': self.newline()
            elif ch == '\b': self.col = max(0, self.col - 1); self.pending_wrap = False
            elif ch == '\t': self.col = min(self.width-1, (self.col//8+1)*8); self.pending_wrap = False
            elif ch in '\x0e\x0f': self.acs = ch == '\x0e'
            elif ord(ch) >= 32 and ch != '\x7f':
                if self.acs: ch = {'q':'─', 'x':'│', 'l':'┌', 'k':'┐', 'm':'└', 'j':'┘'}.get(ch, ch)
                cw = char_width(ch)
                if cw == 0:
                    c = min(self.width-1, self.col - 1)
                    if c >= 0: self.grid[self.row][c][0] += ch
                else:
                    if self.pending_wrap and self.wrap: self.newline(); self.col = 0
                    self.pending_wrap = False
                    if self.insert: self.csi(str(cw), '@')
                    self.grid[self.row][self.col] = [ch, self.attr]
                    if cw == 2 and self.col+1 < self.width: self.grid[self.row][self.col+1] = ['', self.attr]
                    self.col += cw
                    if self.col >= self.width: self.col = self.width-1; self.pending_wrap = True
            i += 1
        self.pending = text[i:]


def lisp_string(value):
    return json.dumps(value, ensure_ascii=False)


def corpus():
    long = ''.join(str(i % 10) for i in range(175))
    lines = ''.join('line %02d\n' % i for i in range(1, 51))
    return [
        ('empty-buffer', '', ''),
        ('short-lines', 'alpha\nbeta\ngamma\n', ''),
        ('wrap-long', long+'\nEND\n', ''),
        ('truncate-long', long+'\nEND\n', '(parity-local truncate-lines t)'),
        ('wrap-exact-edge', 'x'*79+'\nEND', ''),
        ('wrap-multiple', 'x'*160+'\nEND', ''),
        ('invisible-text', 'abHIDDENcd\n', "(put-text-property 3 9 'invisible t)"),
        ('invisible-inactive', 'abVISIBLEcd\n', "(parity-local buffer-invisibility-spec nil) (put-text-property 3 10 'invisible 'hide)"),
        ('invisible-ellipsis', 'abHIDDENcd\n', "(parity-local buffer-invisibility-spec '((hide . t))) (put-text-property 3 9 'invisible 'hide)"),
        ('invisible-newline', 'abHIDDEN\ncd\n', "(put-text-property 3 10 'invisible t)"),
        ('display-string', 'abXcd\n', "(put-text-property 3 4 'display \"REPLACED\")"),
        ('display-range', 'abXXXXcd\n', "(put-text-property 3 7 'display \"R\")"),
        ('display-empty', 'abXXXXcd\n', "(put-text-property 3 7 'display \"\")"),
        ('display-space', 'abXcd\n', "(put-text-property 3 4 'display '(space :width 5))"),
        ('display-align-space', 'abXcd\n', "(put-text-property 3 4 'display '(space :align-to 12))"),
        ('overlay-before', 'abcd\n', '(let ((o (make-overlay 2 4))) (overlay-put o \'before-string "<"))'),
        ('overlay-after', 'abcd\n', '(let ((o (make-overlay 2 4))) (overlay-put o \'after-string ">"))'),
        ('overlay-empty', 'abcd\n', '(let ((o (make-overlay 3 3))) (overlay-put o \'before-string "<") (overlay-put o \'after-string ">"))'),
        ('overlay-invisible', 'abHIDDENcd\n', "(let ((o (make-overlay 3 9))) (overlay-put o 'invisible t))"),
        ('tabs', 'a\tb\tcc\n\tEND\n', ''),
        ('tabs-local-width', 'a\tb\tcc\n', '(parity-local tab-width 4)'),
        ('wide-japanese', '日本語の表示\nA日本B\n', ''),
        ('wide-wrap', '日'*45+'\nEND', ''),
        ('faces', 'bold under reverse plain\n', "(put-text-property 1 5 'face 'bold) (put-text-property 6 11 'face 'underline) (put-text-property 12 19 'face 'parity-inverse)"),
        ('face-tabs', 'a\tB\n', "(put-text-property 2 3 'face 'underline)"),
        ('header-line', 'body\nnext\n', '(parity-local header-line-format \'(" HEADER %b "))'),
        ('mode-line-default', 'body\n', '(parity-local mode-line-format parity-default-mode-line)'),
        ('split-right', 'left\nbody\n', '(parity-split \'right)'),
        ('split-below', 'top\nbody\n', '(parity-split \'below)'),
        ('minibuffer-prompt', 'body\n', '(setq parity-prompt "Prompt: value")'),
        ('point-scroll', lines, '(goto-char (point-max))'),
        ('window-start', lines, '(goto-char 161) (set-window-start (selected-window) 161)'),
        ('header-eval', 'body\nnext\n', '(parity-local header-line-format \'("Header " (:eval (buffer-name)) " %l"))'),
        ('header-styled-eval', 'body\n', '(parity-local header-line-format \'("plain " (:propertize (:eval "bold") face bold) " tail"))'),
        ('line-numbers', lines, '(display-line-numbers-mode 1)'),
        ('line-numbers-relative', lines, '(display-line-numbers-mode 1) (parity-local display-line-numbers \'relative) (goto-char 41)'),
        ('line-numbers-split', 'first\nsecond\nthird\n', "(display-line-numbers-mode 1) (parity-split 'right)"),
        ('line-numbers-wrap', long+'\nEND\n', '(display-line-numbers-mode 1)'),
        ('tabs-width-two', 'a\tb\t  c\n\tEND\n', '(parity-local tab-width 2)'),
        ('tabs-width-sixteen', 'a\tb\t c\n', '(parity-local tab-width 16)'),
        ('whitespace-mixed', '  leading\twords  \n\t\tindented\nend  ', '(parity-local tab-width 3)'),
        ('japanese-long-wrap', '日常の作業と編集'*18+'\n終わり\n', ''),
        ('japanese-mixed-wrap', ('日本 abc 語 XYZ '*14)+'\nEND', '(parity-local word-wrap t)'),
        ('scroll-many-bottom', ''.join('row %03d\n'%i for i in range(1,301)), '(goto-char (point-max))'),
        ('scroll-many-middle', ''.join('row %03d\n'%i for i in range(1,301)), '(goto-char 1601) (set-window-start (selected-window) 1601)'),
        ('fill-column-indicator-off', 'x'*100+'\nEND', '(parity-local display-fill-column-indicator nil) (parity-local fill-column 20)'),
        ('selective-display-indent', 'top\n  hidden one\n    hidden two\nnext\n', '(parity-local selective-display 2) (parity-local selective-display-ellipses t)'),
        ('selective-display-cr', 'visible\rhidden suffix\nnext\n', '(parity-local selective-display t) (parity-local selective-display-ellipses t)'),
        ('overlay-face-priority', 'abcdefghijk\n', "(let ((a (make-overlay 2 9)) (b (make-overlay 4 7))) (overlay-put a 'face 'bold) (overlay-put a 'priority 1) (overlay-put b 'face 'underline) (overlay-put b 'priority 10))"),
        ('overlay-face-cons-priority', 'abcdefghijk\n', "(let ((a (make-overlay 2 9)) (b (make-overlay 4 7))) (overlay-put a 'face 'bold) (overlay-put a 'priority '(nil . 2)) (overlay-put b 'face 'underline) (overlay-put b 'priority '(nil . 5)))"),
        ('region-highlight', 'alpha beta\ngamma delta\n', '(setq transient-mark-mode t) (set-mark 18) (goto-char 4) (setq mark-active t deactivate-mark nil)'),
        ('isearch-lazy-highlight', 'find match and match again\n', "(let ((a (make-overlay 6 11)) (b (make-overlay 16 21))) (overlay-put a 'face 'lazy-highlight) (overlay-put b 'face 'lazy-highlight))"),
        ('minibuffer-long-prompt', 'body\n', '(setq parity-prompt "A long prompt for a daily editing command with several descriptive words and a file name to complete: value")'),
        ('completions-buffer', 'Click on a completion to select it.\nIn this buffer, type RET to select the completion near point.\n\nPossible completions are:\nalpha      beta       gamma      delta\nepsilon    zeta       eta        theta\n', '(rename-buffer "*Completions*" t)'),
        ('completions-wide', 'Possible completions are:\n日本語候補    alpha-long-name    beta-long-name\n別の候補      gamma-long-name    delta-long-name\n', '(rename-buffer "*Completions*" t)'),
        ('mode-line-fields', lines, '(parity-local mode-line-format \'("%*%+ %12b %p L%l C%c (%m) %-")) (goto-char 41)'),
        ('mode-line-scroll-middle', lines, '(parity-local mode-line-format parity-default-mode-line) (goto-char 201) (set-window-start (selected-window) 201)'),
        ('mode-line-scroll-bottom', lines, '(parity-local mode-line-format parity-default-mode-line) (goto-char (point-max))'),
        ('mode-line-minor-modes', 'body\n', '(parity-local mode-line-format parity-default-mode-line) (parity-local mode-name "Text" major-mode \'text-mode) (parity-local minor-mode-alist \'((parity-minor " Minor"))) (parity-local parity-minor t)'),
        ('mode-line-inactive-face', 'body\n', '(parity-split \'below) (parity-face \'mode-line-inactive :inverse-video nil :underline t) (parity-local mode-line-format parity-default-mode-line)'),
        ('erase-display-properties', 'ab', "(put-text-property 1 3 'display \"X\") (erase-buffer) (insert \"abcdefghijkl\") (goto-char 1)"),
    ]


COMMON = r'''
(defvar parity-nelisp nil)
;; The standalone prelude's setq-local is a placeholder.  Establish real
;; local cells through the public buffer owner before writing their values.
(defmacro parity-local (&rest pairs)
  (let (forms)
    (while pairs
      (let ((sym (pop pairs)) (value (pop pairs)))
        (push `(if parity-nelisp
                   (emacs-buffer-set-buffer-local-toplevel-value ',sym ,value)
                 (set (make-local-variable ',sym) ,value)) forms)))
    (cons 'progn (nreverse forms))))
(defvar parity-prompt nil)
(defvar parity-state-path nil)
(defun parity-face (face &rest properties)
  (if parity-nelisp (emacs-redisplay-defface face properties)
    (apply #'set-face-attribute face nil properties)))
(defun parity-window-state ()
  (mapcar (lambda (w) (list (window-start w) (window-end w) (window-point w)
                            (window-total-width w) (window-total-height w)))
          (window-list nil 'nomini)))
(defun parity-save-state ()
  (let ((state (parity-window-state)))
    (when parity-state-path
      (with-temp-file parity-state-path (prin1 state (current-buffer))))))
(defvar parity-default-mode-line (default-value 'mode-line-format))
(defun parity-buffer (name text)
  (let ((b (get-buffer-create name)))
    (set-buffer b) (erase-buffer) (insert text) (goto-char 1)
    (parity-local truncate-lines nil)
    (parity-local word-wrap nil)
    (parity-local tab-width 8)
    (parity-local buffer-invisibility-spec t)
    (parity-local mode-line-format '(" %b "))
    (parity-local header-line-format nil)
    b))
(defun parity-split (side)
  (let ((w (selected-window)) (b (current-buffer))
        (other (split-window (selected-window) nil side)))
    (set-window-buffer other (parity-buffer "other" "other\nsecond\n"))
    (set-buffer b) (select-window w)))
'''

GNU_INIT = r'''
(setq inhibit-startup-screen t inhibit-startup-message t initial-scratch-message nil)
(menu-bar-mode -1)
(setq use-dialog-box nil echo-keystrokes 0 scroll-conservatively 0
      scroll-margin 0 scroll-step 0 auto-window-vscroll nil)
(dolist (entry '((mode-line :inverse-video t) (mode-line-inactive :inverse-video t)
                 (header-line :inverse-video t) (bold :weight bold)
                 (underline :underline t) (parity-inverse :inverse-video t)
                 (escape-glyph :weight normal) (vertical-border :weight normal) (region :underline t)
                 (lazy-highlight :weight bold) (line-number :weight normal)
                 (line-number-current-line :weight bold)))
  (unless (facep (car entry)) (make-face (car entry))))
(dolist (face '(mode-line mode-line-inactive header-line bold underline
                         parity-inverse escape-glyph vertical-border region lazy-highlight
                         line-number line-number-current-line))
  (set-face-attribute face nil :inherit nil :foreground 'unspecified
                      :background 'unspecified :weight 'normal :slant 'normal
                      :underline nil :inverse-video nil))
(set-face-attribute 'mode-line nil :inverse-video t)
(set-face-attribute 'mode-line-inactive nil :inverse-video t)
(set-face-attribute 'header-line nil :inverse-video t)
(set-face-attribute 'bold nil :weight 'bold)
(set-face-attribute 'underline nil :underline t)
(set-face-attribute 'parity-inverse nil :inverse-video t)
(set-face-attribute 'region nil :underline t)
(set-face-attribute 'lazy-highlight nil :weight 'bold)
(set-face-attribute 'line-number-current-line nil :weight 'bold)
'''

NELISP_RENDER = r'''
(defun parity-attrs (face)
  (+ (if (cdr (assq :bold face)) 1 0)
     (if (cdr (assq :italic face)) 2 0)
     (if (cdr (assq :underline face)) 4 0)
     (if (cdr (assq :reverse face)) 8 0)))
(defun parity-render (cols lines)
  (let* ((grid (make-vector lines nil)) (handle (emacs-redisplay-init))
         (windows (emacs-window-window-list)))
    (when parity-prompt
      (let* ((saved (current-buffer))
             (b (parity-buffer " *Minibuf*" parity-prompt))
             (height (min (max 1 (/ lines 4))
                          (1+ (/ (max 0 (1- (string-width parity-prompt))) (1- cols)))))
             (mini (emacs-window--make :id -1 :leaf-p t :buffer b
                    :point (1+ (length parity-prompt)) :start 1 :total-cols cols :total-lines height
                    :top-line (- lines height) :parameters '((minibuffer . t)))))
        (emacs-window-layout-frame cols lines 0 height)
        (setq windows (append windows (list mini)))
        (set-buffer saved)))
    (dotimes (r lines) (aset grid r (make-vector cols nil)))
    (dolist (w windows)
      (let* ((b (emacs-window-buffer w))
             (emacs-redisplay-truncate-lines (buffer-local-value 'truncate-lines b))
             (emacs-redisplay-word-wrap (buffer-local-value 'word-wrap b))
             (emacs-redisplay-default-tab-width (buffer-local-value 'tab-width b))
             (matrix (emacs-redisplay-redisplay-window handle w))
             (edges (emacs-window-window-edges w))
             (rows (emacs-redisplay-glyph-matrix-rows matrix)))
        (dotimes (r (length rows))
          (let* ((row (aref rows r)) (glyphs (emacs-redisplay-glyph-row-glyphs row))
                 (out (aref grid (+ r (nth 1 edges)))))
            (dotimes (c (emacs-redisplay-glyph-row-used row))
              (let ((g (aref glyphs c)))
                (when g
                  (aset out (+ c (car edges))
                        (cons (emacs-redisplay-glyph-char g)
                              (parity-attrs (emacs-redisplay-glyph-realized-face g))))
                  (when (> (emacs-redisplay-glyph-width g) 1)
                    (dotimes (d (1- (emacs-redisplay-glyph-width g)))
                      (when (< (+ c d 1 (car edges)) cols)
                        (aset out (+ c d 1 (car edges))
                              (cons (if (= (emacs-redisplay-glyph-char g) 32) 32 0) (parity-attrs (emacs-redisplay-glyph-realized-face g))))))))))))))
    (princ "PARITY-GRID-BEGIN\n")
    (dotimes (r lines)
      (princ "ROW|")
      (prin1 (aref grid r))
      (princ "\n"))
    (princ "PARITY-GRID-END\n")
    (princ "STATE|") (prin1 (parity-window-state)) (princ "\n")))
'''


def setup_case(name, text, form):
    return '\n'.join([
        '(delete-other-windows)',
        '(set-window-buffer (selected-window) (parity-buffer "parity" %s))' % lisp_string(text),
        '(setq parity-prompt nil)', form,
        # Set window point after property/setup forms.  This avoids an unrelated
        # difference in selected-window bookkeeping leaking into the renderer.
        '(set-window-point (selected-window) (point))',
    ])


def gnu_capture(emacs, file, width, height, timeout):
    master, slave = pty.openpty()
    fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', height, width, 0, 0))
    def session():
        os.setsid()
        fcntl.ioctl(0, termios.TIOCSCTTY, 0)
    process = subprocess.Popen([emacs, '-Q', '-nw', '-l', str(file)],
                               stdin=slave, stdout=slave, stderr=slave,
                               env={**os.environ, 'TERM': 'vt100', 'LC_ALL': 'C.UTF-8'},
                               preexec_fn=session)
    os.close(slave)
    data = b''
    try:
        deadline = time.monotonic() + timeout
        while MARKER not in data and time.monotonic() < deadline:
            if select.select([master], [], [], .1)[0]:
                try: chunk = os.read(master, 65536)
                except OSError: break
                if not chunk: break
                data += chunk
        if MARKER not in data: raise RuntimeError('GNU completion marker missing; exit=%s' % process.poll())
        data = data[:data.index(MARKER)]
        screen = Screen(width, height)
        screen.feed(data)
        if screen.pending or screen.unknown:
            raise RuntimeError('unsupported tty output: %r %r' % (screen.pending, sorted(screen.unknown)))
        return screen.grid, data
    finally:
        # Preserve failed oracle captures too (missing markers/unknown escapes).
        file.with_suffix('.raw').write_bytes(data)
        try: os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError: pass
        process.wait()
        os.close(master)


def face_runs(grid):
    runs = []
    for r, row in enumerate(grid):
        start = 0
        for c in range(1, len(row)+1):
            if c == len(row) or row[c][1] != row[start][1]:
                runs.append([r, start, c, row[start][1]])
                start = c
    return runs


def save_grid(path, grid):
    path.write_text(json.dumps(dict(cells=grid, faces=face_runs(grid)), ensure_ascii=False, indent=1)+'\n')
    path.with_suffix('.screen').write_text('\n'.join(''.join(c[0] for c in row) for row in grid)+'\n')


def image_path(lib, binary):
    env = {**os.environ, 'NELISP_BIN': str(binary)}
    result = subprocess.run(['bash', str(lib/'tools/c-core-image.sh'), 'path'],
                            env=env, text=True, capture_output=True, check=True)
    return Path(result.stdout.strip())


def engine_identity(base, engine):
    # The full engine loads its explicit shim after the reduced bootstrap.
    shim = engine.with_name('emacs-redisplay-builtins.el')
    pieces = [base.name, hashlib.sha256(engine.read_bytes()).hexdigest()]
    if shim.is_file():
        pieces.append(hashlib.sha256(shim.read_bytes()).hexdigest())
    return hashlib.sha256('\n'.join(pieces).encode()).hexdigest()


def engine_image(lib, binary, base, engine, cache, timeout):
    cache.mkdir(parents=True, exist_ok=True)
    key = engine_identity(base, engine)
    # Independent invocations may share a cache, including negative controls.
    with (cache/(key+'.lock')).open('a') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        return _engine_image_unlocked(lib, binary, base, engine, cache, timeout)


def _engine_image_unlocked(lib, binary, base, engine, cache, timeout):
    """Snapshot the unchanged full engine once; no compilation or native build."""
    cache.mkdir(parents=True, exist_ok=True)
    source_hash = hashlib.sha256(engine.read_bytes()).hexdigest()
    key = engine_identity(base, engine)
    image = cache/(key+'.flat')
    if image.is_file() and image.stat().st_size:
        return image, key
    temporary = cache/(key+'.tmp')
    form = ('(progn (load %s nil t) (setq redisplay-parity--engine-key %s) '
            '(unless (> (nelisp--arena-dump-image-stream %s) 0) '
            '(error "Full redisplay image dump failed")) '
            '(princ "PARITY-ENGINE-READY\\n") t)' %
            (lisp_string(str(engine)), lisp_string(key), lisp_string(str(temporary))))
    def limits():
        resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
    started = time.monotonic()
    result = subprocess.run([str(binary), '--cold-load-from', str(base), '--eval', form],
                            cwd=lib, stdin=subprocess.DEVNULL, capture_output=True,
                            timeout=max(180, timeout), preexec_fn=limits)
    (cache/(key+'.stdout')).write_bytes(result.stdout)
    (cache/(key+'.stderr')).write_bytes(result.stderr)
    if result.returncode or result.stderr or result.stdout != b'PARITY-ENGINE-READY\n' + b't\n':
        raise RuntimeError('full engine snapshot failed; see '+str(cache/(key+'.stderr')))
    if source_hash != hashlib.sha256(engine.read_bytes()).hexdigest():
        raise RuntimeError('engine changed during snapshot')
    temporary.replace(image)
    print('Full engine snapshot %.3fs: %s' % (time.monotonic()-started, image), flush=True)
    return image, key


def nelisp_capture(lib, binary, image, file, timeout):
    def limits():
        resource.setrlimit(resource.RLIMIT_STACK, (resource.RLIM_INFINITY, resource.RLIM_INFINITY))
    result = subprocess.run([str(binary), '--cold-load-from', str(image), '--eval',
                             '(progn (load %s nil t) t)' % lisp_string(str(file))],
                            cwd=lib, stdin=subprocess.DEVNULL, capture_output=True,
                            timeout=timeout, preexec_fn=limits,
                            env={**os.environ, 'NELISP_HOME': str(binary.parent.parent)})
    file.with_suffix('.stdout').write_bytes(result.stdout)
    file.with_suffix('.stderr').write_bytes(result.stderr)
    if result.returncode or result.stderr:
        raise RuntimeError('NeLisp exit=%d stderr=%s' % (result.returncode, result.stderr.decode(errors='replace')[:400]))
    output = result.stdout.decode()
    if output.count('PARITY-GRID-BEGIN\n') != 1 or output.count('PARITY-GRID-END\n') != 1:
        raise RuntimeError('NeLisp grid marker missing')
    rows = output.split('PARITY-GRID-BEGIN\n')[1].split('PARITY-GRID-END\n')[0].splitlines()
    grid = []
    for row in rows:
        if not row.startswith('ROW|'): raise RuntimeError('unexpected grid output')
        cells = []
        payload = row[4:]
        tokens = list(re.finditer(r'nil|\((\d+) \. (\d+)\)', payload))
        if re.sub(r'nil|\(\d+ \. \d+\)|[\[\]\s]', '', payload):
            raise RuntimeError('invalid cell vector: '+payload[:200])
        for token in tokens:
            char, attr = (32, 0) if token[0] == 'nil' else (int(token[1]), int(token[2]))
            cells.append([chr(char) if char else '', attr])
        grid.append(cells)
    state_lines = [line[6:] for line in output.splitlines() if line.startswith('STATE|')]
    if len(state_lines) != 1:
        raise RuntimeError('NeLisp window state missing')
    return grid, parse_window_state(state_lines[0])


def parse_window_state(text):
    if not re.fullmatch(r'\(\s*(?:\(\d+(?: \d+){4}\)\s*)+\)', text.strip()):
        raise RuntimeError('Malformed window state: '+text[:200])
    numbers = [int(n) for n in re.findall(r'\d+', text)]
    return [numbers[i:i+5] for i in range(0, len(numbers), 5)]


def self_test():
    s = Screen(5, 3)
    for data in [b'ab\x1b[', b'1;4mC\x1b[0m', '日'.encode()[:1], '日'.encode()[1:]]:
        s.feed(data)
    assert s.grid[0] == [['a',0],['b',0],['C',5],['日',0],['',0]]
    s.feed(b'Z\r\n\x1b[7mhi\x1b[0m\x1b[K')
    assert s.grid[1][0] == ['Z',0] and s.grid[2][0] == ['h',8]
    s.feed(b'\x1b[2;1H\x1b[2K')
    assert s.grid[1] == [[' ',0]]*5
    # Exercise the exact comparator used by run, not a separate equality check.
    assert compare_grids([[['x',0]]], [[['x',0]]])['passed']
    char = compare_grids([[['x',0]]], [[['y',0]]])
    face = compare_grids([[['x',0]]], [[['x',1]]])
    assert not char['passed'] and (char['characters'], char['faces']) == (1, 0)
    assert not face['passed'] and (face['characters'], face['faces']) == (0, 1)
    assert char['first_differences'][0]['col'] == 0
    assert face['first_differences'][0]['nelisp'] == ['x',1]
    assert parse_window_state('((1 8 1 80 23) (1 3 2 40 11))') == [[1,8,1,80,23],[1,3,2,40,11]]
    try:
        parse_window_state('((1 2 3))')
        raise AssertionError('bad state accepted')
    except RuntimeError:
        pass
    print('emulator self-test: PASS (streaming, wide cells, wrap, SGR, erase)')


def compare_grids(gnu, nelisp):
    """Compare every character and attribute; malformed grids cannot pass."""
    if len(gnu) != len(nelisp) or any(len(a) != len(b) for a,b in zip(gnu, nelisp)):
        raise RuntimeError('wrong grid dimensions')
    chars = faces = 0; diff = []
    for r, (oracle, actual) in enumerate(zip(gnu, nelisp)):
        for c, (a, b) in enumerate(zip(oracle, actual)):
            if a != b:
                chars += a[0] != b[0]
                faces += a[1] != b[1]
                if len(diff) < 12:
                    diff.append(dict(row=r, col=c, gnu=a, nelisp=b))
    return dict(passed=not (chars or faces), characters=chars, faces=faces,
                first_differences=diff)


def run(args):
    started = time.monotonic()
    harness_hash = hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
    lib = args.lib.resolve()
    binary = Path(args.nelisp).resolve()
    image = image_path(lib, binary)
    engine = (args.engine or lib/'packages/nelisp-emacs-core/src/emacs-redisplay.el').resolve()
    source_hash = hashlib.sha256(engine.read_bytes()).hexdigest()
    out = args.output.resolve(); out.mkdir(parents=True, exist_ok=True)
    base_image = image
    bundle_hash = hashlib.sha256((lib/'build/nemacs-bootstrap.el').read_bytes()).hexdigest()
    image, engine_key = engine_image(lib, binary, image, engine, out.parent/'redisplay-engine-images', args.timeout)
    if source_hash != hashlib.sha256(engine.read_bytes()).hexdigest():
        raise RuntimeError('engine changed before measurement; retry with a stable source')
    jobs = [(width, height, name, text, form)
            for width, height in [(80,24),(40,12)]
            for name, text, form in corpus()
            if not args.case or name in args.case]

    def one(job):
            width, height, name, text, form = job
            label = '%s-%dx%d' % (name, width, height)
            gnu_file, nelisp_file = out/(label+'.gnu.el'), out/(label+'.nelisp.el')
            setup = setup_case(name, text, form)
            gnu_file.write_text(';;; -*- lexical-binding: t; -*-\n'+COMMON+GNU_INIT+
                '(setq parity-state-path %s)\n' % lisp_string(str(gnu_file.with_suffix('.state')))+
                # Startup echo text can grow the 40-column minibuffer before
                # split-window runs.  Restore fixed geometry before setup.
                '\n(run-with-timer 0.1 nil (lambda ()\n(message nil)\n(redisplay t)\n'+setup+
                '\n(message nil)\n(redisplay t)\n'+
                '(if parity-prompt\n (minibuffer-with-setup-hook\n  (lambda () (insert "value") (redisplay t)\n   (parity-save-state) (send-string-to-terminal "\\e]777;REDISPLAY-PARITY-DONE\\a"))\n  (read-from-minibuffer (substring parity-prompt 0 (- (length parity-prompt) 5))))\n (progn (parity-save-state) (send-string-to-terminal "\\e]777;REDISPLAY-PARITY-DONE\\a")))))\n')
            # The same corpus forms run against the library's standard shim.
            nelisp_file.write_text(';;; -*- lexical-binding: t; -*-\n(progn\n'+
                '(unless (equal redisplay-parity--engine-key %s) (error \"Engine image identity mismatch\"))\n' % lisp_string(engine_key)+
                COMMON+NELISP_RENDER+
                '(setq parity-nelisp t)\n'+
                '(dolist (entry \'((bold :weight bold :inverse-video nil :underline nil) (underline :underline t :weight normal :inverse-video nil) (parity-inverse :inverse-video t) (header-line :inverse-video t) (region :underline t) (lazy-highlight :weight bold) (line-number :weight normal) (line-number-current-line :weight bold))) (emacs-redisplay-defface (car entry) (cdr entry)))\n'+
                '(emacs-window-layout-frame %d %d 0)\n' % (width, height)+setup+
                '\n(parity-render %d %d)\nt)\n' % (width, height))
            try:
                gnu, raw = gnu_capture(args.emacs, gnu_file, width, height, args.timeout)
                gnu_file.with_suffix('.raw').write_bytes(raw)
                save_grid(out/(label+'.gnu.json'), gnu)
                nelisp, nelisp_state = nelisp_capture(lib, binary, image, nelisp_file, args.timeout)
                gnu_state = parse_window_state(gnu_file.with_suffix('.state').read_text())
                if len(nelisp) != height or any(len(row) != width for row in nelisp): raise RuntimeError('wrong grid dimensions')
                save_grid(out/(label+'.nelisp.json'), nelisp)
                row = dict(case=label, **compare_grids(gnu, nelisp), gnu_windows=gnu_state, nelisp_windows=nelisp_state)
                row['window_state_passed'] = gnu_state == nelisp_state
                row['passed'] = row['passed'] and row['window_state_passed']
                chars, faces, diff = row['characters'], row['faces'], row['first_differences']
                print('%s %s chars=%d faces=%d%s' % ('PASS' if row['passed'] else 'FAIL', label, chars, faces,
                      ((' first='+str(diff[0])) if diff else '') + (' windows='+str((gnu_state, nelisp_state)) if gnu_state != nelisp_state else '')), flush=True)
            except (RuntimeError, OSError, subprocess.SubprocessError) as error:
                row = dict(case=label, passed=False, error=str(error))
                print('ERROR %s %s' % (label, error), flush=True)
            return row

    # Cases are independent processes; run them concurrently so the whole
    # corpus fits the ledger meter's 600 s ceiling (sequential: ~960 s).
    with concurrent.futures.ThreadPoolExecutor(max_workers=max(1, args.jobs)) as pool:
        result_rows = list(pool.map(one, jobs))
    report = dict(passed=sum(row['passed'] for row in result_rows), total=len(result_rows),
                  binary=str(binary), image=str(image), base_image=str(base_image), engine=str(engine),
                  engine_sha256=source_hash,
                  harness_sha256=harness_hash, elapsed_seconds=time.monotonic()-started,
                  binary_sha256=hashlib.sha256(binary.read_bytes()).hexdigest(),
                  bundle_sha256=bundle_hash,
                  cases=result_rows)
    (out/'report.json').write_text(json.dumps(report, indent=2)+'\n')
    print('S2.1: %d/%d case-size pairs identical; report=%s' % (report['passed'], report['total'], out/'report.json'))
    return 0 if result_rows and all(row['passed'] for row in result_rows) else 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=['run','self-test'])
    root = Path(__file__).resolve().parents[1]
    parser.add_argument('--lib', type=Path, default=root if (root/'packages').is_dir() else root/'lib')
    parser.add_argument('--nelisp', default=os.environ.get('NELISP_BIN'), required='NELISP_BIN' not in os.environ)
    parser.add_argument('--emacs', default=os.environ.get('EMACS','emacs'))
    parser.add_argument('--output', type=Path, default=Path('build/redisplay-layout-parity'))
    parser.add_argument('--case', action='append', choices=[c[0] for c in corpus()])
    parser.add_argument('--timeout', type=int, default=90)
    parser.add_argument('--jobs', type=int, default=int(os.environ.get('REDISPLAY_PARITY_JOBS', '4')))
    parser.add_argument('--engine', type=Path, help='explicit pristine engine for before/fix measurements')
    args = parser.parse_args()
    if args.command == 'self-test': self_test(); return 0
    return run(args)


if __name__ == '__main__':
    sys.exit(main())
