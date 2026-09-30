;;; emacs-cc-treesit-3.el --- tree-sitter C primitive compatibility -*- lexical-binding: t; -*-

(unless (fboundp 'treesit-node-start)
  (defun treesit-node-start (node)
    "Return the NODE's start position in its buffer.
If NODE is nil, return nil."
    (when node
      (signal 'wrong-type-argument (list 'treesit-node-p node)))))

(unless (fboundp 'treesit-node-string)
  (defun treesit-node-string (node)
    "Return the string representation of NODE.
If NODE is nil, return nil."
    (when node
      (signal 'wrong-type-argument (list 'treesit-node-p node)))))

(unless (fboundp 'treesit-node-type)
  (defun treesit-node-type (node)
    "Return the NODE's type as a string.
If NODE is nil, return nil."
    (when node
      (signal 'wrong-type-argument (list 'treesit-node-p node)))))

(unless (fboundp 'treesit-parser-add-notifier)
  (defun treesit-parser-add-notifier (parser function)
    "Add FUNCTION to the list of PARSER's after-change notifiers.
FUNCTION must be a function symbol, rather than a lambda form.
FUNCTION should take 2 arguments, RANGES and PARSER.  RANGES is a list
of cons cells of the form (START . END), where START and END are buffer
positions.  PARSER is the parser issuing the notification."
    (unless (and (fboundp 'treesit-parser-p) (treesit-parser-p parser))
      (signal 'wrong-type-argument (list 'treesit-parser-p parser)))
    (unless (symbolp function)
      (signal 'wrong-type-argument (list 'symbolp function)))
    nil))

(unless (fboundp 'treesit-parser-buffer)
  (defun treesit-parser-buffer (parser)
    "Return the buffer of PARSER."
    (unless (and (fboundp 'treesit-parser-p) (treesit-parser-p parser))
      (signal 'wrong-type-argument (list 'treesit-parser-p parser)))))

(unless (fboundp 'treesit-parser-changed-regions)
  (defun treesit-parser-changed-regions (parser)
    "Force PARSER to re-parse and return the affected regions.

Return ranges as a list of (BEG . END).  If there's no need to re-parse
or no affected ranges, return nil."
    (unless (and (fboundp 'treesit-parser-p) (treesit-parser-p parser))
      (signal 'wrong-type-argument (list 'treesit-parser-p parser)))))

(unless (fboundp 'treesit-parser-create)
  (defun treesit-parser-create (language &optional buffer no-reuse tag)
    "Create and return a parser in BUFFER for LANGUAGE with TAG.

The parser is automatically added to BUFFER's parser list, as returned
by `treesit-parser-list'.  LANGUAGE is a language symbol.  If BUFFER is
nil or omitted, it defaults to the current buffer.  If BUFFER
already has a parser for LANGUAGE with TAG, return that parser, but if
NO-REUSE is non-nil, always create a new parser.

TAG can be any symbol except t, and defaults to nil.  Different
parsers can have the same tag.

If that buffer is an indirect buffer, its base buffer is used instead.
That is, indirect buffers use their base buffer's parsers.  Lisp
programs should widen as necessary should they want to use a parser in
an indirect buffer."
    (unless (symbolp language)
      (signal 'wrong-type-argument (list 'symbolp language)))
    (signal 'treesit-error (list "Tree-sitter is not available"))))

(unless (fboundp 'treesit-parser-delete)
  (defun treesit-parser-delete (parser)
    "Delete PARSER from its buffer's parser list.
See `treesit-parser-list' for the buffer's parser list."
    (unless (and (fboundp 'treesit-parser-p) (treesit-parser-p parser))
      (signal 'wrong-type-argument (list 'treesit-parser-p parser)))
    nil))

(unless (fboundp 'treesit-parser-embed-level)
  (defun treesit-parser-embed-level (parser)
    "Return PARSER's embed level.

The embed level can be either nil or a non-negative integer.  A value of
nil means the parser isn't part of the embedded parser tree.  The
primary parser has embed level 0, and each additional layer of parser
embedding increments the embed level by 1."
    (unless (and (fboundp 'treesit-parser-p) (treesit-parser-p parser))
      (signal 'wrong-type-argument (list 'treesit-parser-p parser)))
    nil))

(unless (fboundp 'treesit-parser-included-ranges)
  (defun treesit-parser-included-ranges (parser)
    "Return the ranges set for PARSER.
If no ranges are set for PARSER, return nil.
See also `treesit-parser-set-included-ranges'."
    (unless (and (fboundp 'treesit-parser-p) (treesit-parser-p parser))
      (signal 'wrong-type-argument (list 'treesit-parser-p parser)))
    nil))

(unless (fboundp 'treesit-parser-language)
  (defun treesit-parser-language (parser)
    "Return PARSER's language symbol.
This symbol is the one used to create the parser."
    (unless (and (fboundp 'treesit-parser-p) (treesit-parser-p parser))
      (signal 'wrong-type-argument (list 'treesit-parser-p parser)))))

(unless (fboundp 'treesit-parser-list)
  (defun treesit-parser-list (&optional buffer language tag)
    "Return BUFFER's parser list, filtered by LANGUAGE and TAG.

BUFFER defaults to the current buffer.  If that buffer is an indirect
buffer, its base buffer is used instead.  That is, indirect buffers
use their base buffer's parsers.

If LANGUAGE is non-nil, only return parsers for that language.

The returned list only contain parsers with TAG.  TAG defaults to nil.
If TAG is t, include parsers in the returned list regardless of their
tag."
    (when buffer
      (unless (bufferp buffer)
        (signal 'wrong-type-argument (list 'bufferp buffer))))
    (when language
      (unless (symbolp language)
        (signal 'wrong-type-argument (list 'symbolp language))))
    (when tag
      (unless (or (eq tag t) (symbolp tag))
        (signal 'wrong-type-argument (list 'symbolp tag))))
    nil))

(provide 'emacs-cc-treesit-3)
