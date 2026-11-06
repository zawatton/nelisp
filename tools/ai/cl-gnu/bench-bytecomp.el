;;; bench-bytecomp.el --- (require 'bytecomp) time  -*- lexical-binding: t; -*-
(require 'bytecomp)
(princ (format "BENCH-BYTECOMP %S\n" (featurep 'bytecomp)))
