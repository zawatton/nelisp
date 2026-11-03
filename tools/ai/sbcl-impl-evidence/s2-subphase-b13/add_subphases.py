#!/usr/bin/env python3
"""Insert PREP_* phase events into a repinned phased driver (in place). Usage: add_subphases.py DRIVER"""
import re, sys
p = sys.argv[1]; s = open(p).read()
def once(s, needle, new):
    assert s.count(needle) == 1, needle
    return s.replace(needle, new)
s = once(s, '(nelisp-runtime-driver-preparation--verify-pins pins) (setenv "NELISP_FXIR_ROOT"',
         "(nelisp-phase-event 'PREP_CHECKS_BEGIN) (nelisp-runtime-driver-preparation--verify-pins pins) (nelisp-phase-event 'PREP_PINS_VERIFIED) (setenv \"NELISP_FXIR_ROOT\"")
for f, ev in (('nelisp-fxir-eval', 'HELPER'), ('nelisp-stdlib-prelude', 'PRELUDE'), ('nelisp-native-application-producer', 'PRODUCER')):
    ms = list(re.finditer(r'\(load "[^"]*/%s\.el" nil t t\)' % re.escape(f), s)); assert len(ms) == 1, f
    s = s[:ms[0].end()] + " (nelisp-phase-event 'PREP_%s_LOADED)" % ev + s[ms[0].end():]
s = once(s, '(receipt (nelisp-native-application-producer-install', "(_prep-preflight (nelisp-phase-event 'PREP_PREFLIGHT_DONE)) (receipt (nelisp-native-application-producer-install")
s = once(s, '(installed-identities ', "(_prep-install (nelisp-phase-event 'PREP_INSTALL_DONE)) (installed-identities ")
s = once(s, "(unless (functionp v8-exp--saved-fx-register-fn)\n  (error \"Saved image", "(nelisp-phase-event 'PREP_DRIVER_PREP_DONE)\n(unless (functionp v8-exp--saved-fx-register-fn)\n  (error \"Saved image")
open(p, 'w').write(s)
