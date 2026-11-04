#!/usr/bin/env python3
"""Insert PREP_* phase events into a repinned phased driver without prelude/helper loads (in place)."""
import re, sys
p = sys.argv[1]; s = open(p).read()
def once(s, a, b):
    assert s.count(a) == 1, a
    return s.replace(a, b)
s = once(s, '(nelisp-runtime-driver-preparation--verify-pins pins) (setenv "NELISP_FXIR_ROOT"',
         "(nelisp-phase-event 'PREP_CHECKS_BEGIN) (nelisp-runtime-driver-preparation--verify-pins pins) (nelisp-phase-event 'PREP_PINS_VERIFIED) (setenv \"NELISP_FXIR_ROOT\"")
assert not re.search(r'\(load "[^"]*/(nelisp-stdlib-prelude|test/nelisp-fxir-eval)\.el" nil t t\)', s)
m = list(re.finditer(r'\(load "[^"]*/nelisp-native-application-producer\.el" nil t t\)', s)); assert len(m) == 1
s = s[:m[0].end()] + " (nelisp-phase-event 'PREP_PRODUCER_LOADED)" + s[m[0].end():]
s = once(s, '(receipt (nelisp-native-application-producer-install', "(_prep-preflight (nelisp-phase-event 'PREP_PREFLIGHT_DONE)) (receipt (nelisp-native-application-producer-install")
s = once(s, '(installed-identities ', "(_prep-install (nelisp-phase-event 'PREP_INSTALL_DONE)) (installed-identities ")
s = once(s, "(unless (functionp v8-exp--saved-fx-register-fn)\n  (error \"Saved image", "(nelisp-phase-event 'PREP_DRIVER_PREP_DONE)\n(unless (functionp v8-exp--saved-fx-register-fn)\n  (error \"Saved image")
open(p, 'w').write(s)
