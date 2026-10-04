"""Exercise generation with preserved unit objects; never compile or issue proof."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


class StartupBuild(unittest.TestCase):
    def test_preserved_units_generate_current_source_bound_startup(self):
        lane = Path(__file__).resolve().parents[1]
        main = lane
        fixture = Path(os.environ["NELISP_ROOT_BUILD_FIXTURE"]).resolve()
        closure = json.loads((fixture / "root-prelink-ticket-gc-component.json").read_text())
        metadata = json.loads((fixture / "root-complete-unit-metadata.json").read_text())
        names = {record["unit"] for record in closure["records"]}
        selected = [entry for entry in metadata if entry["name"] in names]
        self.assertEqual({entry["name"] for entry in selected}, names)
        with tempfile.TemporaryDirectory(prefix="root-startup-build-") as directory:
            root = Path(directory)
            for relative in (
                "lisp/nelisp-native-load.el", "lisp/nelisp-runtime-reload-abi.el",
                "lisp/nelisp-native-raw-file.el",
            ):
                destination = root / relative
                destination.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(main / relative, destination)
            for relative in (
                "lisp/nelisp-native-rooted-startup-evidence.el",
                "lisp/nelisp-native-rooted-build-evidence.el",
                "scripts/nelisp-native-rooted-prelink-closure.py",
                "scripts/nelisp-native-rooted-direct-closure.py",
                "templates/nelisp-native-rooted-abi-proof.el.in",
            ):
                destination = root / relative
                destination.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(lane / relative, destination)
            builder = root / "scripts/nelisp-standalone-build.el"
            shutil.copyfile(main / "scripts/nelisp-standalone-build.el", builder)
            paths = [str(fixture / "source-view/target/standalone-units/linux-x86_64" /
                         Path(entry["path"]).name) for entry in selected]
            for path in paths:
                self.assertLessEqual(Path(path).stat().st_size, 16777216)
            data = json.loads((fixture / "root-bss-owner.json").read_text())
            # Preserve the genuine generated data layout as a source fixture.
            # This test does not attest a linked image or replay an old certificate.
            symbols = " ".join(
                '(:name %s :value %d :size %d :section bss :bind global :type object)' %
                (json.dumps(item["name"]), item["value"], item.get("size", 0))
                for item in data["symbols"])
            arena = '(:name %s :sections ((bss . %d)) :symbols (%s) :relocs nil)' % (
                json.dumps(data["unit"]), data["bss-size"], symbols)
            probe = root / "probe.el"
            probe.write_text(''';;; Source generation fixture -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-native-rooted-startup-evidence)
(defun decode-unit (object)
  (cond ((and (consp object) (eq (car object) :nelisp-cache-bytes-hex))
         (let* ((hex (cadr object)) (bytes (make-string (/ (length hex) 2) 0)) (i 0))
           (setq bytes (string-to-unibyte bytes))
           (while (< i (length bytes))
             (aset bytes i (string-to-number (substring hex (* i 2) (+ (* i 2) 2)) 16))
             (setq i (1+ i))) bytes))
        ((consp object) (cons (decode-unit (car object)) (decode-unit (cdr object))))
        (t object)))
(let ((units nil) (root (expand-file-name default-directory)))
  (dolist (path 'PATHS)
    (with-temp-buffer (insert-file-contents path)
      (push (decode-unit (read (current-buffer))) units)))
  (push 'ARENA units)
  (let ((source (nelisp-native-rooted-startup-evidence-build
                 units (expand-file-name "scripts/nelisp-standalone-build.el" root)
                 root (expand-file-name "generation" root))))
    (should (string-match-p "nelisp-native-rooted-abi-evidence" source))
    (should (string-match-p "ticket-gc-memory-v1" source))
    (should-not (string-match-p "NELISP_BUILD_EVIDENCE_PIN" source))
    (with-temp-buffer (emacs-lisp-mode) (insert source) (check-parens))
    (princ "SOURCE-BOUND-STARTUP-GENERATION-PASS\\n")))
'''.replace("PATHS", "(" + " ".join(map(json.dumps, paths)) + ")")
               .replace("ARENA", arena))
            result = subprocess.run([
                "emacs", "-Q", "--batch", "-L", str(main / "lisp"),
                "-L", str(root / "lisp"), "-l", str(root / "lisp/nelisp-native-load.el"),
                "-l", str(root / "lisp/nelisp-native-rooted-build-evidence.el"),
                "-l", str(probe),
            ], cwd=root, capture_output=True, text=True, timeout=30)
            self.assertEqual(result.returncode, 0, result.stderr[-1500:])
            self.assertIn("SOURCE-BOUND-STARTUP-GENERATION-PASS", result.stdout)


if __name__ == "__main__":
    unittest.main()
