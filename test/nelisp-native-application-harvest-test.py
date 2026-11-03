#!/usr/bin/env python3
"""Offline host orchestration tests; child fixtures are harmless host Python, never NeLisp."""
from __future__ import annotations

import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "tools/ai/nelisp-native-application-harvest.py"
SPEC = importlib.util.spec_from_file_location("harvest", SCRIPT)
harvest = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(harvest)


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def gnu_check_owned_capture(build_file, request_file, result, workdir):
    emacs = shutil.which(os.environ.get("NELISP_TEST_GNU_EMACS", "emacs"))
    if not emacs or not build_file:
        raise AssertionError("configured GNU capture verifier is required")
    script = Path(workdir) / "check-owned-capture.el"
    script.write_text(
        '(require \'nelisp-native-application-build)\n'
        '(let* ((request-file ' + json.dumps(str(request_file)) + ')\n'
        '       (capture-file ' + json.dumps(result["capture"]) + ')\n'
        '       (request-json (with-temp-buffer (insert-file-contents request-file) (buffer-string)))\n'
        '       (request (json-parse-string request-json :object-type \'hash-table))\n'
        '       (response (make-hash-table :test \'equal)))\n'
        '  (puthash "capture" capture-file response)\n'
        '  (puthash "output" ' + json.dumps(result["output"]) + ' response)\n'
        '  (puthash "status" "RAW_CAPTURED_NOT_ADMITTED" response)\n'
        '  (nelisp-native-application-build--check-owned-capture\n'
        '   response request-file request-json\n'
        '   (list :request request :pins (gethash "pins" request))))\n',
        encoding="utf-8")
    repo = Path(build_file).resolve().parents[1]
    return subprocess.run(
        [emacs, "--batch", "-Q", "-L", str(repo / "lisp"), "-l", str(build_file), "-l", str(script)],
        capture_output=True, text=True, timeout=20)


class HarvestAdapterTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = Path(self.tmp.name)
        self.repo = self.root / "repo"
        self.repo.mkdir()
        (self.repo / "trusted.el").write_text(";; trusted fixture\n")
        self.names = self.root / "names.txt"
        self.names.write_text("alpha-beta\ngamma\n")
        self.manifest = self.root / "manifest.el"
        # Envelope fixture only: Python must leave this Lisp data uninterpreted.
        self.manifest.write_text('(:schema 1 :kind :application-intake :names (alpha-beta gamma))\n')
        self.paths = {"repository": self.repo}
        for role in harvest.ROLES - {"repository"}:
            path = self.root / (role + ".bin")
            path.write_text("fixture:" + role + "\n")
            self.paths[role] = path
        self.paths["producer"].write_text(
            "(defun nelisp-native-application-producer-install "
            "(manifest-file names-file expected-manifest-sha256 expected-names-sha256 "
            "&optional expected-image-sha256 mode)\n")
        self.paths["names"] = self.names
        self.paths["application_manifest"] = self.manifest
        self.request = {
            "schema": "nelisp-native-application-harvest/v1",
            "pins": {role: {"path": str(path), "sha256": harvest._tree_sha(path) if role == "repository" else sha(path)}
                     for role, path in self.paths.items()},
            "mode": "selected-definitions", "names_sha256": sha(self.names), "id0": 31,
            "constant": "fixture-native-data", "output_parent": str(self.root),
        }
        self.request_path = self.root / "request.json"
        self.save_request()

    def tearDown(self):
        self.tmp.cleanup()

    def save_request(self):
        self.request_path.write_text(json.dumps(self.request))

    def fake_owned_run(self, request, pins, out, argv, env):
        self.assertEqual(argv, [str(pins["producer_image"]), "--load", str(out / "producer-driver.el")])
        self.assertEqual(env["NC1_ID0"], "31")
        self.assertEqual(env["NELISP_APPLICATION_MODE"], "selected-definitions")
        self.assertEqual(env["NC1_NAMES"], str(out / "names.txt"))
        self.assertNotIn("NELISP_INJECTED", env)
        self.assertNotIn("EMACSLOADPATH", env)
        child = out / "child"
        child.mkdir()
        # Exercise a terminal harmless host child. This is orchestration evidence only.
        proc = subprocess.run([sys.executable, "-c", "print('HOST_CHILD_DONE')"],
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=3)
        (child / "stdout").write_bytes(proc.stdout)
        (child / "stderr").write_bytes(proc.stderr)
        (child / "rc").write_text(str(proc.returncode))
        (child / "command.json").write_text(json.dumps({"cleanup": {"verified": True},
                                                          "timed_out": False, "cancelled": False,
                                                          "error": None}))
        (child / "launch.json").write_text("{}\n")
        (out / "install-receipt.el").write_text("(fixture-receipt)\n")
        (out / "rows.el").write_text("(fixture-rows)\n")
        return {"status": "host-fixture"}, "fixture-runner-ast-hash"

    def test_positive_host_child_and_fixed_driver(self):
        with mock.patch.object(harvest, "_owned_run", side_effect=self.fake_owned_run):
            result = harvest.run_request(self.request_path)
        self.assertEqual(result["status"], "RAW_CAPTURED_NOT_ADMITTED")
        out = Path(result["output"])
        driver = (out / "producer-driver.el").read_text()
        self.assertIn("nelisp-native-application-producer-install", driver)
        self.assertNotIn("nelisp-fxir-native-install-application", driver)
        self.assertIn("(load \"" + str(self.paths["harvester"]) + "\" nil t)", driver)
        self.assertEqual((out / "child/stdout").read_text().strip(), "HOST_CHILD_DONE")
        self.assertFalse((out / "capture.json").read_text().find('"accepted": true') >= 0)

    def test_driver_uses_current_producer_api_and_exact_arguments(self):
        pins = {role: path for role, path in self.paths.items()}
        driver = harvest._driver(pins, self.root / "receipt.el")
        producer_source = pins["producer"].read_text()
        self.assertIn("(defun nelisp-native-application-producer-install", producer_source)
        self.assertIn("expected-manifest-sha256 expected-names-sha256", producer_source)
        self.assertIn("&optional expected-image-sha256 mode)", producer_source)
        self.assertEqual(driver.count("nelisp-native-application-producer-install"), 1)
        self.assertNotIn("nelisp-fxir-native-install-application", driver)
        exact_call = """(nelisp-native-application-producer-install
                (getenv \"NELISP_APPLICATION_MANIFEST\")
                (getenv \"NC1_NAMES\")
                (getenv \"NELISP_APPLICATION_MANIFEST_SHA256\")
                (getenv \"NELISP_APPLICATION_NAMES_SHA256\")
                (getenv \"NELISP_PRODUCER_IMAGE_SHA256\")
                (if (equal (getenv \"NELISP_APPLICATION_MODE\") \"selected-definitions\")
                    :selected-definitions nil))"""
        self.assertIn(exact_call, driver)
        selected_mode = """(if (equal (getenv \"NELISP_APPLICATION_MODE\") \"selected-definitions\")
                    :selected-definitions nil)"""
        self.assertIn(selected_mode, driver)
        for field in ("NELISP_APPLICATION_MANIFEST_SHA256", "NELISP_APPLICATION_NAMES_SHA256",
                      "NELISP_PRODUCER_IMAGE_SHA256"):
            self.assertIn(f'(getenv "{field}")', driver)
        self.assertIn(":selected-definitions", driver)
        self.assertIn("(load " + harvest._lisp_string(str(pins["harvester"])) + " nil t)", driver)

    @unittest.skipUnless(shutil.which("emacs"), "GNU Emacs required for generated driver prefix")
    def test_generated_driver_prefix_loads_real_pinned_producer_with_tools_and_lisp_paths(self):
        helper = self.root / "helper.el"
        prelude = self.root / "prelude.el"
        driver_file = self.root / "producer-driver.el"
        helper.write_text(";; harmless fixture\n")
        prelude.write_text(";; harmless fixture\n")
        pins = {
            "repository": ROOT,
            "producer": ROOT / "tools/nelisp-native-application-producer.el",
            "helper": helper,
            "prelude": prelude,
            "harvester": self.root / "harvester.el",
        }
        driver_file.write_text(harvest._driver(pins, self.root / "receipt.el"))
        lisp = f"""(let ((driver {harvest._lisp_string(str(driver_file))})
      (producer {harvest._lisp_string(str(pins['producer']))})
      (root {harvest._lisp_string(str(ROOT))})
      (found nil))
  (unless (and (member (expand-file-name "lisp" root) load-path)
               (not (member (expand-file-name "tools" root) load-path)))
    (error "fixture must start with only the repository Lisp path"))
  (with-temp-buffer
    (insert-file-contents driver)
    (goto-char (point-min))
    (while (and (not found) (not (eobp)))
      (let ((form (read (current-buffer))))
        (eval form)
        (when (equal form (list 'load producer nil t))
          (setq found t)))))
  (unless (and found (featurep 'nelisp-native-application-producer)
               (fboundp 'nelisp-native-application-producer-install)
               (not (member (expand-file-name "tools" root) load-path)))
    (error "pinned producer was not loaded")))"""
        result = subprocess.run(
            [shutil.which("emacs"), "-Q", "--batch", "-L", str(ROOT / "lisp"),
             "--eval", lisp], stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            timeout=30, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)

    @unittest.skipUnless(shutil.which("emacs"), "GNU Emacs required for generated driver reader")
    def test_generated_driver_is_balanced_and_has_exact_complete_forms(self):
        helper = self.root / "helper.el"
        prelude = self.root / "prelude.el"
        harvester = self.root / "harvester.el"
        driver_file = self.root / "generated-whole-driver.el"
        helper.write_text(";; harmless fixture\n")
        prelude.write_text(";; harmless fixture\n")
        harvester.write_text(";; harmless fixture\n")
        pins = {
            "repository": self.repo,
            "producer": self.paths["producer"],
            "helper": helper,
            "prelude": prelude,
            "harvester": harvester,
        }
        driver_file.write_text(harvest._driver(pins, self.root / "receipt.el"))
        lisp = f"""(let ((driver {harvest._lisp_string(str(driver_file))})
      (harvester {harvest._lisp_string(str(harvester))})
      (forms nil) (eof nil))
  (with-temp-buffer
    (insert-file-contents driver)
    (check-parens)
    (goto-char (point-min))
    (while (not eof)
      (condition-case nil
          (push (read (current-buffer)) forms)
        (end-of-file (setq eof t))))
    (unless (= (point) (point-max))
      (error "driver reader stopped before EOF")))
  (setq forms (nreverse forms))
  (unless (= (length forms) 7)
    (error "expected 7 complete forms, read %d" (length forms)))
  (unless (equal (car (last forms)) (list 'load harvester nil t))
    (error "harvester load is not a separate final form"))
  (unless (eq (car (nth 5 forms)) 'let)
    (error "installer form is missing or misplaced"))
  (princ "forms=7 eof=t"))"""
        result = subprocess.run(
            [shutil.which("emacs"), "-Q", "--batch", "--eval", lisp],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=20, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("forms=7 eof=t", result.stdout)

    def test_driver_load_path_root_uses_escaped_lisp_string(self):
        pins = {role: path for role, path in self.paths.items()}
        odd_root = self.root / 'root "quoted" \\ path'
        pins["repository"] = odd_root
        driver = harvest._driver(pins, self.root / "receipt.el")
        root_literal = harvest._lisp_string(str(odd_root))
        self.assertIn(f'(setq default-directory {root_literal})', driver)
        self.assertNotIn(f'(expand-file-name "tools" {root_literal})', driver)
        self.assertIn(f'(expand-file-name "lisp" {root_literal})', driver)

    @unittest.skipUnless(shutil.which("emacs"), "GNU Emacs required for collision resolution")
    def test_tools_audit_collision_resolves_only_genuine_existing_package_provider(self):
        pins = {role: path for role, path in self.paths.items()}
        pins["repository"] = ROOT
        current = harvest._driver(pins, self.root / "receipt.el")
        old = current.replace('(list (expand-file-name "lisp"',
                              '(list (expand-file-name "tools" ' + harvest._lisp_string(str(ROOT)) + ')\n'
                              '                              (expand-file-name "lisp"', 1)
        for label, text, expected in (
                ("new", current, ROOT / "packages/nelisp-emacs-buffer-core/src/nelisp-emacs-compat.el"),
                ("old", old, ROOT / "tools/nelisp-emacs-compat.el")):
            path = self.root / (label + "-lookup.el")
            path.write_text(text)
            lisp = f"""(let ((driver {harvest._lisp_string(str(path))}))
  (setq load-path (cons {harvest._lisp_string(str(ROOT / 'packages/nelisp-emacs-buffer-core/src'))} load-path))
  (with-temp-buffer
    (insert-file-contents driver) (check-parens) (goto-char (point-min))
    (dotimes (_ 2) (eval (read (current-buffer)))))
  (unless (equal (locate-library "nelisp-emacs-compat") {harvest._lisp_string(str(expected))})
    (error "Wrong collision resolution")))"""
            result = subprocess.run([shutil.which("emacs"), "-Q", "--batch", "--eval", lisp],
                                    stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=20, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)

    @unittest.skipUnless(all(os.environ.get("NELISP_TEST_OWNED_" + key) and
                             os.environ.get("NELISP_TEST_OWNED_" + key + "_SHA256")
                             for key in ("LAUNCHER", "GUARD", "RUNNER")),
                         "pinned actual owned-runner inputs were not supplied")
    def test_actual_pinned_owned_runner_runs_harmless_host_child(self):
        pins = {}
        for role in ("LAUNCHER", "GUARD", "RUNNER"):
            path = Path(os.environ["NELISP_TEST_OWNED_" + role]).resolve(strict=True)
            expected = os.environ["NELISP_TEST_OWNED_" + role + "_SHA256"]
            self.assertEqual(harvest._sha(path), expected)
            pins[{"LAUNCHER": "launcher", "GUARD": "guard", "RUNNER": "owned_runner"}[role]] = path
        out = self.root / "actual-owned-run"
        out.mkdir(mode=0o700)
        unrelated = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(20)"],
                                     stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        try:
            report, runner_hash = harvest._owned_run(
                {}, pins, out,
                [sys.executable, "-c", "import os; print(sorted(k for k in os.environ if k.startswith(('NELISP_', 'V8_', 'FX_', 'EMACS'))))"],
                {"LC_ALL": "C"})
            child = out / "child"
            command = json.loads((child / "command.json").read_text())
            self.assertTrue(command["cleanup"]["verified"])
            self.assertFalse(command["timed_out"])
            self.assertFalse(command["cancelled"])
            self.assertIsNone(command["error"])
            self.assertEqual(command["rc"], 0)
            self.assertEqual(command["child"]["session"], command["child"]["pid"])
            self.assertEqual((child / "stdout").read_text().strip(), "[]")
            self.assertEqual((child / "stderr").read_bytes(), b"")
            self.assertTrue((child / "launch.json").is_file())
            self.assertTrue(report["children"])
            self.assertEqual(len(runner_hash), 64)
            self.assertIsNone(unrelated.poll(), "owned runner terminated an unrelated process")
            self.assertEqual({role: harvest._sha(path) for role, path in pins.items()},
                             {role: os.environ["NELISP_TEST_OWNED_" + key + "_SHA256"]
                              for role, key in (("launcher", "LAUNCHER"), ("guard", "GUARD"),
                                                ("owned_runner", "RUNNER"))})
        finally:
            if unrelated.poll() is None:
                unrelated.terminate()
                unrelated.wait(timeout=3)

    def test_broken_request_and_pin_hash_are_red(self):
        self.request["surprise"] = True
        self.save_request()
        with self.assertRaises(harvest.RequestError):
            harvest._request(self.request_path)
        self.request.pop("surprise")
        self.request["pins"]["producer"]["sha256"] = "0" * 64
        self.save_request()
        with self.assertRaises(harvest.RequestError):
            harvest._request(self.request_path)

    def test_symlinked_request_is_rejected_before_output_creation(self):
        link = self.root / "request-link.json"
        link.symlink_to(self.request_path)
        with self.assertRaises(harvest.RequestError):
            harvest.run_request(link)
        self.assertEqual(list(self.root.glob("nc1-owned-*")), [])

    def test_output_receipt_failure_rejects_without_touching_sentinel(self):
        sentinel = self.root / "provider-sentinel"
        sentinel.write_text("keep\n")
        def missing_receipt(request, pins, out, argv, env):
            child = out / "child"
            child.mkdir()
            (child / "stdout").write_bytes(b"terminal\n")
            (child / "stderr").write_bytes(b"")
            (child / "rc").write_text("0")
            (child / "command.json").write_text(json.dumps({"cleanup": {"verified": True},
                                                              "timed_out": False, "cancelled": False,
                                                              "error": None}))
            (child / "launch.json").write_text("{}\n")
            (out / "rows.el").write_text("rows\n")
            return {}, "mock"
        with mock.patch.object(harvest, "_owned_run", side_effect=missing_receipt):
            with self.assertRaises(harvest.RequestError):
                harvest.run_request(self.request_path)
        self.assertEqual(sentinel.read_text(), "keep\n")
        rejected = list(self.root.glob("nc1-owned-*/capture.json"))
        self.assertEqual(len(rejected), 1)
        self.assertIn('"rejected": true', rejected[0].read_text())

    def test_nonzero_child_and_missing_harvest_are_rejected(self):
        def completed_child(request, pins, out, argv, env, *, rc, rows):
            child = out / "child"
            child.mkdir()
            (child / "stdout").write_bytes(b"fixture output\n")
            (child / "stderr").write_bytes(b"")
            (child / "rc").write_text(str(rc))
            (child / "command.json").write_text(json.dumps({"cleanup": {"verified": True},
                                                              "timed_out": False, "cancelled": False,
                                                              "error": None}))
            (child / "launch.json").write_text("{}\n")
            (out / "install-receipt.el").write_text("(fixture-receipt)\n")
            if rows:
                (out / "rows.el").write_text("(fixture-rows)\n")
            return {"status": "fixture"}, "mock"
        for rc, has_rows, expected_message in ((7, True, "producer child failed"),
                                                (0, False, "harvest rows missing")):
            with self.subTest(rc=rc, has_rows=has_rows):
                with mock.patch.object(harvest, "_owned_run",
                                       side_effect=lambda req, pins, out, argv, env, rc=rc, rows=has_rows:
                                       completed_child(req, pins, out, argv, env, rc=rc, rows=rows)):
                    with self.assertRaisesRegex(harvest.RequestError, expected_message):
                        harvest.run_request(self.request_path)

    def test_runner_failure_is_not_success(self):
        with mock.patch.object(harvest, "_owned_run", side_effect=TimeoutError("fixture timeout")):
            with self.assertRaisesRegex(TimeoutError, "fixture timeout"):
                harvest.run_request(self.request_path)
        report = list(self.root.glob("nc1-owned-*/capture.json"))[0].read_text()
        self.assertIn("CAPTURE_REJECTED", report)

    def test_cleanup_failure_is_not_success(self):
        with mock.patch.object(harvest, "_owned_run", side_effect=AssertionError("cleanup not verified")):
            with self.assertRaisesRegex(AssertionError, "cleanup not verified"):
                harvest.run_request(self.request_path)
        report = list(self.root.glob("nc1-owned-*/capture.json"))[0].read_text()
        self.assertIn("CAPTURE_REJECTED", report)

    def test_actual_lisp_manifest_envelope_and_stale_hash(self):
        original = Path(os.environ["NELISP_TEST_ORIGINAL_INPUTS"])
        self.manifest.write_bytes((original / "manifest.el").read_bytes())
        self.names.write_bytes((original / "names.txt").read_bytes())
        for role in ("names", "application_manifest"):
            self.request["pins"][role]["sha256"] = sha(self.paths[role])
        self.request["names_sha256"] = sha(self.names)
        self.save_request()
        request, pins, names = harvest._request(self.request_path)
        self.assertEqual(len(names), 9)
        self.assertEqual(pins["application_manifest"].read_bytes(), (original / "manifest.el").read_bytes())
        self.assertEqual(request["pins"]["application_manifest"]["sha256"],
                         "b226478642d76413017c1dacadaa2f671ae0b6ef2d36d4b5c3f9ee8050f0de8c")
        self.manifest.write_bytes(self.manifest.read_bytes() + b"\n")
        with self.assertRaisesRegex(harvest.RequestError, "pin hash mismatch: application_manifest"):
            harvest._request(self.request_path)

    def test_duplicate_json_keys_and_constants_are_rejected(self):
        original = self.request_path.read_text()
        variants = [original.replace('"id0": 31', '"id0": 999, "id0": 31'),
                    original.replace('"path":', '"path": "ignored", "path":', 1),
                    original.replace('"id0": 31', '"id0": NaN')]
        for text in variants:
            with self.subTest(text=text[:60]):
                self.request_path.write_text(text)
                with self.assertRaises(harvest.RequestError):
                    harvest._request(self.request_path)
        self.assertEqual(list(self.root.glob("nc1-owned-*")), [])

    def test_postflight_rechecks_every_file_tree_and_raw_request(self):
        for role in sorted(harvest.ROLES) + ["request"]:
            with self.subTest(role=role):
                target = (self.repo / "trusted.el" if role == "repository" else
                          self.request_path if role == "request" else self.paths[role])
                before = target.read_bytes()
                def mutate(req, pins, out, argv, env):
                    result = self.fake_owned_run(req, pins, out, argv, env)
                    target.write_bytes(before + b"\n")
                    return result
                try:
                    with mock.patch.object(harvest, "_owned_run", side_effect=mutate):
                        with self.assertRaises(harvest.RequestError):
                            harvest.run_request(self.request_path)
                finally:
                    target.write_bytes(before)
        self.assertTrue(list(self.root.glob("nc1-owned-*/capture.json")))
        for capture in self.root.glob("nc1-owned-*/capture.json"):
            self.assertTrue(json.loads(capture.read_text())["rejected"])

    def test_output_parent_in_pinned_tree_rejected_before_creation(self):
        self.request["output_parent"] = str(self.repo)
        self.save_request()
        with self.assertRaisesRegex(harvest.RequestError, "inside pinned repository"):
            harvest.run_request(self.request_path)
        self.assertEqual(list(self.repo.glob("nc1-owned-*")), [])

    def test_child_output_symlinks_and_hardlinks_never_follow_external_targets(self):
        sentinel = self.root / "external-sentinel"
        sentinel.write_bytes(b"preserve external output\n")
        targets = ("rows.el", "install-receipt.el", "child/stdout", "child/stderr",
                   "child/rc", "child/command.json", "child/launch.json",
                   "names.txt", "producer-driver.el", "capture.json")
        for kind in ("symlink", "hardlink"):
            for relative in targets:
                with self.subTest(kind=kind, relative=relative):
                    def alias(req, pins, out, argv, env):
                        result = self.fake_owned_run(req, pins, out, argv, env)
                        path = out / relative
                        if path.exists():
                            path.unlink()
                        if kind == "symlink":
                            path.symlink_to(sentinel)
                        else:
                            os.link(sentinel, path)
                        return result
                    with mock.patch.object(harvest, "_owned_run", side_effect=alias):
                        with self.assertRaises(harvest.RequestError):
                            harvest.run_request(self.request_path)
                    self.assertEqual(sentinel.read_bytes(), b"preserve external output\n")
        self.assertEqual(len(list(self.root.glob("nc1-owned-*"))), 20)
        self.assertEqual(len(list(self.root.glob("nc1-owned-*/capture*.json"))), 22)

    def test_actual_full_run_request_with_original_lisp_and_pinned_owned_runner(self):
        # Reuse the independently reviewed actual-host-boundary fixture. The
        # harmless Python executable ignores Lisp; this proves orchestration only.
        for role, key in (("launcher", "LAUNCHER"), ("guard", "GUARD"), ("owned_runner", "RUNNER")):
            path = Path(os.environ["NELISP_TEST_OWNED_" + key])
            self.assertEqual(sha(path), os.environ["NELISP_TEST_OWNED_" + key + "_SHA256"])
            self.paths[role] = path
        original = Path(os.environ["NELISP_TEST_ORIGINAL_INPUTS"])
        self.manifest.write_bytes((original / "manifest.el").read_bytes())
        self.names.write_bytes((original / "names.txt").read_bytes())
        binary = self.root / 'host child "quoted" \\ path.py'
        binary.write_text('#!' + sys.executable + '\n' +
                          'import os\nfrom pathlib import Path\n' +
                          'Path(os.environ["NELISP_APPLICATION_INSTALL_RECEIPT"]).write_text("(HOST_FIXTURE_RECEIPT)\\n")\n' +
                          'Path(os.environ["NC1_OUT"]).write_text("(HOST_FIXTURE_ROWS)\\n")\n' +
                          'print("HOST_ONLY_FIXED_BOUNDARY")\n')
        binary.chmod(0o700)
        self.paths["producer_image"] = binary
        self.paths["producer"] = Path(os.environ["NELISP_TEST_PRODUCER_FILE"])
        self.assertEqual(sha(self.paths["producer"]),
                         "1587b5037de9cdd4d8d546d6991b984bdc6416965bdf9b11913f5fe57b5645d7")
        self.request["pins"] = {role: {"path": str(path), "sha256": harvest._tree_sha(path)
                                      if role == "repository" else sha(path)}
                                for role, path in self.paths.items()}
        self.request["names_sha256"] = sha(self.names)
        self.save_request()
        before = {role: harvest._tree_sha(path) if role == "repository" else sha(path)
                  for role, path in self.paths.items()}
        unrelated = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(20)"])
        try:
            with mock.patch.dict(os.environ, {"NELISP_REVIEW_INJECT": "must be scrubbed"}):
                result = harvest.run_request(self.request_path)
            out = Path(result["output"])
            captured_pins = json.loads(Path(result["capture"]).read_text())["pins"]
            self.assertEqual(set(captured_pins), harvest.ROLES)
            for role, requested in self.request["pins"].items():
                self.assertEqual(set(captured_pins[role]), {"path", "sha256"})
                self.assertEqual(captured_pins[role], requested)
            build_file = os.environ.get("NELISP_TEST_BUILD_FILE")
            if build_file:
                valid = gnu_check_owned_capture(build_file, self.request_path, result, self.root)
                self.assertEqual(valid.returncode, 0, valid.stderr)
                capture_file = Path(result["capture"])
                original = json.loads(capture_file.read_text())
                def reject(mutator):
                    changed = json.loads(json.dumps(original))
                    mutator(changed["pins"])
                    capture_file.write_text(json.dumps(changed))
                    rejected = gnu_check_owned_capture(build_file, self.request_path, result, self.root)
                    self.assertNotEqual(rejected.returncode, 0, rejected.stderr)
                reject(lambda pins: pins.__setitem__("repository", pins["repository"]["sha256"]))
                reject(lambda pins: pins["repository"].pop("path"))
                reject(lambda pins: pins["repository"].__setitem__("extra", "x"))
                reject(lambda pins: pins["repository"].__setitem__("path", pins["repository"]["path"] + "/."))
                reject(lambda pins: pins["repository"].__setitem__("path", pins["repository"]["path"].replace("/", "/./", 1)))
                reject(lambda pins: pins["repository"].__setitem__("sha256", "0" * 64))
                reject(lambda pins: pins.__setitem__("repository", pins["launcher"]))
                reject(lambda pins: pins.pop("repository"))
                reject(lambda pins: pins.__setitem__("unexpected", pins["launcher"]))
                capture_file.write_text(json.dumps(original))
            command = json.loads((out / "child/command.json").read_text())
            self.assertEqual(command["argv"], [str(binary), "--load", str(out / "producer-driver.el")])
            self.assertEqual(command["timeout"], 45)
            self.assertTrue(command["cleanup"]["verified"])
            self.assertEqual(command["cleanup"]["remaining"], [])
            self.assertEqual(command["child"]["session"], command["child"]["pid"])
            self.assertNotIn("NELISP_REVIEW_INJECT", command["environment"])
            self.assertEqual((out / "child/stdout").read_text(), "HOST_ONLY_FIXED_BOUNDARY\n")
            self.assertEqual((out / "child/stderr").read_bytes(), b"")
            self.assertIsNone(unrelated.poll())
            self.assertEqual(before, {role: harvest._tree_sha(path) if role == "repository" else sha(path)
                                      for role, path in self.paths.items()})
            self.assertEqual(os.stat(out).st_mode & 0o777, 0o700)
            cli = subprocess.run([sys.executable, str(SCRIPT), str(self.request_path)],
                                 capture_output=True, text=True, timeout=10)
            self.assertEqual(cli.returncode, 0, cli.stderr)
            self.assertEqual(cli.stderr, "")
            self.assertEqual(len(cli.stdout.splitlines()), 1)
            cli_result = json.loads(cli.stdout)
            self.assertEqual(cli_result["status"], "RAW_CAPTURED_NOT_ADMITTED")
            self.assertTrue(Path(cli_result["capture"]).is_file())
            self.assertIsNone(unrelated.poll())
            evidence = os.environ.get("NELISP_TEST_CAPTURE_COPY")
            if evidence:
                shutil.copytree(out, evidence)
                Path(evidence, "fixture-request.json").write_text(self.request_path.read_text())
                Path(evidence, "fixture-manifest.el").write_bytes(self.manifest.read_bytes())
                Path(evidence, "fixture-names.txt").write_bytes(self.names.read_bytes())
                Path(evidence, "cli-handoff.json").write_text(cli.stdout)
        finally:
            if unrelated.poll() is None:
                unrelated.terminate()
                unrelated.wait(timeout=3)


if __name__ == "__main__":
    unittest.main(verbosity=2)
