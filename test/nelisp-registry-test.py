"""Registry snapshots through real HTTPS and the public project frontend."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("https_fixture", ROOT / "test/nelisp-package-store-test.py")
fixture = importlib.util.module_from_spec(spec)
spec.loader.exec_module(fixture)
from nelisp_registry import load_index, parse_index, search_index


class RegistryContracts(unittest.TestCase):
    def setUp(self):
        self.https = fixture.PackageStore()
        self.https.setUp()
        self.addCleanup(self.https.doCleanups)
        self.root = Path(self.https.temp.name)
        self.url = self.https.locked["url"].replace("source", "index")
        self.https.responses["/source"] = self.https.content
        release = {key: value for key, value in self.https.locked.items() if key != "name"}
        release["yanked"] = False
        self.index = {"schema_version": 1, "packages": {"example": [release]}}
        self.https.content = json.dumps(self.index).encode()

    def cli(self, *args, cwd=None):
        return subprocess.run([sys.executable, str(ROOT / "tools/nelisp-project.py"), *args],
                              cwd=cwd or self.root, capture_output=True, text=True, timeout=20)

    def test_search_add_and_offline_update(self):
        result = self.cli("search", "exam", "--registry", self.url, "--json")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout), [{"name": "example", "version": "1.0.0"}])
        self.assertEqual(self.cli("new", "hello").returncode, 0)
        project = self.root / "hello"
        result = self.cli("add", "example", "--registry", self.url, cwd=project)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("/source", self.https.requests)
        requests = list(self.https.requests)
        result = self.cli("update", "--registry", self.url, "--offline", cwd=project)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.https.requests, requests)

    def test_failed_refresh_keeps_prior_snapshot_and_bad_cache_fails(self):
        self.assertEqual(load_index(registry=self.url), self.index)
        snapshot = next((self.https.cache / "indexes-v1").glob("*.json"))
        before = snapshot.read_bytes()
        self.https.content = b'{"schema_version": 99, "packages": {}}'
        with self.assertRaises(ValueError):
            load_index(registry=self.url)
        self.assertEqual(snapshot.read_bytes(), before)
        self.assertEqual(load_index(registry=self.url, offline=True), self.index)
        envelope = json.loads(before)
        envelope["index"] += " "
        snapshot.write_text(json.dumps(envelope))
        with self.assertRaisesRegex(ValueError, "integrity"):
            load_index(registry=self.url, offline=True)

    def test_explicit_local_index_and_environment_registry(self):
        os.environ["NELISP_REGISTRY"] = self.url
        self.addCleanup(os.environ.pop, "NELISP_REGISTRY", None)
        self.assertEqual(load_index(), self.index)
        local = self.root / "index.json"
        local.write_text(json.dumps(self.index))
        requests = list(self.https.requests)
        self.assertEqual(load_index(local=local, offline=True), self.index)
        self.assertEqual(requests, self.https.requests)
        with self.assertRaises(ValueError):
            parse_index(b'{"schema_version":1,"schema_version":1,"packages":{}}')

    def test_tls_redirect_size_and_url_bound_offline_cache(self):
        with patch.dict(os.environ, SSL_CERT_FILE=str(self.root / "absent-ca")):
            with self.assertRaises(OSError):
                load_index(registry=self.url)
        with self.assertRaises(ValueError):
            load_index(registry=self.url.replace("index", "redirect"))
        with patch("nelisp_registry.MAX_INDEX_BYTES", 8), self.assertRaisesRegex(ValueError, "size"):
            load_index(registry=self.url)
        self.assertFalse((self.https.cache / "indexes-v1").exists())
        load_index(registry=self.url)
        with self.assertRaisesRegex(ValueError, "no cached"):
            load_index(registry=self.url + "?other=1", offline=True)

    def test_search_ignores_yanked_latest_release(self):
        newer = dict(self.index["packages"]["example"][0], version="1.9.0", yanked=True)
        self.index["packages"]["example"].append(newer)
        self.assertEqual(search_index(self.index, "EXAM"), [{"name": "example", "version": "1.0.0"}])
        self.assertEqual(search_index(self.index, "absent"), [])


if __name__ == "__main__":
    result = unittest.main(exit=False).result
    findings = len(result.failures) + len(result.errors)
    print(f'GATE-COUNT checked={result.testsRun} findings={findings}')
    raise SystemExit(not result.wasSuccessful())
