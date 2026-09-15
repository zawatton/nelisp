"""Real local HTTPS acquisition, cache reuse, and failure publication checks."""
import hashlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import os
from pathlib import Path
import ssl
import subprocess
import sys
import tempfile
import threading
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "tools"))
from nelisp_package_store import source_bytes


class PackageStore(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="nelisp-https-")
        self.addCleanup(self.temp.cleanup)
        root = Path(self.temp.name)
        cert, key = root / "cert.pem", root / "key.pem"
        subprocess.run(["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes",
                        "-keyout", str(key), "-out", str(cert), "-days", "1",
                        "-subj", "/CN=localhost", "-addext", "subjectAltName=IP:127.0.0.1"],
                       check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        self.content = b"(provide 'example)\n"
        self.requests = []
        self.responses = {}
        owner = self

        class Handler(BaseHTTPRequestHandler):
            def do_GET(self):
                owner.requests.append(self.path)
                if self.path == "/redirect":
                    self.send_response(302)
                    self.send_header("Location", "http://127.0.0.1/unsafe")
                    self.end_headers()
                    return
                self.send_response(200)
                self.end_headers()
                self.wfile.write(owner.responses.get(self.path, owner.content))

            def log_message(self, *args):
                pass

        self.server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
        context.load_cert_chain(cert, key)
        self.server.socket = context.wrap_socket(self.server.socket, server_side=True)
        # The default 0.5 s shutdown polling dominates these short fixtures.
        self.thread = threading.Thread(target=self.server.serve_forever, kwargs={"poll_interval": 0.02})
        self.thread.start()
        self.addCleanup(self.stop_server)
        self.cache = root / "cache"
        self.environment = patch.dict(os.environ, NELISP_CACHE=str(self.cache), SSL_CERT_FILE=str(cert),
                                      NO_PROXY="127.0.0.1", no_proxy="127.0.0.1")
        self.environment.start()
        self.addCleanup(self.environment.stop)
        self.locked = {"name": "example", "version": "1.0.0", "dependencies": {},
                       "sha256": hashlib.sha256(self.content).hexdigest(),
                       "url": f"https://127.0.0.1:{self.server.server_port}/source"}

    def stop_server(self):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join(timeout=5)
        self.assertFalse(self.thread.is_alive())

    def test_https_fetch_then_offline_and_corrupt_cache(self):
        self.assertEqual(source_bytes(self.locked, offline=False), self.content)
        self.assertEqual(source_bytes(self.locked), self.content)
        self.assertEqual(self.requests, ["/source"])
        path = self.cache / (self.locked["sha256"] + ".nl")
        self.assertFalse(path.stat().st_mode & 0o111)
        path.write_bytes(b"corruption")
        with self.assertRaisesRegex(ValueError, "integrity"):
            source_bytes(self.locked, offline=False)
        self.assertEqual(self.requests, ["/source"])

    def test_no_publication_on_bad_hash_redirect_or_size(self):
        for locked in [dict(self.locked, sha256="0" * 64),
                       dict(self.locked, url=self.locked["url"].replace("source", "redirect"))]:
            with self.assertRaises(ValueError):
                source_bytes(locked, offline=False)
        with patch("nelisp_package_store.MAX_ARTIFACT_BYTES", 4), self.assertRaisesRegex(ValueError, "exceeds"):
            source_bytes(self.locked, offline=False)
        self.assertFalse(self.cache.exists())

    def test_offline_miss_never_requests_network(self):
        with self.assertRaisesRegex(ValueError, "missing cached"):
            source_bytes(self.locked)
        self.assertEqual(self.requests, [])

    def test_untrusted_tls_certificate_is_rejected(self):
        with patch.dict(os.environ, SSL_CERT_FILE=str(self.cache / "absent-ca.pem")):
            with self.assertRaises(OSError):
                source_bytes(self.locked, offline=False)
        self.assertFalse(self.cache.exists())


if __name__ == "__main__":
    unittest.main()
