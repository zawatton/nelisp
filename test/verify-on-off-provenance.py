"""Verify a reviewed compileless packet; this does not execute native code.

The expected evidence digest must come from the independent reviewer. This
checks recorded provenance and counters, not arithmetic guard qualification.
"""
import argparse
import hashlib
import json
from pathlib import Path


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def verify(packet, repo, expected):
    evidence_path = packet / "positive-evidence.json"
    assert digest(evidence_path) == expected, "untrusted evidence digest"
    evidence = json.loads(evidence_path.read_text())
    for name, sha in evidence["files"].items():
        path = (packet / name).resolve()
        assert path.is_relative_to(packet.resolve()), "packet path escape"
        assert digest(path) == sha, name
    review = json.loads((packet / "review.json").read_text())
    assert digest(packet / "review.json") == evidence["review_sha256"]
    for name, sha in review["files"].items():
        path = (packet / name).resolve()
        assert path.is_relative_to(packet.resolve()), "review path escape"
        assert digest(path) == sha, name
    for name, sha in review["artifacts"].items():
        path = (repo / name).resolve()
        assert path.is_relative_to(repo.resolve()), "artifact path escape"
        assert digest(path) == sha, name
    assert digest(packet / "source.sha256") == review["source_manifest_sha256"]
    result = evidence["result"]
    status = json.loads((packet / "full.status.json").read_text())
    assert status == result, "status mismatch"
    assert result["launched"] and result["exit_code"] == 0 and result["guard"] is None
    assert result["elapsed_seconds"] <= result["wall_limit_seconds"] <= 240
    assert result["global_native_limit"] == 2 and result["rss_limit_kib"] <= 2097152
    assert result["peak_rss_kib"] <= result["rss_limit_kib"]
    assert result["minimum_available_kib"] >= 16777216
    assert result["mem_available_kib"] >= result["minimum_available_kib"]
    assert evidence["native_entries"] == evidence["frame_cleanups"] == 20
    assert evidence["GNU_parity_rows"] == 10
    assert "PRESERVED-ON-OFF-PASS calls=20 cleanups=20 no-compile" in (packet / "full.stdout").read_text()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("packet", type=Path)
    parser.add_argument("repo", type=Path)
    parser.add_argument("expected_evidence_sha256")
    args = parser.parse_args()
    verify(args.packet, args.repo, args.expected_evidence_sha256)
    print("PROVENANCE-PASS carried-phi-only; guard-native-pending")
