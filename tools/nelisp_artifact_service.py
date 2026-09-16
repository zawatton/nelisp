"""Doc 203 Section 5 artifact-service seam, in front of the frozen store.

Doc 203 (docs/design/203-nelix-package-foundation.org, `#+STATUS: PROPOSED`)
Section 8 step 1 is to freeze today's resolver/store/project contracts and
"define the versioned batch protocol, data reader and error cases" before any
acquisition code moves behind Nelix. This module is that seam: it names the
Section 5 project-frontend-facing operations, `ensure_artifacts(plan, policy)`
and `read_artifact(identity)`, but every byte still passes through
`nelisp_package_store.source_bytes`/`cache_directory`, unchanged. Those two
functions keep their existing signatures, exceptions, messages and on-disk
cache layout -- `test/nelisp-package-store-test.py` continues to pin that
contract directly, unmodified. Nothing here replaces them or touches the
existing project-frontend call sites in `nelisp-project.py`/`nelisp_registry.py`;
this only gives their behavior a name that matches the Section 5 vocabulary,
so a later backend swap (Section 8 step 2+) changes what is behind this file
rather than every caller. Connecting `nelisp fetch`/`run`/`test`/`build` to
this seam is Section 8 step 3, not this step.

`plan` is today's list of locked package records (the shape `nelisp.lock`
already produces: name/version/dependencies/sha256/url). `policy` is a
mapping with one optional key, `offline` (default `True`, matching
`source_bytes`'s own default) -- the only acquisition-policy field this
backend has. `identity` for `read_artifact` is, for this step, one such
locked record: today's single backend has no separate digest-to-URL registry
outside that record, so a bare algorithm+digest identity (Doc 203 Section 4's
"blob identity") cannot yet be resolved to bytes on its own. Narrowing
`identity` to plain digest form is later work, tracked in the report, not
done here.

Error identities
----------------
Every failure `ensure_artifacts`/`read_artifact` can raise through this seam
is one `ArtifactError`, a `ValueError` subclass carrying a stable `.identity`
string. `_classify` maps today's `nelisp_package_store`/`nelisp_packages`
messages onto that identity; `test/nelisp-artifact-service-test.py` pins both
directions so a later backend cannot change what a failure means silently.

| identity           | current trigger (nelisp_package_store.source_bytes)          |
|---------------------|----------------------------------------------------------------|
| ``"missing"``        | the object is absent from the local cache and policy forbids fetching it. Today that is exactly `offline=True` with no cache entry -- Doc 203 Section 5's "offline means no network attempt", and the only shape a missing object takes in this single-backend step. |
| ``"digest-mismatch"`` | verified bytes (cached or freshly fetched) disagree with the locked SHA-256, via `nelisp_packages.verify_artifact`. Covers both a corrupted cache entry and a tampered/incorrect locked digest -- both raise the same message today. |
| ``"oversized"``       | a transfer -- cache read or HTTPS response -- exceeded the 16 MiB artifact cap. The cap is enforced with a bounded read (`MAX_ARTIFACT_BYTES + 1` bytes requested, never less), not after unbounded buffering. |
| ``"redirect"``        | the HTTPS transport was asked to follow a redirect. |
| ``"invalid-content"`` | verified bytes are not valid UTF-8, or contain a NUL byte. |

A transport-level failure below this classification -- TLS/certificate
errors, DNS, connection refused, timeouts -- is not reclassified here: it
propagates as whatever `urllib`/`ssl` raises today, unchanged, because
reclassifying it would itself be the "observable CLI behaviour change" this
step must not make.
"""
import re

from nelisp_package_store import source_bytes

SCHEMA_VERSION = 1

_PATTERNS = (
    ("missing", re.compile(r"^missing cached package ")),
    ("digest-mismatch", re.compile(r"^artifact integrity mismatch: ")),
    ("oversized", re.compile(r"^package exceeds \d+ bytes: ")),
    ("redirect", re.compile(r"^package artifact redirects are not supported$")),
    ("invalid-content", re.compile(r"^package source contains NUL: ")),
)


class ArtifactError(ValueError):
    """A stable, documented failure identity for an artifact-service call.

`identity` is one of "missing", "digest-mismatch", "oversized", "redirect",
or "invalid-content" -- see the module docstring's table. The original
message is preserved unchanged as the exception's own text, so an existing
caller that only catches bare `ValueError` sees no behavior change.
"""

    def __init__(self, identity, message):
        super().__init__(message)
        self.identity = identity


def _classify(error):
    if isinstance(error, UnicodeDecodeError):
        return "invalid-content"
    message = str(error)
    for identity, pattern in _PATTERNS:
        if pattern.search(message):
            return identity
    return None


def _read(locked, offline):
    try:
        return source_bytes(locked, offline=offline)
    except ValueError as error:
        identity = _classify(error)
        if identity is None:
            # No known identity claims this message: leave it as the plain
            # ValueError source_bytes already raises, rather than invent a
            # classification this step has not pinned with a test.
            raise
        raise ArtifactError(identity, str(error)) from error


def _offline_from_policy(policy):
    if policy is None:
        return True
    if not isinstance(policy, dict) or set(policy) - {"offline"}:
        raise ValueError("artifact-service policy accepts only 'offline'")
    offline = policy.get("offline", True)
    if type(offline) is not bool:
        raise ValueError("artifact-service policy 'offline' must be boolean")
    return offline


def ensure_artifacts(plan, policy=None):
    """Materialize every locked artifact in `plan`, verified, as data receipts.

No resolution, profile activation, build execution, metadata loading, macro
expansion, hooks, or archive extraction happens here or in the backend this
calls -- each entry is read exactly the way `nelisp-project.py` already reads
one today, through the unchanged `source_bytes`. `policy["offline"]`
(default `True`) is the sole acquisition policy field; `offline=False` allows
one HTTPS fetch per missing entry, with the same redirect/size/TLS rules
`source_bytes` already enforces.

Returns one receipt per plan entry, in `plan` order: a dict with
`schema_version`, `name`, `version`, `sha256` and `size`. Receipts carry no
local filesystem path -- Doc 203 Section 5 requires that a machine-local
cache path never enter a portable lock -- and no other derived state.
"""
    offline = _offline_from_policy(policy)
    receipts = []
    for locked in plan:
        content = _read(locked, offline)
        receipts.append({
            "schema_version": SCHEMA_VERSION,
            "name": locked["name"],
            "version": locked["version"],
            "sha256": locked["sha256"],
            "size": len(content),
        })
    return receipts


def read_artifact(identity, *, offline=True):
    """Return verified bytes for one locked artifact identity.

`identity` is today's locked-package record (name, version, dependencies,
sha256, url) -- see the module docstring for why this step cannot yet narrow
it to a bare digest. Missing or corrupt objects raise `ArtifactError`; see
the module docstring's identity table.
"""
    return _read(identity, offline)
