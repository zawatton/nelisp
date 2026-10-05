# Independent design and completion audit

Auditor: `/root/backend_audit`, explicitly selected `gpt-6-astra`, reasoning effort `ultra`.
Coordinator: gpt-6.1-sol. Audit operated read-only, with no network/builds/git writes.
Authority: GUI daily ledger S3.1 requires an independent audit; user-wide model policy requires Astra/maximum reasoning.

Status: approved for G1 / S3.1 design completion, 2026-10-05.
Exact DESIGN.md SHA-256: `87a58b228baf4d9b1e08a2e0c29b4910b5856a3f8f6e8c63b361de1942903c92` (177 lines).
Auditor's final disposition: “No substantive findings remain.” Configuration and hash also recorded in audit.json.

The audit compared A–D, inspected nl-ffi/ELF and XCB/libffi/XKB headers, and reviewed design claims against local probe results.
Findings incorporated before completion:

- Six-argument direct-call limit hides seven-argument XKB update and eight-argument setup; demonstrated shared libffi bridge resolves direction.
- Real XCB cookie aggregates must use ffi_type descriptors; dead-connection tests do not prove live cookie/error handling.
- Generic XCB requests require two reserved leading iovecs; XCB owns its socket and flush can block.
- Pango native rendering does not require Lisp callbacks, but surviving font workers expose shared runtime process-exit failure.
- Actual binary entry epilogue uses SYS_exit60; normal timeout now shows a zombie leader and sleeping worker. Diagnostic exit_group succeeds.
- Keep production EOF/exit/kill-emacs, GC/native lifetime, live XCB/XKB and real GNOME selection/scaling acceptance open.
- Narrow historical metadata claims, distinguish saved-image checking from render replay, and change real renderer/resource DPI in metrics gate.
- Give checker content-negative cases matching document hashes, with separate stale-hash rejection.

Final review approved the B recommendation, A–D comparison, ownership boundaries, evidence qualifications and S3.1–S5.3 plan.
Approval covers design only; live interoperability, production quit, resource lifetime, performance and GNOME integration remain implementation gates.
