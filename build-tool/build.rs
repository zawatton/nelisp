//! Doc 84 §84.2 (2026-05-10) — Syscall name → number table codegen.
//!
//! Emits `lisp/nelisp-syscall-table.el` containing the
//! `nelisp--syscall-nr-table' alist + `nelisp--syscall-nr-resolve'
//! resolver consumed by `lisp/nelisp-jit-strategy.el' (= the
//! `nelisp--syscall' wrapper's `nr-int' lookup).  Replaces the
//! pre-Doc-84 Rust `bi_syscall_nr_resolve' primitive in
//! `build-tool/src/jit/strategy.rs' (= -13 Rust LOC in `src/').
//!
//! The 1:1 source-of-truth for the syscall name → number map is the
//! `syscall_nr` helper in `build-tool/src/eval/builtins.rs' which the
//! `bi_syscall' primitive still consumes for the bare `(nelisp--
//! syscall NR &rest ARGS)' fast path.  This codegen mirrors that map
//! into elisp so the wrapper's symbol → number step is pure elisp.
//!
//! Cross-platform: on non-Linux targets the generated file emits a
//! `nelisp--syscall-nr-resolve' that signals `arith-error' (= same
//! semantics as the pre-Doc-84 `cfg(not(target_os = "linux"))'
//! variant).  Per Doc 62 Phase 5 §5.8, non-Linux nelisp routes all
//! syscall callers through `nl-ffi-call' libc instead, so the
//! resolver is a dead path there but still callable.

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=CARGO_CFG_TARGET_OS");
    println!("cargo:rerun-if-env-changed=CARGO_CFG_TARGET_ARCH");

    let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    let target_arch = std::env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .expect("CARGO_MANIFEST_DIR must be set by cargo");
    let out_path = std::path::Path::new(&manifest_dir)
        .join("..")
        .join("lisp")
        .join("nelisp-syscall-table.el");

    let body = if target_os == "linux" {
        emit_linux_table()
    } else {
        emit_unsupported_stub(&target_os)
    };

    // Only write if content differs (= avoid touching mtime + spurious
    // image rebakes on no-op rebuilds).
    let existing = std::fs::read_to_string(&out_path).unwrap_or_default();
    if existing != body {
        std::fs::write(&out_path, &body)
            .unwrap_or_else(|e| panic!("write {} failed: {}", out_path.display(), e));
    }

    // Doc 99 §99.B spike — compile `lisp/nelisp-cc-spike-noop.el' to a
    // C-callable ELF / Mach-O object via `scripts/compile-elisp-objects.el',
    // wrap the result in a static archive, and link it into the final
    // `nelisp' binary.  §100.D Stage 2/3 extends the matrix to:
    //   linux + x86_64    (ELF + SysV AMD64, §100.C/D Stage 1)
    //   linux + aarch64   (ELF + AAPCS,      §100.D Stage 2)
    //   macos + aarch64   (Mach-O + AAPCS,   §100.D Stage 3)
    // Other targets fall through to the Rust trampoline path until
    // their object-format / asm support lands.
    let supported = (target_os == "linux"
        && (target_arch == "x86_64" || target_arch == "aarch64"))
        || (target_os == "macos" && target_arch == "aarch64");
    if supported {
        link_elisp_cc_spike(&manifest_dir, &target_os, &target_arch);
    }
}

fn link_elisp_cc_spike(manifest_dir: &str, target_os: &str, target_arch: &str) {
    let repo_root = std::path::Path::new(manifest_dir).join("..");
    let script = repo_root.join("scripts").join("compile-elisp-objects.el");
    let compiler_src = repo_root
        .join("lisp")
        .join("nelisp-phase47-compiler.el");
    let layout_src = repo_root
        .join("lisp")
        .join("nelisp-sexp-layout.el");

    // Re-run when any elisp source the manifest can consume changes.
    // Keep this list in sync with `compile-elisp-objects-manifest' in
    // `scripts/compile-elisp-objects.el'.
    let manifest_sources = [
        "nelisp-cc-spike-noop.el",
        "nelisp-cc-fact-i64.el",
        // Doc 100 §100.C — `bi_truncate' Int swap.
        "nelisp-cc-truncate-int.el",
        // Doc 101 §101.B — `length' Cons/Nil swap.
        "nelisp-cc-length-cons.el",
        // Doc 101 §101.C — `bi_eq' Symbol swap.
        "nelisp-cc-eq-symbol.el",
        // Doc 101 §101.D — `cons' constructor swap.
        "nelisp-cc-cons-construct.el",
        // Doc 117 §117.A.2 — `bi_string_bytes' byte-length swap.
        "nelisp-cc-bi-string-bytes.el",
        // Doc 111 §111.B — `recordp' predicate swap.
        "nelisp-cc-recordp.el",
        // Doc 111 §111.C — `aref' Vector arm swap.
        "nelisp-cc-aref-vector.el",
        // Doc 117 §117.A.1 — `make-vector' allocate + fill swap.
        "nelisp-cc-bi-make-vector.el",
        // Doc 111 §111.D — Cell read+write op probes (4 entries,
        // shared source file, one .o per op).
        "nelisp-cc-cell-ops.el",
        // Doc 111 §111.E #1 — `mirror_lookup_entry' Phase 47 rewrite
        // (= Group A read-path foundation, composed by 5 sibling
        // helpers).  Doc 115 §115.7 — calls the pure-elisp
        // `nelisp_fnv1a' helper for the bucket-index hash (previously
        // routed through the now-deleted `nl_mirror_fnv1a_sexp' Rust
        // extern wrapping `env_helpers::mirror_fnv1a').
        "nelisp-cc-mirror-lookup-entry.el",
        // Doc 111 §111.E #2-6 — Group A compose-on-#1 read helpers
        // (lookup_value / lookup_function / is_bound / is_fbound /
        // is_constant).  Each calls `nelisp_mirror_lookup_entry' via
        // `extern-call' and adds a 1-2 op tail to read the requested
        // symbol-entry slot.
        "nelisp-cc-mirror-lookup-value.el",
        "nelisp-cc-mirror-lookup-function.el",
        "nelisp-cc-mirror-is-bound.el",
        "nelisp-cc-mirror-is-fbound.el",
        "nelisp-cc-mirror-is-constant.el",
        // Doc 111 §111.E #7-#12 — `mirror_set_value' / `mirror_set_function'
        // / `mirror_clear_value' / `mirror_clear_function' /
        // `mirror_set_constant' / `mirror_install_entry' Phase 47 helpers
        // (= Group B write path).  All six compose on `mirror_lookup_entry'
        // (= #1) via `extern-call' and use the §111.B `record-slot-set'
        // ABI to overwrite the matched entry's slots refcount-safely.
        // The miss / auto-vivify branch stays in Rust under the
        // dispatcher for now (returns 0 from these helpers).
        "nelisp-cc-mirror-set-value.el",
        "nelisp-cc-mirror-set-function.el",
        "nelisp-cc-mirror-clear-value.el",
        "nelisp-cc-mirror-clear-function.el",
        "nelisp-cc-mirror-set-constant.el",
        "nelisp-cc-mirror-install-entry.el",
        // Doc 111 §111.E #19-#26 Group E — env_lexframe.rs Phase 47
        // rewrites.  Seven entries; each wraps an `nl_frame_*' Rust
        // shim in `env_lexframe_phase47_shims.rs'.
        "nelisp-cc-frame-stack-view.el",
        "nelisp-cc-frame-ensure-capacity.el",
        "nelisp-cc-frame-push.el",
        "nelisp-cc-frame-pop.el",
        "nelisp-cc-frame-bind.el",
        "nelisp-cc-frame-stack-find.el",
        "nelisp-cc-wrap-alist-cells.el",
        // Doc 115 §115.7 — pure-elisp 32-bit FNV-1a hash.  Replaces
        // the Rust `mirror_fnv1a' + `nl_mirror_fnv1a_sexp' extern.
        "nelisp-cc-fnv1a.el",
        // Doc 100 §100.D Stage 1 — 12-trampoline `jit/arith.rs' swap.
        "nelisp-cc-jit-arith.el",
        // Doc 110 §110.E.2.a — 4-trampoline `jit/float.rs' partial swap.
        "nelisp-cc-jit-float.el",
        // Doc 110 §110.F — 3-trampoline `jit/math.rs' swap (float / exp / log).
        "nelisp-cc-jit-math.el",
        // Doc 120 §120.A — 2 of 4 `jit/predicate.rs' trampoline swaps
        // (`predicate_eq' + `ref_eq'; `sxhash' + `type_of' stay Rust).
        "nelisp-cc-jit-predicate-eq.el",
        "nelisp-cc-jit-ref-eq.el",
        // Doc 120 §120.B — 4 of 11 `jit/box_accessor.rs' record-family
        // trampoline swaps (`record_type' / `record_len' / `record_ref'
        // / `record_set'; `record_alloc' stays Rust + 6 non-record
        // entries SKIP per blocker notes in `jit/box_accessor.rs').
        "nelisp-cc-jit-record.el",
        // Doc 120 §120.D — 4 of 4 `jit/access.rs' trampoline swaps
        // (`length' / `aref' / `aset' / `elt').  Str length + BoolVector
        // aref/aset sub-arms reach narrow Rust externs via `extern-call'
        // (= same shape `nl_sexp_eq' uses for the §120.A predicate-eq
        // slow path).
        "nelisp-cc-jit-length.el",
        "nelisp-cc-jit-aref.el",
        "nelisp-cc-jit-aset.el",
        "nelisp-cc-jit-elt.el",
        // Doc 122 §122.A — `sexp-write-str' / `sexp-write-symbol' grammar
        // ops (= 2 entries, shared source file).
        "nelisp-cc-sexp-write-str.el",
        // Doc 122 §122.G — `sexp-write-float' grammar op (= Reader Float
        // unlock).  Single entry; same Linux-x86_64 gate as §122.A.
        "nelisp-cc-sexp-write-float.el",
        // Doc 122 §122.B — Mutable string builder grammar ops (= 5
        // entries, shared source file).
        "nelisp-cc-mut-str.el",
        // Doc 122 §122.E — Atomic + raw memory primitive grammar ops
        // (= 6 entries, shared source file).  Substrate gate for
        // Doc 123-128 refcount / nl*.rs lifecycle elisp化.
        "nelisp-cc-atomic-raw-mem.el",
        // Doc 125 §125.A — alloc / dealloc primitive grammar ops
        // (= 2 entries, shared source file).  Substrate gate for
        // Doc 124.G-K NlBox Drop kernels + Doc 126-128 GC arena.
        "nelisp-cc-alloc-dealloc.el",
        // Doc 123 §123.A — first substrate elisp化 stage.  Single
        // entry: `nelisp_rc_inc' = the refcount-inc kernel pulled
        // out of `build-tool/src/eval/rc_primitives.rs' using the
        // §122.E `atomic-fetch-add' op.  Proof of concept that the
        // substrate gate is functional; §123.B-E sweep the remaining
        // rc primitive bodies onto the same pattern.
        "nelisp-cc-rc-inc.el",
        // Doc 123 §123.B — second substrate elisp化 stage.  Single
        // entry: `nelisp_rc_dec' = the refcount-dec twin of §123.A,
        // pulled out of `build-tool/src/eval/rc_primitives.rs'
        // (= `rc_dec_no_drop' + `bi_nl_rc_dec_strong' mutation half)
        // using the §122.E `atomic-fetch-add' op with delta = -1
        // (= fetch-sub semantics).  Dispatch swap lands in §123.F.
        "nelisp-cc-rc-dec.el",
        // Doc 123 §123.C — refcount-reader twins.  Two source files:
        // `nelisp_rc_strong_count' (= `ptr-read-u64' at offset 64 of
        // an `NlConsBox') and `nelisp_rc_kind' (= `ptr-read-u8' at
        // offset 0 of the outer `Sexp' enum's `#[repr(C, u8)]'
        // discriminant).
        "nelisp-cc-rc-strong-count.el",
        "nelisp-cc-rc-kind.el",
        // Doc 123 §123.D — the last MEDIUM stage of Doc 123's
        // substrate elisp化 chain.  Two source files:
        // `nelisp_rc_payload_ptr' (= `ptr-read-u64' at offset 8 of
        // the outer `Sexp' = `SEXP_PAYLOAD_OFFSET'; mirrors the
        // `bi_nl_rc_payload_ptr' Cons arm body in `rc_primitives.rs:
        // 230-244') and `nelisp_gc_walk_children' (= two `cons-make'
        // allocations driven by `ptr-read-u64' for the box-ptr
        // extraction; mirrors `bi_nl_gc_walk_children' Cons arm
        // body via `Sexp::list_from(&[car, cdr])').  Dispatch swap
        // lands in a future §123.F-like sweep stage that also handles
        // non-Cons tag-dispatch fallback.
        "nelisp-cc-rc-payload-ptr.el",
        "nelisp-cc-gc-walk-children.el",
        // Doc 124 §124.A — first stage of the `nl*.rs::Clone/Drop'
        // substrate elisp化 chain.  Ships the NlConsBox Clone kernel
        // (= `rc_inc' + return-the-pointer) as PoC; §124.B-E sweep the
        // remaining 4 NlBox types onto the same pattern.  Reuses Doc
        // 123 §123.A's `atomic-fetch-add' at REFCOUNT_OFFSET = 64.
        // Drop half (§124.G-K) gated on Doc 125 alloc/dealloc grammar.
        "nelisp-cc-nlconsbox-clone.el",
        // Doc 124 §124.G — first Drop-half stage.
        "nelisp-cc-nlconsbox-drop.el",
        // Doc 124 §124.H — NlVector Drop kernel (REFCOUNT_OFFSET=24,
        // SIZE=32, ALIGN=8).  Mechanical port of §124.G to the
        // `Vec<Sexp>' header + AtomicUsize trailer layout.
        "nelisp-cc-nlvector-drop.el",
        // Doc 124 §124.I/J/K — sibling Drop kernels.  Same shape as
        // §124.G/H modulo per-type SIZE / REFCOUNT_OFFSET literals:
        //   §124.I NlCell:   REFCOUNT_OFFSET = 32, SIZE = 40, ALIGN = 8
        //   §124.J NlRecord: REFCOUNT_OFFSET = 56, SIZE = 64, ALIGN = 8
        //   §124.K NlStr:    REFCOUNT_OFFSET = 24, SIZE = 32, ALIGN = 8
        "nelisp-cc-nlcell-drop.el",
        "nelisp-cc-nlrecord-drop.el",
        "nelisp-cc-nlstr-drop.el",
        // Doc 124 §124.B-E — mechanical sibling Clone kernels for the
        // remaining 4 nl*.rs box types (NlVector / NlCell / NlRecord /
        // NlStr).  REFCOUNT_OFFSET = 24 / 32 / 56 / 24 respectively.
        "nelisp-cc-nlvector-clone.el",
        "nelisp-cc-nlcell-clone.el",
        "nelisp-cc-nlrecord-clone.el",
        "nelisp-cc-nlstr-clone.el",
        // Doc 116 §116.A — pure-elisp Reader lexer.  Single source
        // file defining `nelisp_reader_lex_one' + ~20 tail-call
        // helpers; replaces the eventual deletion of
        // `build-tool/src/reader/lexer.rs' (= -885 LOC Rust) once
        // §116.B parser + §116.C top-level wrapper SHIP.
        "nelisp-cc-reader-lexer.el",
        // Doc 122 §122.C — Extended extern-call f64 probes (= 3
        // entries wrapping libm sqrt / sin / cos for the
        // `tests/elisp_cc_extern_call_f64_probe.rs' round-trip).
        "nelisp-cc-extern-call-f64.el",
        // Doc 116 §116.B — pure-elisp Reader parser.  Single source
        // file defining `nelisp_reader_parse_one' + ~25 tail-call
        // helpers; consumes §116.A's tokens via `extern-call' and
        // builds Sexp values via the §101/§111/§122 grammar
        // primitives.  Top-level wire-in (`read_str' / `read_all'
        // public entry) is §116.C; Rust parser.rs deletion is §116.D.
        "nelisp-cc-reader-parser.el",
    ];

    println!("cargo:rerun-if-changed={}", script.display());
    println!("cargo:rerun-if-changed={}", compiler_src.display());
    println!("cargo:rerun-if-changed={}", layout_src.display());
    for src in &manifest_sources {
        println!(
            "cargo:rerun-if-changed={}",
            repo_root.join("lisp").join(src).display()
        );
    }

    // Emacs is the build tool here — gated so users without Emacs see
    // a friendly skip rather than a cryptic exec failure.  The spike
    // is single-entry so a skip just disables the §99.B probe test.
    let emacs = match which_or_skip("emacs") {
        Some(p) => p,
        None => {
            println!("cargo:warning=skipping §99.B elisp-object link: emacs not on PATH");
            return;
        }
    };

    let out_dir = std::env::var("OUT_DIR")
        .expect("OUT_DIR must be set by cargo");
    let elisp_obj_dir = std::path::Path::new(&out_dir).join("elisp-objects");
    std::fs::create_dir_all(&elisp_obj_dir)
        .unwrap_or_else(|e| panic!("create_dir_all {}: {}", elisp_obj_dir.display(), e));

    let status = std::process::Command::new(&emacs)
        .arg("--batch")
        .arg("-Q")
        .arg("-L")
        .arg(repo_root.join("lisp"))
        .arg("-l")
        .arg(&script)
        .arg("-f")
        .arg("compile-elisp-objects-emit-all")
        .env("NELISP_ELISP_OBJECTS_DIR", &elisp_obj_dir)
        .env("NELISP_PHASE47_TARGET_ARCH", target_arch)
        .env("NELISP_PHASE47_TARGET_OS", target_os)
        .status()
        .unwrap_or_else(|e| panic!("emacs --batch failed to spawn: {}", e));
    if !status.success() {
        panic!(
            "compile-elisp-objects-emit-all exited with {} (script={})",
            status,
            script.display()
        );
    }

    // Collect every `.o' the orchestrator wrote into the dir and
    // bundle them into a single static archive via `ar rcs'.  Cargo
    // takes a single `-lstatic=...' that covers the whole manifest;
    // adding a new entry to `compile-elisp-objects-manifest' costs
    // zero extra build.rs lines.  Order is sorted for determinism
    // across filesystem traversal quirks.
    let mut obj_paths: Vec<std::path::PathBuf> = std::fs::read_dir(&elisp_obj_dir)
        .unwrap_or_else(|e| panic!("read_dir {}: {}", elisp_obj_dir.display(), e))
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("o"))
        .collect();
    if obj_paths.is_empty() {
        panic!(
            "no .o files in {} after orchestrator run (manifest empty? silent failure?)",
            elisp_obj_dir.display()
        );
    }
    obj_paths.sort();
    let ar = which_or_skip("ar").unwrap_or_else(|| "ar".to_string());
    let archive = std::path::Path::new(&out_dir).join("libnelisp_elisp_spike.a");
    // `ar rcs' replaces / creates the archive in one step; remove any
    // stale .a first so we don't accumulate orphan members across rebuilds.
    let _ = std::fs::remove_file(&archive);
    let mut cmd = std::process::Command::new(&ar);
    cmd.arg("rcs").arg(&archive);
    for p in &obj_paths {
        cmd.arg(p);
    }
    let status = cmd
        .status()
        .unwrap_or_else(|e| panic!("ar failed to spawn: {}", e));
    if !status.success() {
        panic!("ar rcs {} exited with {}", archive.display(), status);
    }

    println!("cargo:rustc-link-search=native={}", out_dir);
    println!("cargo:rustc-link-lib=static=nelisp_elisp_spike");
}

fn which_or_skip(prog: &str) -> Option<String> {
    let path = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path) {
        let cand = dir.join(prog);
        if cand.is_file() {
            return Some(cand.to_string_lossy().into_owned());
        }
    }
    None
}

fn emit_linux_table() -> String {
    // 1:1 mirror of `syscall_nr' in `src/eval/builtins.rs'.  When that
    // map gains entries, add them here too (= sole syscall-nr source
    // of truth pair).  Each entry is `(NAME . NUMBER)' where NAME is
    // a symbol string and NUMBER is the host `libc::SYS_*' i64.
    let entries: &[(&str, i64)] = &[
        ("read",              libc::SYS_read              as i64),
        ("write",             libc::SYS_write             as i64),
        ("close",             libc::SYS_close             as i64),
        ("openat",            libc::SYS_openat            as i64),
        ("exit_group",        libc::SYS_exit_group        as i64),
        ("lseek",             libc::SYS_lseek             as i64),
        ("dup2",              libc::SYS_dup2              as i64),
        ("getpid",            libc::SYS_getpid            as i64),
        ("kill",              libc::SYS_kill              as i64),
        // Doc 54 Phase 3 — Core-12 additions.
        ("mmap",              libc::SYS_mmap              as i64),
        ("mprotect",          libc::SYS_mprotect          as i64),
        ("munmap",            libc::SYS_munmap            as i64),
        ("fcntl",             libc::SYS_fcntl             as i64),
        // Doc 55 Phase 4 — Posix-30 int-only additions.
        ("fork",              libc::SYS_fork              as i64),
        ("socket",            libc::SYS_socket            as i64),
        ("listen",            libc::SYS_listen            as i64),
        ("wait4",             libc::SYS_wait4             as i64),
        ("getppid",           libc::SYS_getppid           as i64),
        ("setpgid",           libc::SYS_setpgid           as i64),
        // Doc 57 Phase 4.3 — modern Linux event surface.
        ("pidfd_open",        libc::SYS_pidfd_open        as i64),
        ("pidfd_send_signal", libc::SYS_pidfd_send_signal as i64),
        ("inotify_init1",     libc::SYS_inotify_init1     as i64),
        ("inotify_rm_watch",  libc::SYS_inotify_rm_watch  as i64),
        ("eventfd2",          libc::SYS_eventfd2          as i64),
        // Doc 59 Phase 4.2 + 4.3.1 — timerfd_create int-only.
        ("timerfd_create",    libc::SYS_timerfd_create    as i64),
    ];

    let mut s = String::new();
    s.push_str(";;; nelisp-syscall-table.el --- Doc 84 §84.2 syscall nr table  -*- lexical-binding: t; -*-\n");
    s.push_str("\n;;; Commentary:\n\n");
    s.push_str(";; AUTO-GENERATED by `build-tool/build.rs' from `libc::SYS_*'\n");
    s.push_str(";; constants on the host target.  DO NOT EDIT BY HAND — edits\n");
    s.push_str(";; will be clobbered on the next `cargo build'.  When the Rust\n");
    s.push_str(";; `syscall_nr' helper in `build-tool/src/eval/builtins.rs'\n");
    s.push_str(";; gains a new arm, add a matching entry to `emit_linux_table'\n");
    s.push_str(";; in `build-tool/build.rs'.\n");
    s.push_str(";;\n");
    s.push_str(";; Doc 84 §84.2 (2026-05-10) replaces the Rust `bi_syscall_nr_\n");
    s.push_str(";; resolve' primitive (= `jit/strategy.rs') with this elisp\n");
    s.push_str(";; lookup; consumed by `nelisp-jit-strategy.el' for the\n");
    s.push_str(";; `nelisp--syscall' wrapper's symbol → i64 step.\n\n");
    s.push_str(";;; Code:\n\n");
    // Use bare `setq' instead of `defconst' because this file loads
    // BEFORE `nelisp-stdlib-eval-special.el' which provides the
    // `defconst' macro.  `setq' is a Tier 1 special form available
    // from the very first `eval' call.  The resulting binding is
    // mutable from elisp's perspective but conceptually a constant.
    s.push_str("(setq nelisp--syscall-nr-table\n      '(\n");
    for (name, nr) in entries {
        s.push_str(&format!("        ({} . {})\n", name, nr));
    }
    s.push_str("        ))\n\n");
    s.push_str("(fset 'nelisp--syscall-nr-resolve\n");
    s.push_str("      (lambda (name)\n");
    // Body uses only Tier 1 forms (`let' / `if') and Rust builtins
    // (`assq' / `cdr' / `signal' / `cons').  `assq' is itself defined
    // in `nelisp-stdlib-search.el' (loaded after this file), but the
    // resolver is only INVOKED at user-syscall time which is well
    // after stdlib finishes.  At LOAD time we only `fset' the lambda
    // — its body isn't evaluated.
    s.push_str("        (let ((cell (assq name nelisp--syscall-nr-table)))\n");
    s.push_str("          (if cell\n");
    s.push_str("              (cdr cell)\n");
    s.push_str("            (signal 'arith-error\n");
    s.push_str("                    (cons \"nelisp--syscall-nr-resolve: unknown syscall\"\n");
    s.push_str("                          (cons name nil)))))))\n\n");
    s.push_str(";; (provide 'nelisp-syscall-table) — omitted: this file is\n");
    s.push_str(";; loaded during bootstrap before `provide' is installed.\n");
    s.push_str(";;; nelisp-syscall-table.el ends here\n");
    s
}

fn emit_unsupported_stub(target_os: &str) -> String {
    let mut s = String::new();
    s.push_str(";;; nelisp-syscall-table.el --- Doc 84 §84.2 syscall nr table (non-Linux stub)  -*- lexical-binding: t; -*-\n");
    s.push_str("\n;;; Commentary:\n\n");
    s.push_str(&format!(";; AUTO-GENERATED by `build-tool/build.rs' for target_os = {}.\n", target_os));
    s.push_str(";; Per Doc 62 Phase 5 §5.8, non-Linux nelisp routes syscall\n");
    s.push_str(";; callers through `nl-ffi-call' libc bindings; this resolver\n");
    s.push_str(";; is a dead path and signals `arith-error' if invoked.\n\n");
    s.push_str(";;; Code:\n\n");
    // See Linux variant for the `setq'-instead-of-`defconst' rationale.
    s.push_str("(setq nelisp--syscall-nr-table nil)\n\n");
    s.push_str("(fset 'nelisp--syscall-nr-resolve\n");
    s.push_str("      (lambda (name)\n");
    s.push_str("        (let ((_ name))\n");
    s.push_str("          (signal 'arith-error\n");
    s.push_str("                  (cons \"nelisp--syscall-nr-resolve: unsupported platform\" nil)))))\n\n");
    s.push_str(";;; nelisp-syscall-table.el ends here\n");
    s
}
