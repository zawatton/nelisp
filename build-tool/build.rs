fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=CARGO_CFG_TARGET_OS");
    println!("cargo:rerun-if-env-changed=CARGO_CFG_TARGET_ARCH");

    let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    let target_arch = std::env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();
    let manifest_dir =
        std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR must be set by cargo");
    let out_path = std::path::Path::new(&manifest_dir)
        .join("..")
        .join("lisp")
        .join("nelisp-syscall-table.el");

    let body = if target_os == "linux" {
        emit_linux_table()
    } else {
        emit_unsupported_stub(&target_os)
    };

    let existing = std::fs::read_to_string(&out_path).unwrap_or_default();
    if existing != body {
        std::fs::write(&out_path, &body)
            .unwrap_or_else(|e| panic!("write {} failed: {}", out_path.display(), e));
    }

    let supported = (target_os == "linux" && (target_arch == "x86_64" || target_arch == "aarch64"))
        || (target_os == "macos" && (target_arch == "aarch64" || target_arch == "x86_64"))
        || (target_os == "windows" && target_arch == "x86_64");
    if supported {
        link_elisp_cc_spike(&manifest_dir, &target_os, &target_arch);
    }
}

/// Manifest entry count, kept in sync with `compile-elisp-objects-manifest'
/// in scripts/compile-elisp-objects.el.  Used only for chunk partitioning;
/// over-shooting is harmless (= elisp clamps end to manifest length, extra
/// chunks no-op), under-shooting silently skips entries.
const N_MANIFEST_ENTRIES: usize = 208;

fn link_elisp_cc_spike(manifest_dir: &str, target_os: &str, target_arch: &str) {
    let repo_root = std::path::Path::new(manifest_dir).join("..");
    let script = repo_root.join("scripts").join("compile-elisp-objects.el");
    let compiler_src = repo_root.join("lisp").join("nelisp-phase47-compiler.el");
    let layout_src = repo_root.join("lisp").join("nelisp-sexp-layout.el");

    let manifest_sources = [
        "nelisp-cc-spike-noop.el",
        "nelisp-cc-fact-i64.el",
        "nelisp-cc-truncate-int.el",
        "nelisp-cc-length-cons.el",
        "nelisp-cc-eq-symbol.el",
        "nelisp-cc-cons-construct.el",
        "nelisp-cc-bi-string-bytes.el",
        "nelisp-cc-recordp.el",
        "nelisp-cc-aref-vector.el",
        "nelisp-cc-bi-make-vector.el",
        "nelisp-cc-bi-nl-fact-i64.el",
        "nelisp-cc-bi-quit-flag.el",
        "nelisp-cc-bi-write-stderr-line.el",
        "nelisp-cc-bi-write-stdout-bytes.el",
        "nelisp-cc-bi-read-stdin-bytes.el",
        "nelisp-cc-bi-f64-trunc.el",
        "nelisp-cc-cell-ops.el",
        "nelisp-cc-mirror-lookup-entry.el",
        "nelisp-cc-mirror-lookup-value.el",
        "nelisp-cc-mirror-lookup-function.el",
        "nelisp-cc-mirror-is-bound.el",
        "nelisp-cc-mirror-is-fbound.el",
        "nelisp-cc-mirror-is-constant.el",
        "nelisp-cc-mirror-set-value.el",
        "nelisp-cc-mirror-set-function.el",
        "nelisp-cc-mirror-clear-value.el",
        "nelisp-cc-mirror-clear-function.el",
        "nelisp-cc-mirror-set-constant.el",
        "nelisp-cc-mirror-install-entry.el",
        "nelisp-cc-mirror-alloc-entry.el",
        "nelisp-cc-mirror-bucket-prepend.el",
        "nelisp-cc-mirror-set-value-or-insert.el",
        "nelisp-cc-mirror-set-function-or-insert.el",
        "nelisp-cc-mirror-set-constant-or-insert.el",
        "nelisp-cc-mirror-install-entry-or-insert.el",
        "nelisp-cc-frame-stack-view.el",
        "nelisp-cc-frame-ensure-capacity.el",
        "nelisp-cc-frame-push.el",
        "nelisp-cc-frame-pop.el",
        "nelisp-cc-frame-bind.el",
        "nelisp-cc-frame-stack-find.el",
        "nelisp-cc-frame-stack-install.el",
        "nelisp-cc-wrap-alist-cells.el",
        "nelisp-cc-fnv1a.el",
        "nelisp-cc-jit-arith.el",
        "nelisp-cc-jit-float.el",
        "nelisp-cc-jit-math.el",
        "nelisp-cc-jit-predicate-eq.el",
        "nelisp-cc-jit-ref-eq.el",
        "nelisp-cc-jit-type-of.el",
        "nelisp-cc-jit-sxhash.el",
        "nelisp-cc-jit-record.el",
        "nelisp-cc-jit-length.el",
        "nelisp-cc-jit-aref.el",
        "nelisp-cc-jit-aset.el",
        "nelisp-cc-jit-elt.el",
        "nelisp-cc-jit-access-aref-bool-vector-inner.el",
        "nelisp-cc-jit-access-aset-bool-vector-inner.el",
        "nelisp-cc-jit-cons.el",
        "nelisp-cc-jit-cons-car-ptr.el",
        "nelisp-cc-jit-cons-cdr-ptr.el",
        "nelisp-cc-sexp-write-str.el",
        "nelisp-cc-sexp-write-float.el",
        "nelisp-cc-mut-str.el",
        "nelisp-cc-atomic-raw-mem.el",
        "nelisp-cc-alloc-mem.el",
        "nelisp-cc-alloc-dealloc.el",
        "nelisp-cc-rc-inc.el",
        "nelisp-cc-rc-dec.el",
        "nelisp-cc-rc-strong-count.el",
        "nelisp-cc-rc-kind.el",
        "nelisp-cc-rc-payload-ptr.el",
        "nelisp-cc-gc-walk-children.el",
        "nelisp-cc-nlconsbox-clone.el",
        "nelisp-cc-nlconsbox-drop.el",
        "nelisp-cc-nlconsbox-alloc.el",
        "nelisp-cc-nlcell-alloc.el",
        "nelisp-cc-nlvector-alloc.el",
        "nelisp-cc-nlrecord-alloc.el",
        "nelisp-cc-nlvector-drop.el",
        "nelisp-cc-nlcell-drop.el",
        "nelisp-cc-nlrecord-drop.el",
        "nelisp-cc-nlstr-drop.el",
        "nelisp-cc-nlvector-clone.el",
        "nelisp-cc-nlcell-clone.el",
        "nelisp-cc-nlrecord-clone.el",
        "nelisp-cc-nlstr-clone.el",
        "nelisp-cc-nlboolvector-clone.el",
        "nelisp-cc-reader-lexer.el",
        "nelisp-cc-extern-call-f64.el",
        "nelisp-cc-reader-parser.el",
        "nelisp-cc-cstr-helpers.el",
        "nelisp-cc-struct-helpers.el",
        "nelisp-cc-jit-bool-vector-len.el",
        "nelisp-cc-jit-str-codepoint-at.el",
        "nelisp-cc-jit-make-symbol.el",
        "nelisp-cc-jit-type-of.el",
        "nelisp-cc-jit-sxhash.el",
        "nelisp-cc-jit-record-type-tag-ptr.el",
        "nelisp-cc-jit-concat-ints.el",
        "nelisp-cc-sf-quote.el",
        "nelisp-cc-jit-downcase.el",
        "nelisp-cc-jit-upcase.el",
        "nelisp-cc-jit-split-by-non-alnum.el",
        "nelisp-cc-sf-progn.el",
        "nelisp-cc-sf-if.el",
        "nelisp-cc-sf-setq.el",
        "nelisp-cc-sf-while.el",
        "nelisp-cc-sf-let.el",
        "nelisp-cc-sf-let-star.el",
        "nelisp-cc-sf-lambda.el",
        "nelisp-cc-sf-function.el",
        "nelisp-cc-sf-condition-case.el",
        "nelisp-cc-apply-lambda-inner.el",
        "nelisp-cc-bf-formal-tag.el",
        "nelisp-cc-bf-args-nth-ptr.el",
        "nelisp-cc-bf-precompute.el",
        "nelisp-cc-bind-formals.el",
        "nelisp-cc-jit-symbol-name.el",
        "nelisp-cc-sexp-eq.el",
        "nelisp-cc-symbol-is-lambda.el",
        "nelisp-cc-cons-prepend-clone.el",
        "nelisp-cc-jit-secure-hash.el",
        "nelisp-cc-jit-secure-hash-ext.el",
        "nelisp-cc-jit-regex.el",
        "nelisp-cc-jit-alias.el",
        "nelisp-cc-eval-inner.el",
        "nelisp-cc-jit-syscall-call.el",
        "nelisp-cc-env-lookup-value.el",
        "nelisp-cc-env-set-value.el",
        "nelisp-cc-env-lookup-function.el",
        "nelisp-cc-env-shim-set-op.el",
        "nelisp-cc-env-bind-local.el",
        "nelisp-cc-env-install-empty.el",
        "nelisp-cc-nlstr-utf8-direct.el",
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

    // Doc 49 §7 R6L: Wave 7 self-host bootstrap.  `NELISP_BUILD_WITH` env:
    //   "emacs" / unset → use emacs --batch (default, fast, no regression)
    //   "nelisp"        → require target/release/nelisp (release validation, slow)
    //   "auto"          → prefer nelisp if target/release/nelisp exists, else emacs
    // The nelisp path exists only for self-host validation in CI / release builds;
    // casual dev builds stay on the emacs path (cargo:rerun-if-changed semantics
    // unchanged).
    let build_with = std::env::var("NELISP_BUILD_WITH").unwrap_or_default();
    println!("cargo:rerun-if-env-changed=NELISP_BUILD_WITH");
    let nelisp_candidate = repo_root.join("target/release/nelisp");
    let interpreter: std::path::PathBuf = match build_with.as_str() {
        "nelisp" => {
            if !nelisp_candidate.exists() {
                panic!("NELISP_BUILD_WITH=nelisp but {} not found — run a default \
                        emacs-bootstrapped `cargo build --release` first", nelisp_candidate.display());
            }
            println!("cargo:warning=§99.B using nelisp self-host (NELISP_BUILD_WITH=nelisp)");
            nelisp_candidate.clone()
        }
        "auto" if nelisp_candidate.exists() => {
            println!("cargo:warning=§99.B using nelisp self-host (NELISP_BUILD_WITH=auto, found target/release/nelisp)");
            nelisp_candidate.clone()
        }
        _ => {
            let Some(emacs) = which_or_skip("emacs") else {
                println!("cargo:warning=skipping §99.B elisp-object link: emacs not on PATH (set NELISP_BUILD_WITH=nelisp if target/release/nelisp exists)");
                return;
            };
            emacs.into()
        }
    };

    let out_dir = std::env::var("OUT_DIR").expect("OUT_DIR must be set by cargo");
    let elisp_obj_dir = std::path::Path::new(&out_dir).join("elisp-objects");
    std::fs::create_dir_all(&elisp_obj_dir)
        .unwrap_or_else(|e| panic!("create_dir_all {}: {}", elisp_obj_dir.display(), e));

    let elisp_obj_dir_str = elisp_obj_dir.display().to_string();
    // Doc 49 Wave 9 R8: multi-process build parallelism.  Partition the
    // 208-entry manifest into NELISP_BUILD_THREADS (default 4) chunks; each
    // subprocess runs `compile-elisp-objects-emit-range' with disjoint
    // RANGE_START/END.  All chunks share the same elisp-objects dir (disjoint
    // output filenames, no race).  N=1 ≈ legacy single-process path.
    println!("cargo:rerun-if-env-changed=NELISP_BUILD_THREADS");
    let total = N_MANIFEST_ENTRIES;
    let n_threads = std::env::var("NELISP_BUILD_THREADS").ok()
        .and_then(|s| s.parse::<usize>().ok()).filter(|n| *n >= 1).unwrap_or(4).min(total);
    let chunk = (total + n_threads - 1) / n_threads;
    let shared = std::sync::Arc::new((interpreter.clone(), script.clone(), repo_root.join("lisp"),
        elisp_obj_dir_str.clone(), target_arch.to_string(), target_os.to_string()));
    let mut handles = Vec::with_capacity(n_threads);
    for i in 0..n_threads {
        let (start_i, end_i) = (i * chunk, ((i + 1) * chunk).min(total));
        if start_i >= end_i { break; }
        let sh = shared.clone();
        handles.push(std::thread::spawn(move || {
            let (interp, scr, lisp_dir, obj_dir, arch, os_) = (&sh.0, &sh.1, &sh.2, &sh.3, &sh.4, &sh.5);
            let st = std::process::Command::new(interp)
                .arg("--batch").arg("-Q").arg("-L").arg(lisp_dir)
                .arg("--eval").arg(format!("(setenv \"NELISP_ELISP_OBJECTS_DIR\" \"{}\")", obj_dir))
                .arg("--eval").arg(format!("(setenv \"NELISP_PHASE47_TARGET_ARCH\" \"{}\")", arch))
                .arg("--eval").arg(format!("(setenv \"NELISP_PHASE47_TARGET_OS\" \"{}\")", os_))
                .arg("--eval").arg(format!("(setenv \"NELISP_RANGE_START\" \"{}\")", start_i))
                .arg("--eval").arg(format!("(setenv \"NELISP_RANGE_END\" \"{}\")", end_i))
                .arg("-l").arg(scr).arg("-f").arg("compile-elisp-objects-emit-range")
                .env("NELISP_ELISP_OBJECTS_DIR", obj_dir).env("NELISP_PHASE47_TARGET_ARCH", arch)
                .env("NELISP_PHASE47_TARGET_OS", os_).env("NELISP_RANGE_START", start_i.to_string())
                .env("NELISP_RANGE_END", end_i.to_string()).status();
            (start_i, end_i, st)
        }));
    }
    let failures: Vec<String> = handles.into_iter().filter_map(|h| match h.join() {
        Ok((_, _, Ok(st))) if st.success() => None,
        Ok((s, e, Ok(st))) => Some(format!("chunk [{},{}) exit {}", s, e, st)),
        Ok((s, e, Err(err))) => Some(format!("chunk [{},{}) spawn err {}", s, e, err)),
        Err(_) => Some("chunk thread panicked".to_string()),
    }).collect();
    if !failures.is_empty() {
        panic!("compile-elisp-objects-emit-range failed: {} (script={})",
               failures.join("; "), script.display());
    }

    let mut obj_paths: Vec<std::path::PathBuf> = std::fs::read_dir(&elisp_obj_dir)
        .unwrap_or_else(|e| panic!("read_dir {}: {}", elisp_obj_dir.display(), e))
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("o"))
        .collect();
    if obj_paths.is_empty() { panic!("no .o files in {} after orchestrator run (manifest empty? silent failure?)", elisp_obj_dir.display()); }
    obj_paths.sort();
    let ar = which_or_skip("ar").unwrap_or_else(|| "ar".to_string());
    let archive = std::path::Path::new(&out_dir).join("libnelisp_elisp_spike.a");
    let _ = std::fs::remove_file(&archive);
    let mut cmd = std::process::Command::new(&ar);
    cmd.arg("rcs").arg(&archive);
    for p in &obj_paths { cmd.arg(p); }
    let status = cmd.status().unwrap_or_else(|e| panic!("ar failed to spawn: {}", e));
    if !status.success() { panic!("ar rcs {} exited with {}", archive.display(), status); }

    println!("cargo:rustc-link-search=native={}", out_dir);
    // Wave A22-unblock: `+whole-archive' modifier forces the linker to keep every
    // .o member of `libnelisp_elisp_spike.a' in the final binary, regardless of
    // whether any Rust code references the symbol.  This obsoletes the previous
    // `_ELISP_ARCHIVE_ANCHOR' bogus-signature array in src/lib.rs (which had to be
    // manually extended every time a new Phase 47 helper was added — a Rust-LOC
    // tax that blocked the carve-out waves).  Cost is ~10-15 KB binary size for
    // the ~57 helper symbols that were previously DCE'd; benefit is zero Rust LOC
    // per future Phase 47 helper.  Supported on GNU ld / lld / macOS ld / lld-link
    // via rustc's portable `whole-archive' modifier (stable since rustc 1.61).
    println!("cargo:rustc-link-lib=static:+whole-archive=nelisp_elisp_spike");
}

fn which_or_skip(prog: &str) -> Option<String> {
    let path = std::env::var_os("PATH")?;
    std::env::split_paths(&path)
        .map(|d| d.join(prog))
        .find(|c| c.is_file())
        .map(|c| c.to_string_lossy().into_owned())
}

fn emit_linux_table() -> String {
    let entries: &[(&str, i64)] = &[
        ("read", libc::SYS_read as i64),
        ("write", libc::SYS_write as i64),
        ("close", libc::SYS_close as i64),
        ("openat", libc::SYS_openat as i64),
        ("exit_group", libc::SYS_exit_group as i64),
        ("lseek", libc::SYS_lseek as i64),
        ("dup2", libc::SYS_dup2 as i64),
        ("getpid", libc::SYS_getpid as i64),
        ("kill", libc::SYS_kill as i64),
        ("mmap", libc::SYS_mmap as i64),
        ("mprotect", libc::SYS_mprotect as i64),
        ("munmap", libc::SYS_munmap as i64),
        ("fcntl", libc::SYS_fcntl as i64),
        ("fork", libc::SYS_fork as i64),
        ("socket", libc::SYS_socket as i64),
        ("listen", libc::SYS_listen as i64),
        ("wait4", libc::SYS_wait4 as i64),
        ("getppid", libc::SYS_getppid as i64),
        ("setpgid", libc::SYS_setpgid as i64),
        ("pidfd_open", libc::SYS_pidfd_open as i64),
        ("pidfd_send_signal", libc::SYS_pidfd_send_signal as i64),
        ("inotify_init1", libc::SYS_inotify_init1 as i64),
        ("inotify_rm_watch", libc::SYS_inotify_rm_watch as i64),
        ("eventfd2", libc::SYS_eventfd2 as i64),
        ("timerfd_create", libc::SYS_timerfd_create as i64),
    ];

    let table: String = entries.iter().map(|(n, nr)| format!("        ({} . {})\n", n, nr)).collect();
    format!(";;; nelisp-syscall-table.el --- Doc 84 §84.2 syscall nr table  -*- lexical-binding: t; -*-\n\
             \n;;; Commentary:\n\n\
             ;; AUTO-GENERATED by build-tool/build.rs. DO NOT EDIT.\n\n\
             ;;; Code:\n\n\
             (setq nelisp--syscall-nr-table\n      '(\n{table}        ))\n\n\
             (fset 'nelisp--syscall-nr-resolve\n      (lambda (name)\n\
               (if (integerp name)\n            name\n\
               (let ((cell (assq name nelisp--syscall-nr-table)))\n\
               (if cell (cdr cell)\n\
               (signal 'arith-error (cons \"nelisp--syscall-nr-resolve: unknown syscall\" (cons name nil))))))))\n\n\
             ;;; nelisp-syscall-table.el ends here\n")
}

fn emit_unsupported_stub(target_os: &str) -> String {
    format!(";;; nelisp-syscall-table.el --- stub for {target_os}  -*- lexical-binding: t; -*-\n\
             ;;; Code:\n(setq nelisp--syscall-nr-table nil)\n\
             (fset 'nelisp--syscall-nr-resolve (lambda (name) (signal 'arith-error (cons \"unsupported\" (cons name nil)))))\n\
             ;;; nelisp-syscall-table.el ends here\n")
}
