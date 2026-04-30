//! Developer CLI bridge for executing raw native-code bytes.
//!
//! This binary intentionally lives outside `nelisp-runtime`: Doc 49
//! keeps the runtime crate as the Rust-min OS ABI substrate, while this
//! CLI remains a test/dev bridge for the native compiler.

use std::env;

fn parse_exec_i64_args(args: &[String]) -> Result<[i64; 6], String> {
    if args.len() > 6 {
        return Err(format!(
            "exec-bytes: expected at most 6 i64 args, got {}",
            args.len()
        ));
    }

    let mut parsed = [0_i64; 6];
    for (idx, raw) in args.iter().enumerate() {
        parsed[idx] = raw.parse::<i64>().map_err(|e| {
            format!(
                "exec-bytes: invalid i64 arg #{} {:?}: {}",
                idx + 1,
                raw,
                e
            )
        })?;
    }
    Ok(parsed)
}

fn exec_bytes(path: &str, raw_args: &[String]) -> i32 {
    let args = match parse_exec_i64_args(raw_args) {
        Ok(args) => args,
        Err(e) => {
            eprintln!("nelisp-exec-bytes: {}", e);
            return 1;
        }
    };

    let bytes = match std::fs::read(path) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("nelisp-exec-bytes: read {}: {}", path, e);
            return 4;
        }
    };
    let len = bytes.len();
    if len == 0 {
        eprintln!("nelisp-exec-bytes: empty input file {}", path);
        return 6;
    }

    let page_size: usize = 4096;
    let mapped_size = ((len + page_size - 1) / page_size) * page_size;

    unsafe {
        let map_jit = nelisp_runtime::NELISP_MAP_JIT;
        let prot_rw = nelisp_runtime::NELISP_PROT_READ | nelisp_runtime::NELISP_PROT_WRITE;
        let flags = nelisp_runtime::NELISP_MAP_PRIVATE
            | nelisp_runtime::NELISP_MAP_ANONYMOUS
            | map_jit;

        let p = nelisp_runtime::nelisp_syscall_mmap_jit(
            std::ptr::null_mut(),
            mapped_size,
            prot_rw,
            flags,
            -1,
            0,
        );
        if p.is_null() || p as isize == -1 {
            eprintln!("nelisp-exec-bytes: mmap_jit failed");
            return 3;
        }

        nelisp_runtime::nelisp_syscall_jit_write_protect(0);
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), p, len);
        nelisp_runtime::nelisp_syscall_jit_write_protect(1);

        let prot_rx = nelisp_runtime::NELISP_PROT_READ
            | nelisp_runtime::NELISP_PROT_WRITE
            | nelisp_runtime::NELISP_PROT_EXEC;
        let mp = nelisp_runtime::nelisp_syscall_mprotect(p, mapped_size, prot_rx);
        if mp != 0 {
            eprintln!("nelisp-exec-bytes: mprotect(RWX) failed");
            let _ = nelisp_runtime::nelisp_syscall_munmap(p, mapped_size);
            return 5;
        }

        let _ = nelisp_runtime::nelisp_syscall_clear_icache(p, p.add(len));

        let func: extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 =
            std::mem::transmute(p);
        let result = func(args[0], args[1], args[2], args[3], args[4], args[5]);

        println!("RESULT: {}", result);

        let _ = nelisp_runtime::nelisp_syscall_munmap(p, mapped_size);
    }

    0
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let code = match args.get(1).map(|s| s.as_str()) {
        Some("--version") => {
            println!("nelisp-exec-bytes {}", env!("CARGO_PKG_VERSION"));
            0
        }
        Some(path) => exec_bytes(path, &args[2..]),
        None => {
            eprintln!("usage: nelisp-exec-bytes <bytes-file> [ARG1 ... ARG6]");
            2
        }
    };
    std::process::exit(code);
}

#[cfg(test)]
mod tests {
    use super::parse_exec_i64_args;

    fn strings(xs: &[&str]) -> Vec<String> {
        xs.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn parse_exec_i64_args_pads_to_six() {
        let parsed = parse_exec_i64_args(&strings(&["5", "-2"])).unwrap();
        assert_eq!(parsed, [5, -2, 0, 0, 0, 0]);
    }

    #[test]
    fn parse_exec_i64_args_rejects_too_many() {
        let err = parse_exec_i64_args(&strings(&["1", "2", "3", "4", "5", "6", "7"]))
            .unwrap_err();
        assert!(err.contains("at most 6"));
    }

    #[test]
    fn parse_exec_i64_args_rejects_non_i64() {
        let err = parse_exec_i64_args(&strings(&["nope"])).unwrap_err();
        assert!(err.contains("invalid i64 arg #1"));
    }
}
