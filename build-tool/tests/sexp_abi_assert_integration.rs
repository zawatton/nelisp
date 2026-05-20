use nelisp_build_tool::eval::sexp::{Sexp, SEXP_PAYLOAD_OFFSET, SEXP_TAG_INT};
use nelisp_build_tool::eval::sexp_abi_assert::ABI_EXPORT;

#[test]
fn abi_export_is_non_empty() {
    assert!(!ABI_EXPORT.is_empty());
}

#[test]
fn abi_export_keys_are_unique() {
    let mut seen = std::collections::HashSet::new();
    for (k, _) in ABI_EXPORT {
        assert!(seen.insert(*k), "duplicate key {k} in ABI_EXPORT");
    }
}

#[test]
fn abi_export_matches_constants() {
    // Spot-checks — the real assertions are the `const _: ()' blocks
    // above, but a runtime test makes the failure mode crystal clear
    // if someone bypasses the compile-time assertions by feature-
    // gating them.
    let map: std::collections::HashMap<_, _> = ABI_EXPORT.iter().copied().collect();
    assert_eq!(map["tag-int"], SEXP_TAG_INT as i64);
    assert_eq!(map["offset-payload"], SEXP_PAYLOAD_OFFSET as i64);
    assert_eq!(map["slot-size"], std::mem::size_of::<Sexp>() as i64);
}

#[test]
fn string_header_runtime_probe_matches_exported_offsets() {
    let mut s = String::with_capacity(32);
    s.push_str("foo");
    let words = &s as *const String as *const usize;
    let map: std::collections::HashMap<_, _> = ABI_EXPORT.iter().copied().collect();
    let cap_word = unsafe { *words.add(0) } as i64;
    let ptr_word = unsafe { *words.add(1) } as i64;
    let len_word = unsafe { *words.add(2) } as i64;
    assert_eq!(cap_word, 32);
    assert_ne!(ptr_word, 0);
    assert_eq!(len_word, 3);
    assert_eq!(map["string-offset-capacity"], SEXP_PAYLOAD_OFFSET as i64);
    assert_eq!(map["string-offset-ptr"], (SEXP_PAYLOAD_OFFSET + 8) as i64);
    assert_eq!(
        map["string-offset-length"],
        (SEXP_PAYLOAD_OFFSET + 16) as i64
    );
}
