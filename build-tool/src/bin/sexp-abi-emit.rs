// Doc 100 v2 §100.B — Sexp ABI layout dump driver.
//
// Prints the Rust-side `ABI_EXPORT' rows in `NAME=VALUE' form, one
// per line, so `make sexp-abi-check' can diff it against the elisp
// side (= `lisp/nelisp-sexp-layout.el' iterated by an `emacs --batch'
// dump).  See `docs/arch/sexp-abi.md' for the contract.

use nelisp_build_tool::eval::sexp::ABI_EXPORT;

fn main() {
    for (name, value) in ABI_EXPORT {
        println!("{name}={value}");
    }
}
