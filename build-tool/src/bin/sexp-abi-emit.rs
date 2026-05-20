use nelisp_build_tool::eval::sexp::ABI_EXPORT;
fn main() { for (name, value) in ABI_EXPORT { println!("{name}={value}"); } }
