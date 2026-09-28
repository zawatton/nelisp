extern long nl_ffi_runtime_dependency(long value);

long nl_ffi_runtime_root(long value) {
  return nl_ffi_runtime_dependency(value);
}
