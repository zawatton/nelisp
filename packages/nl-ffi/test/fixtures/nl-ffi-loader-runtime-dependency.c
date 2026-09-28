extern long nl_ffi_runtime_provider(long value);

long nl_ffi_runtime_dependency(long value) {
  return nl_ffi_runtime_provider(value);
}
