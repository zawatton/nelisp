long nl_ffi_runtime_provider(long value) { return value + 300; }

long nl_ffi_runtime_local_consumer(long value) {
  return nl_ffi_runtime_provider(value);
}
