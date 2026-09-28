extern long nl_ffi_runtime_provider(long value);
__asm__(".type nl_ffi_runtime_provider, @function");

long nl_ffi_runtime_consumer(long value) {
  return nl_ffi_runtime_provider(value) + 1;
}
