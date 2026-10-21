/* Focused native storage acceptance. The tested implementation is Elisp. */
/* SPDX-License-Identifier: GPL-3.0-or-later */
#include <assert.h>
#include <stdint.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

uint64_t nl_strings_state[20], nl_gc_stats[8];
uintptr_t nl_os_alloc_chunk(size_t n) {
  void *p = mmap(NULL, n, PROT_READ | PROT_WRITE,
                 MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  return p == MAP_FAILED ? 0 : (uintptr_t)p;
}
int nl_os_free_chunk(void *p, size_t n) { return munmap(p, n); }
int nl_os_commit_range(void *p, size_t off, size_t n) { (void)p;(void)off;(void)n;return 1; }
uintptr_t nl_alloc_bytes(size_t n, size_t align) {
  (void)align;
  return (uintptr_t)calloc(1, n);
}
uintptr_t nl_string_state_address(void) { return (uintptr_t)nl_strings_state; }
int nl_gc_conserv_pinned_p(void) { return 0; }
extern uintptr_t nl_string_mark(uintptr_t);
int nl_gc_mark_word(uintptr_t w) { return (w & 7) == 4 ? nl_string_mark(w - 4) : 0; }
uintptr_t wf_copy32(uintptr_t to, uintptr_t from) { (void)to; (void)from; abort(); }
uintptr_t nl_sci_prog2(uintptr_t effect, uintptr_t value) { (void)effect; return value; }
uintptr_t nl_sci_store_imm(uintptr_t word, uintptr_t dst) { (void)word;(void)dst;abort(); }
uintptr_t nl_sci_dispatch(uintptr_t src, uintptr_t dst, uintptr_t tag) { (void)src;(void)dst;(void)tag;abort(); }

extern uintptr_t nl_string_new(const void *, size_t, int);
extern size_t nl_string_sdata_size(size_t);
extern uintptr_t nl_string_mark(uintptr_t);
extern size_t nl_sweep_strings(void);
extern int nl_string_pin_word(uintptr_t);
extern uintptr_t nl_g4arm_copy_w(uintptr_t, uintptr_t);
extern uintptr_t nl_string_store_view(uintptr_t, uintptr_t);
extern uintptr_t nl_string_from_view(uintptr_t);
extern uintptr_t nl_sexp_clone_into(uintptr_t, uintptr_t);
extern uintptr_t nl_string_read64(uintptr_t, size_t);
extern uint64_t strings_equal_probe(uintptr_t, uintptr_t);
extern uint64_t strings_tag_probe(uintptr_t);
extern uint64_t strings_name_probe(uintptr_t);
extern uint64_t strings_byte_probe(uintptr_t, size_t);
extern uint64_t strings_length_probe(uintptr_t);

static uint64_t *header(uintptr_t word) { return (uint64_t *)(word - 4); }
static uintptr_t parallel_words[4][1000];
static void *allocate_parallel(void *arg) {
  size_t worker = (size_t)arg;
  for (int i = 0; i < 1000; i++) {
    parallel_words[worker][i] = nl_string_new("abcdefgh", 8, 1);
    assert(nl_string_read64(parallel_words[worker][i], 24) == 8);
  }
  return NULL;
}
static int compare_words(const void *a, const void *b) {
  uintptr_t x = *(const uintptr_t *)a, y = *(const uintptr_t *)b;
  return (x > y) - (x < y);
}
int main(void) {
  assert(nl_string_sdata_size(8) == 24);
  uintptr_t uninit = nl_string_new(NULL, 8, 1);
  assert(header(uninit)[0] == 8 && ((char *)header(uninit)[3])[8] == 0);
  uintptr_t a = nl_string_new("abcdefgh", 8, 1);
  assert((a & 7) == 4 && header(a)[0] == 8);
  assert(nl_g4arm_copy_w(0, a) == a);
  uint64_t view[4];
  uintptr_t mb = nl_string_new("abcdefgh", 8, 0);
  nl_string_store_view(mb, (uintptr_t)view);
  assert((view[1] & (UINT64_C(1) << 61)) != 0);
  header(mb)[1] = UINT64_MAX;
  assert(nl_string_from_view((uintptr_t)view) == mb && header(mb)[1] == UINT64_MAX);
  uintptr_t unicode = nl_string_new("\xe6\xbc\xa2", 3, 0);
  uint64_t unicode_view[4], unicode_copy[4];
  nl_string_store_view(unicode, (uintptr_t)unicode_view);
  nl_sexp_clone_into((uintptr_t)unicode_view, (uintptr_t)unicode_copy);
  assert(nl_string_from_view((uintptr_t)unicode_copy) == unicode);

  assert((int64_t)header(a)[1] == -1 && header(a)[2] == 0);
  assert(memcmp((void *)header(a)[3], "abcdefgh\0", 9) == 0);
  assert(((uint64_t *)header(a)[3])[-1] == a - 4);
  assert(nl_string_read64(a, 0) == 14 && nl_string_read64(a, 24) == 8);
  assert(nl_string_read64(a, (size_t)-1) == 14);
  uint64_t symbol[4] = {4, 8, (uintptr_t)"abcdefgh", 8};
  assert(strings_equal_probe((uintptr_t)symbol, (uintptr_t)symbol) == 1);
  assert(strings_equal_probe(a, (uintptr_t)symbol) == 1);
  assert(strings_equal_probe((uintptr_t)symbol, a) == 1);
  assert(strings_tag_probe(a) == 14 && strings_tag_probe((uintptr_t)symbol) == 4);
  assert(strings_tag_probe(21) == 2 && strings_tag_probe(3) == 0 && strings_tag_probe(7) == 1);
  uint64_t vector_header[3] = {520, 0, 0};
  assert(strings_tag_probe((uintptr_t)vector_header+6) == 8);
  assert(strings_name_probe(a) == 1 && strings_name_probe((uintptr_t)symbol) == 1);
  assert(strings_byte_probe(a, 3) == 'd' && strings_byte_probe((uintptr_t)symbol, 3) == 'd');
  assert(strings_length_probe(a) == 8 && strings_length_probe((uintptr_t)symbol) == 8);
  assert(strings_name_probe(nl_string_new("abcdefgi", 8, 1)) == 0);
  assert(strings_equal_probe(a, nl_string_new("abcdefgi", 8, 1)) == 0);
  uintptr_t live[1000];
  for (int i = 0; i < 1000; i++) {
    (void)nl_string_new("dead-dead-dead", 14, 1);
    live[i] = nl_string_new("abcdefgh", 8, 1);
  }
  uintptr_t old = header(live[999])[3];
#ifndef STRINGS_KERNEL_NEGATIVE_CONTROL
  for (int i = 0; i < 1000; i++) nl_string_mark(live[i] - 4);
#endif
  assert(nl_sweep_strings() == 1000);
  assert(nl_strings_state[5] == 1000 && nl_strings_state[6] == 56000);
  assert(header(live[999])[3] != old && nl_strings_state[10] > 0);
  int blocks = 0;
  for (uint64_t *b = (uint64_t *)nl_strings_state[2]; b; b = (uint64_t *)b[0]) blocks++;
  assert(blocks <= 3);
  for (int i = 0; i < 1000; i++) {
    assert(memcmp((void *)header(live[i])[3], "abcdefgh\0", 9) == 0);
    assert(((uint64_t *)header(live[i])[3])[-1] == live[i] - 4);
  }
  char large[2049]; memset(large, 'x', sizeof large);
  uintptr_t big = nl_string_new(large, sizeof large, 1);
  uintptr_t big_data = header(big)[3];
  nl_string_mark(big - 4);
  assert(nl_sweep_strings() == 1);
  assert(memcmp((void *)header(big)[3], large, sizeof large) == 0);
  assert(nl_sweep_strings() == 0 && nl_strings_state[4] == 0);
  assert(nl_string_pin_word(big_data) == 0 && nl_strings_state[19] == 0);
  pthread_t threads[4];
  for (size_t i = 0; i < 4; i++) assert(pthread_create(&threads[i], NULL, allocate_parallel, (void *)i) == 0);
  for (int i = 0; i < 4; i++) assert(pthread_join(threads[i], NULL) == 0);
  qsort(parallel_words, 4000, sizeof(uintptr_t), compare_words);
  uintptr_t *words = &parallel_words[0][0];
  for (int i = 0; i < 4000; i++) {
    assert(i == 0 || words[i] != words[i-1]);
    assert(memcmp((void *)header(words[i])[3], "abcdefgh", 8) == 0);
    nl_string_mark(words[i] - 4);
  }
  assert(nl_sweep_strings() == 4000);
  nl_sweep_strings();
  uintptr_t pinned = nl_string_new("pinned-data", 11, 1);
  uintptr_t data = header(pinned)[3];
  assert(nl_string_pin_word(data + 3) == 1);
  nl_sweep_strings();
  assert(nl_strings_state[5] == 1 && header(pinned)[3] == data);
  assert(memcmp((void *)data, "pinned-data", 11) == 0);
  nl_sweep_strings();
  assert(nl_strings_state[5] == 0);
  puts("PASS string kernel: GNU header, 56 B/string, compaction, back-pointers, large sweep");
}
