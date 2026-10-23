/* dispatch-probe-driver.c -- the cost of one opcode dispatch, comparison tree against jump table.
 *
 *   gcc -O1 -o dispatch-probe-driver tools/ai/dispatch-probe-driver.c
 *   taskset -c 12 ./dispatch-probe-driver BLOB OFF_TREE OFF_DENSE OFF_NOP TABLE OPS [ROUNDS]
 *
 * BLOB is the text of tools/ai/dispatch-probe-compile.el (SYMS gives the three offsets), TABLE and
 * OPS come from tools/ai/dispatch-probe-gen.py.  The blob is mapped executable and each probe is
 * called once per opcode of OPS in a loop; user-space cycles, instructions, branches and branch
 * misses come from perf_event_open (kernel.perf_event_paranoid <= 1), the best of ROUNDS runs, and
 * the figures printed are per dispatch above the loop's own cost (probe_nop).
 */
#define _GNU_SOURCE
#include <linux/perf_event.h>
#include <sys/syscall.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>
struct ev { const char *name; uint64_t config; };
static struct ev evs[] = {
  {"cycles", PERF_COUNT_HW_CPU_CYCLES}, {"instr", PERF_COUNT_HW_INSTRUCTIONS},
  {"branches", PERF_COUNT_HW_BRANCH_INSTRUCTIONS}, {"br_miss", PERF_COUNT_HW_BRANCH_MISSES}};
#define NEV 4
static int fds[NEV];
static void open_counters(void) {
  int leader = -1;
  for (int i = 0; i < NEV; i++) {
    struct perf_event_attr a; memset(&a, 0, sizeof a); a.size = sizeof a; a.type = PERF_TYPE_HARDWARE; a.config = evs[i].config;
    a.disabled = (i == 0); a.exclude_kernel = 1; a.exclude_hv = 1; a.read_format = PERF_FORMAT_GROUP;
    fds[i] = syscall(SYS_perf_event_open, &a, 0, -1, leader, 0);
    if (fds[i] < 0) { perror(evs[i].name); exit(4); }
    if (i == 0) leader = fds[0];
  }
}
static void run_start(void) { ioctl(fds[0], PERF_EVENT_IOC_RESET, PERF_IOC_FLAG_GROUP); ioctl(fds[0], PERF_EVENT_IOC_ENABLE, PERF_IOC_FLAG_GROUP); }
static void run_stop(uint64_t out[NEV]) {
  ioctl(fds[0], PERF_EVENT_IOC_DISABLE, PERF_IOC_FLAG_GROUP);
  uint64_t buf[1 + NEV]; if (read(fds[0], buf, sizeof buf) < 0) { perror("read"); exit(5); }
  for (int i = 0; i < NEV; i++) out[i] = buf[1 + i];
}
typedef long (*fn1)(long);
typedef long (*fn2)(const unsigned char *, long);
static unsigned char *slurp(const char *path, size_t *n) {
  FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(2); }
  fseek(f, 0, SEEK_END); *n = ftell(f); fseek(f, 0, SEEK_SET);
  unsigned char *b = malloc(*n); if (fread(b, 1, *n, f) != *n) exit(3); fclose(f); return b;
}
int main(int argc, char **argv) {
  if (argc < 7) { fprintf(stderr, "usage: driver BLOB OFF_TREE OFF_DENSE OFF_NOP TABLE SEQ [ROUNDS]\n"); return 2; }
  size_t bn, tn, sn; unsigned char *blob = slurp(argv[1], &bn); unsigned char *table = slurp(argv[5], &tn); unsigned char *seq = slurp(argv[6], &sn);
  long rounds = argc > 7 ? atol(argv[7]) : 20;
  unsigned char *code = mmap(NULL, (bn + 4095) & ~4095UL, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  memcpy(code, blob, bn);
  fn1 tree = (fn1)(code + atol(argv[2])); fn2 dense = (fn2)(code + atol(argv[3])); fn1 nop = (fn1)(code + atol(argv[4]));
  open_counters();
  const char *names[3] = {"nop", "tree", "dense"};
  uint64_t c[3][NEV]; long sink = 0;
  for (int variant = 0; variant < 3; variant++) {
    for (int warm = 0; warm < 3; warm++) {
      for (size_t i = 0; i < sn; i++) sink += (variant == 0 ? nop(seq[i]) : variant == 1 ? tree(seq[i]) : dense(table, seq[i]));
    }
    uint64_t best[NEV] = {0}; int have = 0;
    for (long r = 0; r < rounds; r++) {
      uint64_t o[NEV];
      run_start();
      if (variant == 0) for (size_t i = 0; i < sn; i++) sink += nop(seq[i]);
      else if (variant == 1) for (size_t i = 0; i < sn; i++) sink += tree(seq[i]);
      else for (size_t i = 0; i < sn; i++) sink += dense(table, seq[i]);
      run_stop(o);
      if (!have || o[0] < best[0]) { memcpy(best, o, sizeof best); have = 1; }
    }
    memcpy(c[variant], best, sizeof best);
  }
  printf("ops=%zu  (per op, minus the nop loop)\n", sn);
  for (int v = 1; v < 3; v++) {
    printf("%-6s cycles=%.2f instr=%.2f branches=%.2f br_miss=%.4f\n", names[v],
           ((double)c[v][0] - c[0][0]) / sn, ((double)c[v][1] - c[0][1]) / sn,
           ((double)c[v][2] - c[0][2]) / sn, ((double)c[v][3] - c[0][3]) / sn);
  }
  printf("(nop loop: cycles=%.2f instr=%.2f)  sink=%ld\n", (double)c[0][0] / sn, (double)c[0][1] / sn, sink & 1);
  return 0;
}
