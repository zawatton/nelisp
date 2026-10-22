/* pmcrun.c -- hardware performance counters of one command (cycles, instructions,
 * branches, branch misses, L1D read misses), user space only, children included.
 *
 * Why it exists: callgrind counts instructions, but a wall-clock row can move with the
 * code address of an unchanged loop (the S2.4 bytecode `let' row: identical Ir, one branch
 * miss per iteration in one layout and none in another, 133 against 79 cycles).  The
 * counters show that directly.  `perf' is not installed here; perf_event_open with
 * kernel.perf_event_paranoid <= 1 counts one's own processes, which is all this needs.
 *
 * Build: gcc -O1 -o pmcrun tools/ai/pmcrun.c
 * Use:   taskset -c 12 setarch "$(uname -m)" -R ./pmcrun COMMAND [ARGS...]
 *        prints  cycles=N instr=N branches=N br_miss=N l1d_miss=N rc=EXIT
 * Per-iteration figures: run the same command at two iteration counts (say 20 and
 * 3000000) and divide the difference by the iteration difference; the start-up cost
 * (about 120M cycles for the standalone binary) cancels.
 */
#define _GNU_SOURCE
#include <linux/perf_event.h>
#include <sys/syscall.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <stdint.h>
#include <fcntl.h>
struct ev { const char *name; uint32_t type; uint64_t config; };
static struct ev evs[] = {
  {"cycles", PERF_TYPE_HARDWARE, PERF_COUNT_HW_CPU_CYCLES},
  {"instr", PERF_TYPE_HARDWARE, PERF_COUNT_HW_INSTRUCTIONS},
  {"branches", PERF_TYPE_HARDWARE, PERF_COUNT_HW_BRANCH_INSTRUCTIONS},
  {"br_miss", PERF_TYPE_HARDWARE, PERF_COUNT_HW_BRANCH_MISSES},
  {"l1d_miss", PERF_TYPE_HW_CACHE, PERF_COUNT_HW_CACHE_L1D | (PERF_COUNT_HW_CACHE_OP_READ << 8) | (PERF_COUNT_HW_CACHE_RESULT_MISS << 16)},
};
#define NEV (int)(sizeof evs / sizeof evs[0])
int main(int argc, char **argv) {
  if (argc < 2) return 2;
  int pfd[2]; if (pipe(pfd)) return 3;
  pid_t p = fork();
  if (p == 0) { char c; close(pfd[1]); if (read(pfd[0], &c, 1) < 0) _exit(126); execvp(argv[1], argv + 1); _exit(127); }
  int fds[NEV]; int leader = -1;
  for (int i = 0; i < NEV; i++) {
    struct perf_event_attr a; memset(&a, 0, sizeof a); a.size = sizeof a; a.type = evs[i].type; a.config = evs[i].config;
    a.disabled = (i == 0); a.enable_on_exec = (i == 0); a.inherit = 1; a.exclude_kernel = 1; a.exclude_hv = 1;
    a.read_format = PERF_FORMAT_GROUP;
    fds[i] = syscall(SYS_perf_event_open, &a, p, -1, leader, 0);
    if (fds[i] < 0) { perror(evs[i].name); kill(p, 9); return 4; }
    if (i == 0) leader = fds[0];
  }
  close(pfd[0]); if (write(pfd[1], "x", 1) < 0) return 5; close(pfd[1]);
  int st; waitpid(p, &st, 0);
  uint64_t buf[1 + NEV]; memset(buf, 0, sizeof buf);
  if (read(leader, buf, sizeof buf) < 0) { perror("read"); return 6; }
  for (int i = 0; i < NEV; i++) printf("%s=%llu ", evs[i].name, (unsigned long long)buf[1 + i]);
  printf("rc=%d\n", WEXITSTATUS(st));
  return 0;
}
