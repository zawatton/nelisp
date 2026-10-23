/* pmcrun2.c -- hardware counters of one command with raw AMD/Intel events, user space only.
 *
 * pmcrun.c counts cycles, instructions, branches, branch misses and L1D misses.  This one
 * adds raw events from the environment, PMC_EVENTS="name:HEXCONFIG,...", where HEXCONFIG is
 * (umask << 8) | event.  At most three raw events fit next to cycles and instructions on
 * a Zen 3 core (five counters while the NMI watchdog holds one); more make the group
 * unschedulable and every count reads zero.
 *
 * Zen 3 (family 19h) events that explained the S2.3 layout sensitivity (callpath-perf ledger,
 * step 7 diagnosis): bp_l1_btb_correct 008A, bp_l2_btb_correct 008B (a taken branch served
 * by the slower second-level BTB), ex_ret_brn_tkn 00C4 (taken branches), ex_ret_brn_misp
 * 00C3, ls_stlf 0035 (store-to-load forwards), ls_bad_status2.stli_other 0224 (failed
 * forwarding), bp_de_redirect 0091, ic_fw32_miss 0081.
 *
 * Build: gcc -O1 -o pmcrun2 tools/ai/pmcrun2.c
 * Use:   PMC_EVENTS="l2btb:008B,tkn:00C4" taskset -c 12 setarch "$(uname -m)" -R ./pmcrun2 COMMAND...
 * Per-iteration figures: run at two iteration counts and divide the difference.
 */
#define _GNU_SOURCE
#include <linux/perf_event.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <stdint.h>
/* PMC_EVENTS="name:hexconfig,..." raw events (PERF_TYPE_RAW); cycles and instructions first always. */
struct ev { char name[40]; uint32_t type; uint64_t config; };
int main(int argc, char **argv) {
  struct ev evs[16]; int n = 0;
  strcpy(evs[n].name, "cycles"); evs[n].type = PERF_TYPE_HARDWARE; evs[n++].config = PERF_COUNT_HW_CPU_CYCLES;
  strcpy(evs[n].name, "instr"); evs[n].type = PERF_TYPE_HARDWARE; evs[n++].config = PERF_COUNT_HW_INSTRUCTIONS;
  char *e = getenv("PMC_EVENTS"); char *dup = e ? strdup(e) : strdup("");
  for (char *tok = strtok(dup, ","); tok && n < 16; tok = strtok(NULL, ",")) {
    char *c = strchr(tok, ':'); if (!c) continue; *c = 0;
    snprintf(evs[n].name, sizeof evs[n].name, "%s", tok); evs[n].type = PERF_TYPE_RAW; evs[n].config = strtoull(c + 1, 0, 16); n++;
  }
  if (argc < 2) return 2;
  int pfd[2]; if (pipe(pfd)) return 3;
  pid_t p = fork();
  if (p == 0) { char ch; close(pfd[1]); if (read(pfd[0], &ch, 1) < 0) _exit(126); execvp(argv[1], argv + 1); _exit(127); }
  int fds[16]; int leader = -1;
  for (int i = 0; i < n; i++) {
    struct perf_event_attr a; memset(&a, 0, sizeof a); a.size = sizeof a; a.type = evs[i].type; a.config = evs[i].config;
    a.disabled = (i == 0); a.enable_on_exec = (i == 0); a.inherit = 1; a.exclude_kernel = 1; a.exclude_hv = 1;
    a.read_format = PERF_FORMAT_GROUP;
    fds[i] = syscall(SYS_perf_event_open, &a, p, -1, leader, 0);
    if (fds[i] < 0) { perror(evs[i].name); kill(p, 9); return 4; }
    if (i == 0) leader = fds[0];
  }
  close(pfd[0]); if (write(pfd[1], "x", 1) < 0) return 5; close(pfd[1]);
  int st; waitpid(p, &st, 0);
  uint64_t buf[1 + 16]; memset(buf, 0, sizeof buf);
  if (read(leader, buf, sizeof buf) < 0) { perror("read"); return 6; }
  for (int i = 0; i < n; i++) printf("%s=%llu ", evs[i].name, (unsigned long long)buf[1 + i]);
  printf("rc=%d\n", WEXITSTATUS(st));
  return 0;
}
