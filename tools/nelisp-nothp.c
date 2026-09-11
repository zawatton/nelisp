/* nelisp-nothp.c -- run a program with transparent huge pages disabled.
 *
 * Why this exists.  The Linux one-hour soak is a release gate, and on a host
 * whose /sys/kernel/mm/transparent_hugepage/enabled is [always] it failed
 * repeatedly while the same soak passed on CI.  The two runs below differ in
 * one variable and settle it -- same binary
 * (sha256 6c9ab049f199644eb4ff8377b0ea882d8f11880a78b64ab8da4faa857ec177e6),
 * same host, same workload, measured 2026-09-12:
 *
 *   THP as the host has it ([always]):
 *       FAIL at 1135.7s, RSS 61844 -> 70688 KiB (+8844), ceiling 5120,
 *       smaps_rollup AnonHugePages 51200 KiB
 *   THP disabled for the child only, via this wrapper:
 *       PASS, 1500.6s, 926 batches, RSS 53084 KiB with peak == start --
 *       no growth at all
 *
 * A leak would grow the heap with or without huge pages.  This one does not
 * appear without them, and the 5120 KiB growth ceiling is smaller than three
 * 2 MiB huge pages, so a process backed by THP can cross it on allocation
 * granularity alone.  The starting RSS differs by 8760 KiB between the two
 * runs, which is the same order as the resident huge pages themselves.
 *
 * Build:  cc -O2 -o nelisp-nothp tools/nelisp-nothp.c
 * Use:    ./nelisp-nothp target/nelisp ...
 *
 * prctl(PR_SET_THP_DISABLE) is per-process and needs no privilege, so this
 * isolates the policy without a global /sys change that would affect
 * everything else running on the machine.
 */
#include <stdio.h>
#include <unistd.h>
#include <sys/prctl.h>
#ifndef PR_SET_THP_DISABLE
#define PR_SET_THP_DISABLE 41
#endif
int main(int argc, char **argv) {
  if (argc < 2) { fprintf(stderr, "usage: nothp PROGRAM [ARGS...]\n"); return 2; }
  if (prctl(PR_SET_THP_DISABLE, 1, 0, 0, 0) != 0) {
    perror("prctl(PR_SET_THP_DISABLE)");
    return 3;
  }
  execvp(argv[1], &argv[1]);
  perror("execvp");
  return 4;
}
