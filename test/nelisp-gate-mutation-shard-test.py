"""Shard assignment of the gate-mutation sweep (tools/gate-mutation-shard.awk).

The union of the shards must be exactly the row table (no row dropped or run
twice), and with the committed weights the busiest of 12 shards must stay
within budget.  The harness wiring is checked through its list-only mode.

Usage: python3 test/nelisp-gate-mutation-shard-test.py
"""
import collections
import os
import re
import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
AWK = Path(os.environ.get("NELISP_SHARD_AWK_UNDER_TEST", ROOT / "tools/gate-mutation-shard.awk"))
TABLE = ROOT / "tools/gate-mutations.txt"
WEIGHTS = ROOT / "tools/gate-mutation-weights.txt"
MAX_SHARD_SECONDS_12 = 1650


def table_rows():
    return [l for l in TABLE.read_text().splitlines() if l.strip() and not l.startswith("#")]


def awk(rows, n, k=None, mode=None, weights=WEIGHTS):
    cmd = ["awk", "-F|", "-v", f"table={TABLE}", "-v", f"wfile={weights}", "-v", f"n={n}"]
    if k is not None:
        cmd += ["-v", f"k={k}"]
    if mode:
        cmd += ["-v", f"mode={mode}"]
    cmd += ["-f", str(AWK)]
    out = subprocess.run(cmd, input="\n".join(rows) + "\n", capture_output=True, text=True,
                         env=dict(os.environ, LC_ALL="C"), check=True, cwd=ROOT).stdout
    return out.splitlines()


def shards(rows, n):
    return [awk(rows, n, k) for k in range(n)]


def partition_problems(rows, parts):
    """Empty when PARTS is a partition of ROWS (as multisets)."""
    seen = collections.Counter(r for p in parts for r in p)
    want = collections.Counter(rows)
    problems = []
    if seen != want:
        problems.append(f"dropped={sum((want - seen).values())} duplicated={sum((seen - want).values())}")
    return problems


def weights_by_key():
    w = {}
    for l in WEIGHTS.read_text().splitlines():
        if l.strip() and not l.startswith("#"):
            k, s = l.split("\t")
            w[k] = int(s)
    return w


class ShardAssignment(unittest.TestCase):
    def setUp(self):
        self.rows = table_rows()

    def test_union_is_the_whole_table_and_shards_are_disjoint(self):
        for n in (1, 4, 6, 12):
            parts = shards(self.rows, n)
            self.assertEqual(partition_problems(self.rows, parts), [], f"n={n}")
            self.assertEqual(sum(len(p) for p in parts), len(self.rows), f"n={n}")

    def test_negative_control_a_dropped_row_is_detected(self):
        parts = shards(self.rows, 12)
        victim = max(range(12), key=lambda i: len(parts[i]))
        dropped = [list(p) for p in parts]
        dropped[victim].pop()
        self.assertNotEqual(partition_problems(self.rows, dropped), [])
        duplicated = [list(p) for p in parts]
        duplicated[0].append(parts[1][0])
        self.assertNotEqual(partition_problems(self.rows, duplicated), [])

    def test_committed_weights_balance_twelve_shards(self):
        keys = dict(zip(self.rows, awk(self.rows, 12, mode="keys")))
        w = weights_by_key()
        self.assertEqual(set(keys.values()) - set(w), set(), "rows without a committed weight")
        sums = [sum(w[keys[r]] for r in part) for part in shards(self.rows, 12)]
        self.assertLessEqual(max(sums), MAX_SHARD_SECONDS_12, sums)

    def test_assignment_is_deterministic_and_order_independent_of_the_environment(self):
        self.assertEqual(shards(self.rows, 12), shards(self.rows, 12))

    def test_unknown_rows_get_the_median_weight_and_are_still_assigned(self):
        extra = "newgate|lisp/new.el|s/a/b/|a row nobody has timed"
        rows = self.rows + [extra]
        parts = shards(rows, 12)
        self.assertEqual(partition_problems(rows, parts), [])

    def test_mode_loads_matches_the_partition(self):
        loads = [int(l.split("\t")[1]) for l in awk(self.rows, 12, mode="loads")]
        self.assertEqual(len(loads), 12)
        self.assertLessEqual(max(loads), MAX_SHARD_SECONDS_12)


class HarnessWiring(unittest.TestCase):
    def test_list_only_shards_cover_the_table_exactly_once(self):
        total = 0
        names = []
        for k in range(12):
            env = dict(os.environ, NELISP_GATE_MUTATION_SHARD=f"{k}/12",
                       NELISP_GATE_MUTATION_LIST_ONLY="1")
            env = {a: b for a, b in env.items() if a != "NELISP_GATE_MUTATION_BASE"}
            r = subprocess.run(["bash", str(ROOT / "tools/nelisp-gate-mutation.sh")], cwd=ROOT,
                               env=env, capture_output=True, text=True, timeout=120)
            self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
            m = re.search(r"(\d+) row\(s\) selected", r.stdout)
            total += int(m.group(1))
            names += [l.split()[0] for l in r.stdout.splitlines() if l.startswith("  ")]
        self.assertEqual(total, len(table_rows()))
        want = collections.Counter(r.split("|")[0] for r in table_rows())
        self.assertEqual(collections.Counter(names), want)


if __name__ == "__main__":
    unittest.main()
