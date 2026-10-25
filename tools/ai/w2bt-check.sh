#!/bin/bash
# w2bt structural check: the non-leaf backtrace record push/finish are lean.
# usage: w2bt-check.sh BINARY   (exit 0 = lean, 1 = not lean)
# Design property: nl_n4_push_words and nl_n4_finish_w contain no call except to
# their own cold helpers (nl_n4_push_words_cold, nl_n4_finish_w_cold).  Static
# instruction count and reserved frame are printed for information.
b=${1:?binary}
rc=0
for f in nl_n4_push_words nl_n4_finish_w; do
  d=$(objdump -d --no-show-raw-insn "$b" --disassemble="$f" | grep -P '^\s+[0-9a-f]+:\s')
  n=$(printf '%s\n' "$d" | grep -c .)
  fr=$(printf '%s\n' "$d" | grep -oP 'sub\s+\$0x\K[0-9a-f]+(?=,%rsp)' | head -1)
  bad=$(printf '%s\n' "$d" | grep -P '\bcall\b' | grep -vP '<nl_n4_(push_words|finish_w)_cold>' | wc -l)
  echo "$f instr=$n frame=$((16#${fr:-0})) non-cold-calls=$bad"
  [ "$n" -gt 0 ] && [ "$bad" -eq 0 ] || rc=1
done
exit $rc
