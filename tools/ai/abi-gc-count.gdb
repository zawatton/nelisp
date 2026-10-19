# Fast calibration only: gdb -q -batch -x tools/ai/abi-gc-count.gdb --args BIN --load FILE
# Callgrind remains authoritative for the measured Ir and sweep counts.
set pagination off
set confirm off
set disable-randomization off
set breakpoint pending off
set $abi_gc_count = 0
break nl_gc_sweep
commands
silent
set $abi_gc_count = $abi_gc_count + 1
continue
end
run
# A signal leaves $_exitcode void, so this also rejects a crashing probe.
if $_exitcode != 0
quit 1
end
printf "ABI_GC_COUNT=%ld\n", $abi_gc_count
if $abi_gc_count < 10
quit 1
end
quit 0
