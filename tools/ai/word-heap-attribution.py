#!/usr/bin/env python3
"""Attribute retained Callgrind instruction costs without charging call edges twice.

Header loads are a CONFIRMED LOWER BOUND, not a complete taint analysis:
audited decoder/accessor loads plus the distinctive inlined nl_val_tag decoder.
All other data reads remain explicit. Materialiser/copy costs are exclusive
self Ir, including their own header loads; those columns therefore overlap.
"""
import collections
import json
from pathlib import Path
import re
import subprocess
import sys

MATERIALISERS = {"nl_cons_car_ptr", "nl_cons_cdr_ptr", "nl_vector_slot_ptr", "nl_record_slot_ptr", "wf_arg_ptr", "wf_word_view"}
PURE_HEADER = {"nl_val_tag", "nl_word_tag64", "nl_word_payload", "wf_bytecode_w_tag", "wf_bytecode_w_int", "wf_bytecode_w_consp", "wf_bytecode_w_nilp"}
MIXED_HEADER = {"nl_cons_car_word": 2, "nl_cons_cdr_word": 2,
                "nl_vector_slot_word": 1, "nl_record_slot_word": 1,
                "nl_cons_car_ptr": 2, "nl_cons_cdr_ptr": 2}


def decode(binary):
    asm = subprocess.check_output(["objdump", "-d", "--no-show-raw-insn", str(binary)], text=True)
    functions, current = {}, None
    for line in asm.splitlines():
        match = re.match(r"([0-9a-f]+) <([^>]+)>:", line)
        if match:
            current = match[2]
            functions[current] = []
        elif current and (match := re.match(r"\s*([0-9a-f]+):\s+(.*)", line)):
            functions[current].append((int(match[1], 16), match[2]))
    addresses, headers, loads = {}, set(), set()
    for name, code in functions.items():
        data_reads = []
        for i, (pc, instruction) in enumerate(code):
            addresses[pc] = name
            # AT&T source memory operands only; stack and RIP metadata reads
            # are not object-header loads. SIMD loads are data reads too.
            if re.match(r"(?:mov\w*|cmp\w*|test\w*)\s+[^,]*\([^)]*\),", instruction) and not re.search(r"\(%r(?:bp|sp|ip)\)", instruction):
                data_reads.append(pc)
                loads.add(pc)
            if re.fullmatch(r"movzbq\s+\(%rdi\),%rax", instruction):
                context = "\n".join(s for _, s in code[max(0, i - 25):i])
                if all(re.search(p, context) for p in (r"cmp\s+\$0x3,%rdi", r"cmp\s+\$0x1,%rax", r"mov\s+\$0x2,%rax")):
                    headers.add(pc)
        if name in PURE_HEADER:
            headers.update(data_reads)
        elif name in MIXED_HEADER:
            headers.update(data_reads[:MIXED_HEADER[name]])
    return addresses, headers, loads


def read_profile(path, binary, decoded):
    addresses, headers, loads = decoded
    objects, current_ob, pc, edge = {}, "", 0, False
    sums = collections.Counter()
    per_function = collections.Counter()
    for line in path.read_text().splitlines():
        if line.startswith("summary:"):
            total = int(line.split()[1])
        elif line.startswith(("ob=", "cob=")):
            value = line.split("=", 1)[1]
            match = re.match(r"\((\d+)\)(?: (.*))?$", value)
            if match:
                if match[2]:
                    objects[match[1]] = match[2]
                value = objects[match[1]]
            if line.startswith("ob="):
                current_ob = value
        elif line.startswith("calls="):
            edge = True
        elif line and (line[0].isdigit() or line[0] in "+-*"):
            fields = line.split()
            if len(fields) != 3:
                raise ValueError(f"unexpected instruction position schema: {line}")
            pos = fields[0]
            if pos != "*":
                value = int(pos, 16) if "0x" in pos else int(pos)
                pc = pc + value if pos[0] in "+-" else value
            cost = int(fields[2])
            if edge:
                edge = False
                continue
            sums["all_self_ir"] += cost
            if current_ob != str(binary):
                continue
            if pc not in addresses:
                sums["unmapped_executable_ir"] += cost
                continue
            name = addresses[pc]
            per_function[name] += cost
            if name in MATERIALISERS:
                sums["materialiser_self_ir"] += cost
            if name.endswith("clone_into") or name == "wf_copy32":
                sums["copy_self_ir"] += cost
            if pc in headers:
                sums["confirmed_header_load_ir_lower_bound"] += cost
            elif pc in loads:
                sums["other_data_load_ir"] += cost
    if sums["all_self_ir"] != total:
        raise ValueError(f"self/edge accounting mismatch: {sums['all_self_ir']} != {total}")
    return dict(total_ir=total, **sums), per_function


def main():
    directory, cand, base = map(lambda p: Path(p).resolve(), sys.argv[1:4])
    decoded = {"cand": decode(cand), "base": decode(base)}
    rows = []
    for mode in ("boot", "files", "regex"):
        for who, binary in (("cand", cand), ("base", base)):
            for layout in (0, 700, 1400, 2100):
                path = directory / f"{mode}-{who}-{layout}.cg"
                if not path.exists() or not path.stat().st_size:
                    continue
                counts, functions = read_profile(path, binary, decoded[who])
                rows.append(dict(mode=mode, who=who, layout=layout, **counts))
                (directory / f"{mode}-{who}-{layout}-functions.json").write_text(json.dumps(functions.most_common(), indent=2) + "\n")
    result = dict(scope="confirmed lower-bound header attribution; other data loads unassigned; self Ir excludes call-edge costs",
                  complete=len(rows) == 24, rows=rows,
                  audited_helper_names=sorted(PURE_HEADER | set(MIXED_HEADER)),
                  materialisers=sorted(MATERIALISERS))
    (directory.parent / "attribution.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps({"profiles": len(rows), "complete": result["complete"]}))


if __name__ == "__main__":
    main()
