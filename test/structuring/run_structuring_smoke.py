#!/usr/bin/env python3
import argparse
import subprocess
import sys
import tempfile
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[4]

CASES = [
    {
        "name": "linear_while",
        "ir": r"""
declare void @a()
declare void @b()

define i32 @main(i32 %x) {
entry:
  br label %head

head:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %body, label %exit

body:
  call void @a()
  call void @b()
  br label %head

exit:
  ret i32 0
}
""",
        "contains": ["while (x == 0)", "return 0;"],
        "counts": {"break;": 0},
    },
    {
        "name": "linear_while_break",
        "ir": r"""
declare void @a()
declare void @b()

define i32 @main(i32 %x, ptr %y) {
entry:
  br label %head

head:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %a, label %exit

a:
  call void @a()
  %isnull = icmp eq ptr %y, null
  br i1 %isnull, label %exit, label %b

b:
  call void @b()
  br label %head

exit:
  ret i32 0
}
""",
        "contains": ["while (x == 0)", "break;", "return 0;"],
        "counts": {"break;": 1},
    },
    {
        "name": "multi_exit_loop_fallback",
        "ir": r"""
declare void @a()
declare void @b()
declare void @c()
declare void @d()

define i32 @main(i32 %x, ptr %y, i32 %z) {
entry:
  br label %head

head:
  %h = icmp eq i32 %x, 0
  br i1 %h, label %a, label %exit

a:
  call void @a()
  %c1 = icmp eq ptr %y, null
  br i1 %c1, label %b, label %c

b:
  call void @b()
  br label %head

c:
  call void @c()
  %c2 = icmp eq i32 %z, 0
  br i1 %c2, label %exit, label %d

d:
  call void @d()
  br label %head

exit:
  ret i32 0
}
""",
        "contains": ["while (x == 0)", "continue;", "break;"],
        "absent": ["goto exit"],
        "counts": {"continue;": 2, "break;": 1},
        "ordered": [("c();", "return 0;")],
    },
    {
        "name": "simple_switch",
        "ir": r"""
declare void @a()
declare void @b()
declare void @c()

define i32 @main(i32 %x) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

default:
  call void @a()
  br label %exit

case1:
  call void @b()
  br label %exit

case2:
  call void @c()
  br label %exit

exit:
  ret i32 0
}
""",
        "contains": ["switch (x)", "case 1:", "case 2:", "return 0;"],
    },
    {
        "name": "fmt_deduplication_like",
        "ir": r"""
declare void @xdectoumax()

define i32 @main(i32 %x) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %left, label %right

left:
  call void @xdectoumax()
  br label %merge

right:
  call void @xdectoumax()
  br label %merge

merge:
  ret i32 0
}
""",
        "contains": ["xdectoumax()", "xdectoumax()", "return 0;"],
        "absent": ["goto left", "goto right"],
    },
    {
        "name": "real_switch_fixture",
        "input": Path("external/NotDec-llvm2c/test/structuring/fixtures/switch_case_recovery.ll"),
        "contains": ["switch (a)", "case 1:", "case 12:", "case 123:",
                      "default:", "return *(int *)&temp_1;"],
        "absent": ["goto ", "case 7:"],
    },
    {
        "name": "real_condensing_fixture",
        "input": Path(
            "/sn640/NotDec-Exp/Bench2/bin2llvm-ir/"
            "lighttpd/1-main_init_once.ll"
        ),
        "contains": ["goto structured_block_1;", "goto structured_block_4;",
                      "goto structured_block_6;"],
        "absent": ["goto structured_block_24;\n    goto structured_block_24;"],
    },
    {
        "name": "shared_synthetic_goto_switch_reuse",
        "ir": r"""
declare void @a()
declare void @b()
declare void @c()

define i32 @main(i32 %x, i32 %y) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

default:
  call void @a()
  br label %merge

case1:
  call void @b()
  br label %merge

case2:
  call void @c()
  br label %merge

merge:
  %cond = icmp eq i32 %y, 0
  br i1 %cond, label %default, label %exit

exit:
  ret i32 0
}
""",
        "contains": ["goto structured_block_1;", "goto structured_block_2;",
                      "goto structured_block_3;"],
        "absent": ["unknown"],
    },
    {
        "name": "switch_before_sequence",
        "ir": r"""
declare void @a()
declare void @b()

define i32 @main(i32 %x) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

default:
  call void @a()
  br label %exit

case1:
  call void @b()
  br label %exit

case2:
  call void @b()
  br label %exit

exit:
  ret i32 0
}
""",
        "contains": ["switch (x)", "case 1:", "case 2:", "return 0;"],
        "absent": ["goto case1", "goto case2", "goto default"],
    },
    {
        "name": "early_return_if",
        "ir": r"""
declare void @a()

define i32 @main(i32 %x) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %ret, label %cont

ret:
  ret i32 1

cont:
  call void @a()
  ret i32 0
}
""",
        "contains": ["if (x == 0)", "return 1;", "return 0;"],
        "absent": ["goto ret", "goto cont"],
    },
    {
        "name": "sailr_angr_dephication_phi",
        "ir": r"""
define i32 @f(i1 %c, i32 %a, i32 %b) {
entry:
  br i1 %c, label %then, label %else

then:
  br label %merge

else:
  br label %merge

merge:
  %x = phi i32 [ %a, %then ], [ %b, %else ]
  ret i32 %x
}
""",
        "args": ["--sailr-dephication-mode=angr"],
        "contains": ["int x;", "x = a;", "x = b;", "return x;"],
        "absent": ["phi", "reg2mem"],
    },
    {
        "name": "sailr_angr_dephication_multi_phi_same_edge",
        "ir": r"""
define i32 @f(i1 %c, i32 %a, i32 %b, i32 %c0, i32 %d) {
entry:
  br i1 %c, label %then, label %else

then:
  br label %merge

else:
  br label %merge

merge:
  %x = phi i32 [ %a, %then ], [ %b, %else ]
  %y = phi i32 [ %c0, %then ], [ %d, %else ]
  %r = add i32 %x, %y
  ret i32 %r
}
""",
        "args": ["--sailr-dephication-mode=angr"],
        "contains": [
            "int x;",
            "int y;",
            "x = a;",
            "y = c0;",
            "x = b;",
            "y = d;",
            "return x + y;",
        ],
        "absent": ["phi", "reg2mem"],
    },
    {
        "name": "sailr_angr_dephication_copied_switch_region",
        "ir": r"""
define i32 @f(i32 %x, i32 %a, i32 %b) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

case1:
  br label %shared

case2:
  br label %shared

default:
  ret i32 0

shared:
  %p = phi i32 [ %a, %case1 ], [ %b, %case2 ]
  %r = add i32 %p, 1
  ret i32 %r
}
""",
        "args": ["--sailr-dephication-mode=angr"],
        "contains": [
            "int p_copy",
            "p_copy1 = a;",
            "return p_copy1 + 1;",
            "p = b;",
            "return p + 1;",
        ],
        "absent": ["phi", "reg2mem"],
    },
    {
        "name": "sailr_angr_dephication_copied_return_end",
        "ir": r"""
define i32 @f(i32 %x, i32 %a, i32 %b) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

case1:
  br label %shared

case2:
  br label %shared

default:
  ret i32 0

shared:
  %p = phi i32 [ %a, %case1 ], [ %b, %case2 ]
  ret i32 %p
}
""",
        "args": ["--sailr-dephication-mode=angr"],
        "contains": [
            "int p_copy",
            "p_copy1 = a;",
            "return p_copy1;",
            "p = b;",
            "return p;",
        ],
        "absent": ["phi", "reg2mem"],
    },
    {
        "name": "sailr_prefixed_diamond_return_region",
        "ir": r"""
define i32 @f(i32 %x, i32 %a, i32 %b) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

case1:
  br label %prefix

case2:
  br label %prefix

default:
  ret i32 0

prefix:
  %sum = add i32 %a, 1
  %cond = icmp eq i32 %sum, %b
  br i1 %cond, label %left, label %right

left:
  br label %ret

right:
  br label %ret

ret:
  ret i32 7
}
""",
        "contains": ["switch (x)", "case 1:", "case 2:", "a + 1 == b;",
                     "return 7;"],
        "absent": ["goto prefix", "goto ret", "phi", "reg2mem"],
        "counts": {"return 7;": 2},
    },
    {
        "name": "sailr_direct_diamond_return_region",
        "ir": r"""
define i32 @f(i32 %x, i32 %a, i32 %b) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

case1:
  br label %head

case2:
  br label %head

default:
  ret i32 0

head:
  %cond = icmp eq i32 %a, %b
  br i1 %cond, label %ret, label %tail

tail:
  %sum = add i32 %a, 1
  br label %ret

ret:
  ret i32 7
}
""",
        "contains": ["switch (x)", "case 1:", "case 2:", "if (a == b)",
                     "a + 1;", "return 7;"],
        "absent": ["goto head", "goto tail", "goto ret", "phi", "reg2mem"],
        "counts": {"return 7;": 4},
    },
    {
        "name": "sailr_prefixed_switch_return_region",
        "ir": r"""
define i32 @f(i32 %x, i32 %a) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

case1:
  br label %prefix

case2:
  br label %prefix

default:
  ret i32 0

prefix:
  %sel = add i32 %a, 1
  switch i32 %sel, label %inner_default [
    i32 10, label %inner_case
    i32 11, label %inner_case2
  ]

inner_default:
  ret i32 7

inner_case:
  ret i32 8

inner_case2:
  ret i32 9
}
""",
        "contains": ["switch (x)", "case 1:", "case 2:", "switch (a + 1)",
                     "case 10:", "case 11:", "return 7;", "return 8;",
                     "return 9;"],
        "absent": ["goto prefix", "goto inner_default", "goto inner_case",
                   "phi", "reg2mem"],
        "counts": {"return 7;": 2, "return 8;": 2, "return 9;": 2},
    },
    {
        "name": "sailr_branch_return_region",
        "ir": r"""
define i32 @f(i32 %x, i32 %a, i32 %b) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

case1:
  br label %branch

case2:
  br label %branch

default:
  ret i32 0

branch:
  %cond = icmp eq i32 %a, %b
  br i1 %cond, label %then, label %else

then:
  ret i32 7

else:
  ret i32 8
}
""",
        "contains": ["switch (x)", "case 1:", "case 2:", "if (a == b)",
                     "return 7;", "return 8;"],
        "absent": ["goto branch", "goto then", "goto else", "phi", "reg2mem"],
        "counts": {"return 7;": 2, "return 8;": 2},
    },
    {
        "name": "sailr_branch_diamond_return_tail",
        "ir": r"""
define i32 @f(i32 %x, i32 %a, i32 %b) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

case1:
  br label %outer

case2:
  br label %outer

default:
  ret i32 0

outer:
  %outer_cond = icmp eq i32 %a, %b
  br i1 %outer_cond, label %plain_tail, label %diamond_head

plain_tail:
  %plain = add i32 %a, 1
  br label %plain_ret

plain_ret:
  ret i32 7

diamond_head:
  %diamond_cond = icmp sgt i32 %a, %b
  br i1 %diamond_cond, label %left, label %right

left:
  %left_sum = add i32 %a, 2
  br label %left_join

right:
  %right_sum = add i32 %b, 2
  br label %right_join

left_join:
  %left_more = add i32 %a, 3
  br label %diamond_ret

right_join:
  %right_more = add i32 %b, 3
  br label %diamond_ret

diamond_ret:
  ret i32 9
}
""",
        "contains": ["switch (x)", "case 1:", "case 2:", "if (a == b)",
                     "if (a > b)", "return 7;", "return 9;"],
        "absent": ["goto outer", "goto plain_tail", "goto plain_ret",
                   "goto diamond_head", "goto diamond_ret", "phi", "reg2mem"],
        "counts": {"return 7;": 2, "return 9;": 4},
    },
    {
        "name": "sailr_switch_diamond_return_tail",
        "ir": r"""
define i32 @f(i32 %x, i32 %a, i32 %b) {
entry:
  switch i32 %x, label %outer_default [
    i32 1, label %case1
    i32 2, label %case2
  ]

case1:
  br label %inner_switch

case2:
  br label %inner_switch

outer_default:
  ret i32 0

inner_switch:
  switch i32 %a, label %plain_tail [
    i32 10, label %diamond_head
    i32 11, label %case_tail
  ]

plain_tail:
  %plain = add i32 %a, 1
  br label %plain_ret

plain_ret:
  ret i32 7

case_tail:
  %case_add = add i32 %b, 1
  br label %case_ret

case_ret:
  ret i32 8

diamond_head:
  %diamond_cond = icmp sgt i32 %a, %b
  br i1 %diamond_cond, label %left, label %right

left:
  %left_sum = add i32 %a, 2
  br label %left_join

right:
  %right_sum = add i32 %b, 2
  br label %right_join

left_join:
  %left_more = add i32 %a, 3
  br label %diamond_ret

right_join:
  %right_more = add i32 %b, 3
  br label %diamond_ret

diamond_ret:
  ret i32 9
}
""",
        "contains": ["switch (x)", "case 1:", "case 2:", "switch (a)",
                     "case 10:", "case 11:", "if (a > b)", "return 7;",
                     "return 8;", "return 9;"],
        "absent": ["goto inner_switch", "goto plain_tail", "goto plain_ret",
                   "goto case_tail", "goto case_ret", "goto diamond_head",
                   "goto diamond_ret", "phi", "reg2mem"],
        "counts": {"switch (a)": 2, "return 7;": 2, "return 8;": 2,
                   "return 9;": 4},
    },
    {
        "name": "sailr_switch_case_default_overlap_body_once",
        "ir": r"""
declare void @a()
declare void @b()
declare void @c()
declare void @d()

define i32 @main(i32 %sel, i32 %x, i32 %y) {
entry:
  %choose = icmp eq i32 %sel, 0
  br i1 %choose, label %xsw, label %ysw

xsw:
  switch i32 %x, label %xdefault [
    i32 1, label %shared
    i32 3, label %xcase3
  ]

xdefault:
  call void @a()
  ret i32 10

xcase3:
  call void @c()
  ret i32 30

ysw:
  switch i32 %y, label %shared [
    i32 2, label %shared
    i32 4, label %ycase4
  ]

ycase4:
  call void @d()
  ret i32 40

shared:
  call void @b()
  ret i32 0
}
""",
        "contains": [
            "switch (y)",
            "case 2:",
            "default:",
            "b();",
            "return 0;",
            "goto structured_block_5;",
        ],
        "counts": {"structured_block_5:": 1},
    },
    {
        "name": "phi_demote_before_structuring",
        "ir": r"""
define i32 @main(i32 %x) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %then, label %else

then:
  br label %merge

else:
  br label %merge

merge:
  %v = phi i32 [ 1, %then ], [ 0, %else ]
  ret i32 %v
}
""",
        "contains": ["if (x == 0)", "return 1;", "return 0;"],
        "absent": ["phi"],
    },
    {
        "name": "phi_demote_before_structuring_htype",
        "input": Path("test/type-recovery/llvm-ir/cases/09_OffsetLoop.ll"),
        "contains": ["while", "break;", "return"],
        "absent": ["phi"],
    },
    {
        "name": "phi_demote_before_structuring_htype_summary",
        "input": Path("test/type-recovery/llvm-ir/cases/09_OffsetLoop.ll"),
        "contains": ["while", "break;", "return"],
        "absent": ["phi", "notdec.phi"],
    },
    {
        "name": "linear_do_while",
        "ir": r"""
declare void @a()
declare void @b()

define i32 @main(i32 %x) {
entry:
  br label %body1

body1:
  call void @a()
  br label %body2

body2:
  call void @b()
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %body1, label %exit

exit:
  ret i32 0
}
""",
        "contains": ["do {", "while (x == 0);", "return 0;"],
        "absent": ["while (1)"],
    },
    {
        "name": "self_loop_while",
        "ir": r"""
define i32 @main(i32 %x) {
entry:
  br label %head

head:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %head, label %exit

exit:
  ret i32 0
}
""",
        "contains": ["while (x == 0)", "return 0;"],
        "absent": ["do {"],
    },
    {
        "name": "root_cycle_follow",
        "ir": r"""
declare void @a()
declare void @b()
declare void @c()

define i32 @main(i32 %x, i32 %y) {
entry:
  br label %head

head:
  %c0 = icmp eq i32 %x, 0
  br i1 %c0, label %a, label %b

a:
  call void @a()
  br label %c

b:
  call void @b()
  br label %c

c:
  call void @c()
  %c1 = icmp eq i32 %y, 0
  br i1 %c1, label %head, label %exit

exit:
  ret i32 0
}
""",
        "contains": ["do {", "c();", "while (y == (char **)0);", "return 0;"],
        "absent": ["goto head", "goto c;"],
    },
]


def run_case(notdec_llvm2c: Path, work_dir: Path, case: dict) -> list[str]:
    output_path = work_dir / f"{case['name']}.c"
    input_path = case.get("input")
    if input_path is None:
        input_path = work_dir / f"{case['name']}.ll"
        input_path.write_text(case["ir"].strip() + "\n")
    elif not input_path.is_absolute():
        input_path = REPO_ROOT / input_path

    proc = subprocess.run(
        [
            str(notdec_llvm2c),
            str(input_path),
            "-o",
            str(output_path),
            "--algo=structured-sailr",
        ] + case.get("args", []),
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    failures = []
    if proc.returncode != 0:
        return [f"{case['name']}: command failed\n{proc.stdout}"]

    output = output_path.read_text()
    for needle in case.get("contains", []):
        if needle not in output:
            failures.append(f"{case['name']}: missing {needle!r}")
    for needle in case.get("absent", []):
        if needle in output:
            failures.append(f"{case['name']}: unexpected {needle!r}")
    for needle, expected in case.get("counts", {}).items():
        actual = output.count(needle)
        if actual != expected:
            failures.append(
                f"{case['name']}: expected {expected} x {needle!r}, got {actual}"
            )
    for before, after in case.get("ordered", []):
        before_index = output.find(before)
        after_index = output.find(after)
        if before_index == -1 or after_index == -1:
            continue
        if before_index >= after_index:
            failures.append(
                f"{case['name']}: expected {before!r} before {after!r}"
            )
    return failures


def resolve_notdec_llvm2c(exe: Path) -> Path:
    if exe.exists():
        return exe
    fallback = REPO_ROOT / "build/external/NotDec-llvm2c/bin/notdec-llvm2c"
    if fallback.exists():
        return fallback
    return exe


def run_sailr_improved_phoenix_case(notdec_llvm2c: Path,
                                    work_dir: Path) -> list[str]:
    case = next(c for c in CASES if c["name"] == "linear_while_break")
    input_path = work_dir / "sailr_improved_phoenix.ll"
    input_path.write_text(case["ir"].strip() + "\n")

    failures = []
    outputs = {}
    for algo in ("structured-sailr", "structured-phoenix"):
        output_path = work_dir / f"sailr_improved_phoenix.{algo}.c"
        proc = subprocess.run(
            [str(notdec_llvm2c), str(input_path), "-o", str(output_path),
             f"--algo={algo}"],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )
        if proc.returncode != 0:
            failures.append(f"{algo}: command failed\n{proc.stdout}")
            continue
        outputs[algo] = output_path.read_text()

    sailr_output = outputs.get("structured-sailr", "")
    phoenix_output = outputs.get("structured-phoenix", "")
    if "break;" not in sailr_output:
        failures.append("structured-sailr: missing improved break output")
    return failures


def run_sailr_dephication_mode_contrast_case(
    notdec_llvm2c: Path, work_dir: Path) -> list[str]:
    case = next(c for c in CASES if c["name"] == "sailr_angr_dephication_phi")
    input_path = work_dir / "sailr_dephication_mode_contrast.ll"
    input_path.write_text(case["ir"].strip() + "\n")

    failures = []
    outputs = {}
    for mode in ("legacy", "angr"):
        output_path = work_dir / f"sailr_dephication_mode_contrast.{mode}.c"
        proc = subprocess.run(
            [
                str(notdec_llvm2c),
                str(input_path),
                "-o",
                str(output_path),
                "--algo=structured-sailr",
                f"--sailr-dephication-mode={mode}",
            ],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )
        if proc.returncode != 0:
            failures.append(f"{mode}: command failed\n{proc.stdout}")
            continue
        outputs[mode] = output_path.read_text()

    legacy_output = outputs.get("legacy", "")
    angr_output = outputs.get("angr", "")
    if "int x;" in legacy_output:
        failures.append("legacy: unexpected copied vvar declaration")
    if "x = a;" in legacy_output or "x = b;" in legacy_output:
        failures.append("legacy: unexpected shared dephication payloads")
    if "return a;" not in legacy_output or "return b;" not in legacy_output:
        failures.append("legacy: missing direct branch returns")

    if "int x;" not in angr_output:
        failures.append("angr: missing copied vvar declaration")
    if "x = a;" not in angr_output or "x = b;" not in angr_output:
        failures.append("angr: missing shared dephication payloads")
    if "return x;" not in angr_output:
        failures.append("angr: missing merged return")
    if "reg2mem" in angr_output:
        failures.append("angr: reg2mem fallback leaked into output")

    return failures


def run_sailr_dephication_copied_switch_contrast_case(
    notdec_llvm2c: Path, work_dir: Path) -> list[str]:
    case = next(c for c in CASES if c["name"] ==
                "sailr_angr_dephication_copied_switch_region")
    input_path = work_dir / "sailr_dephication_copied_switch_contrast.ll"
    input_path.write_text(case["ir"].strip() + "\n")

    failures = []
    outputs = {}
    for mode in ("legacy", "angr"):
        output_path = work_dir / (
            f"sailr_dephication_copied_switch_contrast.{mode}.c")
        proc = subprocess.run(
            [
                str(notdec_llvm2c),
                str(input_path),
                "-o",
                str(output_path),
                "--algo=structured-sailr",
                f"--sailr-dephication-mode={mode}",
            ],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )
        if proc.returncode != 0:
            failures.append(f"{mode}: command failed\n{proc.stdout}")
            continue
        outputs[mode] = output_path.read_text()

    legacy_output = outputs.get("legacy", "")
    angr_output = outputs.get("angr", "")
    if "p_reg2mem" not in legacy_output:
        failures.append("legacy: missing reg2mem copied switch output")
    if "int p_copy" in legacy_output or "p_copy1" in legacy_output:
        failures.append("legacy: unexpected copied vvar declaration")

    if "int p_copy" not in angr_output and "int p_copy1" not in angr_output:
        failures.append("angr: missing copied vvar declaration")
    if "p_copy1 = a;" not in angr_output and "p_copy1 = b;" not in angr_output:
        failures.append("angr: missing copied switch payloads")
    if "return p_copy1 + 1;" not in angr_output:
        failures.append("angr: missing copied return payload")
    if "int p;" not in angr_output:
        failures.append("angr: missing shared branch declaration")
    if "p_reg2mem" in angr_output:
        failures.append("angr: reg2mem fallback leaked into output")

    return failures


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--notdec-llvm2c", required=True, type=Path)
    args = parser.parse_args()
    notdec_llvm2c = resolve_notdec_llvm2c(args.notdec_llvm2c)

    all_failures = []
    with tempfile.TemporaryDirectory(prefix="notdec-structuring-") as tmp:
        work_dir = Path(tmp)
        for case in CASES:
            all_failures.extend(run_case(notdec_llvm2c, work_dir, case))
        all_failures.extend(
            run_sailr_improved_phoenix_case(notdec_llvm2c, work_dir))
        all_failures.extend(
            run_sailr_dephication_mode_contrast_case(notdec_llvm2c, work_dir))
        all_failures.extend(
            run_sailr_dephication_copied_switch_contrast_case(
                notdec_llvm2c, work_dir))

    if all_failures:
        print("\n".join(all_failures))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
