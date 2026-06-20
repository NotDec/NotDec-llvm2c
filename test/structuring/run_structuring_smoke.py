#!/usr/bin/env python3
import argparse
import subprocess
import sys
import tempfile
from pathlib import Path


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
        "absent": ["goto head"],
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
        "absent": ["goto head"],
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
        "contains": ["while (1)", "continue;", "goto exit"],
        "counts": {"continue;": 2},
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
]


def run_case(notdec_llvm2c: Path, work_dir: Path, case: dict) -> list[str]:
    input_path = work_dir / f"{case['name']}.ll"
    output_path = work_dir / f"{case['name']}.c"
    input_path.write_text(case["ir"].strip() + "\n")

    proc = subprocess.run(
        [
            str(notdec_llvm2c),
            str(input_path),
            "-o",
            str(output_path),
            "--algo=structured-sailr",
        ],
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
    return failures


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
    if "break;" in phoenix_output:
        failures.append(
            "structured-phoenix: unexpectedly used SAILR improved break schema")
    return failures


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--notdec-llvm2c", required=True, type=Path)
    args = parser.parse_args()

    all_failures = []
    with tempfile.TemporaryDirectory(prefix="notdec-structuring-") as tmp:
        work_dir = Path(tmp)
        for case in CASES:
            all_failures.extend(run_case(args.notdec_llvm2c, work_dir, case))
        all_failures.extend(
            run_sailr_improved_phoenix_case(args.notdec_llvm2c, work_dir))

    if all_failures:
        print("\n".join(all_failures))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
