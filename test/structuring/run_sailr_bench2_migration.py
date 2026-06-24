#!/usr/bin/env python3
import argparse
import subprocess
import sys
import tempfile
from pathlib import Path


CASES = [
    {
        "name": "return_tail_cleanup",
        "angr_test": "test_sailr_motivating_example",
        "semantic": "ReturnDuplicatorLow tail cleanup",
        "input": Path(
            "/sn640/NotDec-Exp/Bench2/bin2llvm-ir/"
            "hexx64/function-0x1156e0/native/function-0x1156e0.ll"
        ),
        "contains": ["return;"],
        "absent": ["goto "],
    },
    {
        "name": "early_exit_chain",
        "angr_test": "test_decompiling_sha384sum_digest_bsd_split_3",
        "semantic": "ReturnDuplicatorLow early-exit chain",
        "input": Path(
            "/sn640/NotDec-Exp/Bench2/bin2llvm-ir/"
            "python/one-_PyPegen_fill_token.cold.ll"
        ),
        "contains": ["return -1;"],
        "absent": ["goto "],
    },
    {
        "name": "goto_condensing_chain",
        "angr_test": "test_who_condensing_opt_reversion",
        "semantic": "CrossJumpReverter / condensing",
        "input": Path(
            "/sn640/NotDec-Exp/Bench2/bin2llvm-ir/"
            "lighttpd/1-main_init_once.ll"
        ),
        "contains": ["goto structured_block_1;", "goto structured_block_4;",
                      "goto structured_block_6;"],
        "absent": ["goto structured_block_24;\n    goto structured_block_24;"],
    },
    {
        "name": "switch_reuse_proxy",
        "angr_test": "test_decompiling_sha384sum_digest_bsd_split_3",
        "semantic": "LoweredSwitchSimplifier / switch reuse proxy",
        "ir": r"""
define i32 @main(i32 %x) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
    i32 3, label %case3
  ]

default:
  br label %exit

case1:
  br label %shared

case2:
  br label %shared

case3:
  br label %mid

mid:
  br label %shared

shared:
  br label %exit

exit:
  ret i32 0
}
""",
        "contains": ["switch (x)", "case 1:", "case 2:", "case 3:", "return 0;"],
        "absent": ["goto case1", "goto case2", "goto case3"],
    },
    {
        "name": "duplication_reverter_proxy",
        "angr_test": "test_true_a_graph_deduplication",
        "semantic": "DuplicationReverter / duplicated tail proxy",
        "ir": r"""
declare i32 @malloc(i64)

define i32 @main(i32 %x) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %dup1, label %dup2

dup1:
  %a = call i32 @malloc(i64 8)
  br label %tail

dup2:
  %b = call i32 @malloc(i64 8)
  br label %tail

tail:
  ret i32 0
}
""",
        "contains": ["malloc", "return 0;"],
        "absent": ["goto dup1", "goto dup2"],
    },
    {
        "name": "duplication_too_sensitive_proxy",
        "angr_test": "test_deduplication_too_sensitive_split_3",
        "semantic": "DuplicationReverter / keep-legit-duplicate proxy",
        "ir": r"""
define i32 @main(i32 %x) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %then, label %else

then:
  %a = add i32 %x, 1
  br label %merge

else:
  %b = add i32 %x, 1
  br label %merge

merge:
  %c = phi i32 [ %a, %then ], [ %b, %else ]
  ret i32 %c
}
""",
        "contains": ["return x + 1;"],
        "absent": ["malloc", "goto then", "goto else"],
    },
]


def run_case(notdec_llvm2c: Path, work_dir: Path, case: dict) -> list[str]:
    input_path = case.get("input")
    if input_path is None:
        input_path = work_dir / f"{case['name']}.ll"
        input_path.write_text(case["ir"].strip() + "\n")
    output_path = work_dir / f"{case['name']}.c"
    proc = subprocess.run(
        [
            str(notdec_llvm2c),
            "--algo=structured-sailr",
            str(input_path),
            "-o",
            str(output_path),
        ],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    if proc.returncode != 0:
        return [f"{case['name']}: command failed\n{proc.stdout}"]

    output = output_path.read_text()
    failures = []
    header = f"{case['name']} [{case['angr_test']}] ({case['semantic']})"
    if not output:
        failures.append(f"{header}: empty output")
    for needle in case.get("contains", []):
        if needle not in output:
            failures.append(f"{header}: missing {needle!r}")
    for needle in case.get("absent", []):
        if needle in output:
            failures.append(f"{header}: unexpected {needle!r}")
    return failures


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--notdec-llvm2c", required=True, type=Path)
    args = parser.parse_args()

    failures = []
    with tempfile.TemporaryDirectory(prefix="notdec-sailr-bench2-") as tmp:
        work_dir = Path(tmp)
        for case in CASES:
            failures.extend(run_case(args.notdec_llvm2c, work_dir, case))

    if failures:
        print("\n".join(failures))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
