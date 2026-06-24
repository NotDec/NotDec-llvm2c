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
]


def run_case(notdec_llvm2c: Path, work_dir: Path, case: dict) -> list[str]:
    output_path = work_dir / f"{case['name']}.c"
    proc = subprocess.run(
        [
            str(notdec_llvm2c),
            "--algo=structured-sailr",
            str(case["input"]),
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
