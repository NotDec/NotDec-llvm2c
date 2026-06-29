#!/usr/bin/env python3
import argparse
import csv
import subprocess
import sys
import tempfile
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[4]
MAX_FAILURE_DETAIL = 800

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
            "selected-targets-native/lighttpd/executable/module-all.ll"
        ),
        "expected_failure": "lighttpd module-all currently does not finish within the migration timeout",
        "timeout": 90,
        "contains": ["goto structured_block_1;", "goto structured_block_4;",
                      "goto structured_block_6;"],
        "absent": ["goto structured_block_24;\n    goto structured_block_24;"],
    },
    {
        "name": "switch_case_recovery_real",
        "angr_test": "test_reverting_switch_lowering_cksum_digest_print_filename",
        "semantic": "LoweredSwitchSimplifier / real switch recovery sample",
        "input": Path("external/NotDec-llvm2c/test/structuring/fixtures/switch_case_recovery.ll"),
        "contains": [
            "switch (a)",
            "case 1:",
            "case 12:",
            "case 123:",
            "default:",
            "return *(int *)&temp_1;",
        ],
        "absent": ["goto "],
    },
    {
        "name": "lowered_switch_sentinel_proxy",
        "angr_test": "test_reverting_switch_lowering_all_ones_sentinel",
        "semantic": "LoweredSwitchSimplifier all-ones sentinel safety proxy",
        "ir": r"""
define i32 @main(i32 %x) {
entry:
  %cmp_neg1 = icmp eq i32 %x, -1
  br i1 %cmp_neg1, label %case_neg1, label %check7

case_neg1:
  ret i32 -1

check7:
  %cmp7 = icmp eq i32 %x, 7
  br i1 %cmp7, label %case7, label %default

case7:
  ret i32 7

default:
  ret i32 0
}
""",
        "contains": ["return -1;", "return 7;", "return 0;"],
        "absent": ["switch (x)", "case -1:", "case 7:"],
    },
    {
        "name": "lowered_switch_continuous_no_hint_proxy",
        "angr_test": "test_reverting_switch_lowering_continuous_guard",
        "semantic": "LoweredSwitchSimplifier continuous-case safety proxy",
        "ir": r"""
define i32 @main(i32 %x) {
entry:
  %cmp7 = icmp eq i32 %x, 7
  br i1 %cmp7, label %case7, label %check8

case7:
  ret i32 7

check8:
  %cmp8 = icmp eq i32 %x, 8
  br i1 %cmp8, label %case8, label %default

case8:
  ret i32 8

default:
  ret i32 0
}
""",
        "contains": ["return 7;", "return 8;", "return 0;"],
        "absent": ["switch (x)", "case 7:", "case 8:"],
    },
    {
        "name": "lowered_switch_range_compare_proxy",
        "angr_test": "test_reverting_switch_lowering_range_condition_compare",
        "semantic": "LoweredSwitchSimplifier range-compare safety proxy",
        "ir": r"""
define i32 @main(i32 %x) {
entry:
  %cmp7 = icmp sgt i32 %x, 7
  br i1 %cmp7, label %case7, label %check9

case7:
  ret i32 7

check9:
  %cmp9 = icmp sgt i32 %x, 9
  br i1 %cmp9, label %case9, label %default

case9:
  ret i32 9

default:
  ret i32 0
}
""",
        "contains": ["if (x > 7)", "if (x > 9)", "return 7;", "return 9;", "return 0;"],
        "absent": ["switch (x)", "case 7:", "case 9:"],
    },
    {
        "name": "lowered_switch_range_tree_proxy",
        "angr_test": "test_reverting_switch_lowering_range_tree",
        "semantic": "LoweredSwitchSimplifier range-tree recovery proxy",
        "ir": r"""
define i32 @main(i32 %x) {
entry:
  %guard = icmp sle i32 %x, 9
  br i1 %guard, label %low_check7, label %high_check11

low_check7:
  %cmp7 = icmp eq i32 %x, 7
  br i1 %cmp7, label %case7, label %low_check9

low_check9:
  %cmp9 = icmp eq i32 %x, 9
  br i1 %cmp9, label %case9, label %default

high_check11:
  %cmp11 = icmp eq i32 %x, 11
  br i1 %cmp11, label %case11, label %high_check13

high_check13:
  %cmp13 = icmp eq i32 %x, 13
  br i1 %cmp13, label %case13, label %default

case7:
  ret i32 7

case9:
  ret i32 9

case11:
  ret i32 11

case13:
  ret i32 13

default:
  ret i32 0
}
""",
        "contains": [
            "switch (x)",
            "case 7:",
            "case 9:",
            "case 11:",
            "case 13:",
            "default:",
        ],
        "absent": ["if (x <= 9)", "if (x == 7)", "if (x == 11)"],
    },
    {
        "name": "lowered_switch_range_tree_return_var_proxy",
        "angr_test": "test_reverting_switch_lowering_range_tree",
        "semantic": "LoweredSwitchSimplifier range-tree duplicated return-var default proxy",
        "ir": r"""
define i32 @main(i32 %x) {
entry:
  %guard = icmp sle i32 %x, 9
  br i1 %guard, label %low_check7, label %high_check11

low_check7:
  %cmp7 = icmp eq i32 %x, 7
  br i1 %cmp7, label %case7, label %low_check9

low_check9:
  %cmp9 = icmp eq i32 %x, 9
  br i1 %cmp9, label %case9, label %default

high_check11:
  %cmp11 = icmp eq i32 %x, 11
  br i1 %cmp11, label %case11, label %high_check13

high_check13:
  %cmp13 = icmp eq i32 %x, 13
  br i1 %cmp13, label %case13, label %default

case7:
  ret i32 7

case9:
  ret i32 9

case11:
  ret i32 11

case13:
  ret i32 13

default:
  ret i32 %x
}
""",
        "contains": [
            "switch (x)",
            "case 7:",
            "case 9:",
            "case 11:",
            "case 13:",
            "default:",
            "return x;",
        ],
        "absent": ["if (x <= 9)", "if (x == 7)", "if (x == 11)"],
    },
    {
        "name": "fmt_deduplication_proxy",
        "angr_test": "test_fmt_deduplication",
        "semantic": "DuplicationReverter duplicated call proxy",
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
        "contains": ["xdectoumax()", "return 0;"],
        "absent": ["goto left", "goto right"],
        "counts": {"return 0;": 1},
        "body_counts": {"xdectoumax();": 2},
    },
    {
        "name": "branch_common_tail_pipeline_proxy",
        "angr_test": "test_fmt_deduplication",
        "semantic": "DuplicationReverter branch common-tail pipeline proxy",
        "expected_failure": "P1 branch common-tail extraction is covered at pass level, but the full pipeline does not yet produce the required goto hint for this shape",
        "ir": r"""
declare void @a()
declare void @b()
declare void @c()

define i32 @main(i32 %x, i32 %y) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %then, label %else

then:
  call void @a()
  call void @c()
  br label %merge

else:
  call void @b()
  call void @c()
  br label %merge

merge:
  %again = icmp eq i32 %y, 0
  br i1 %again, label %then, label %exit

exit:
  ret i32 0
}
""",
        "contains": ["a();", "b();", "c();", "return 0;"],
        "absent": ["goto structured_block_"],
        "body_counts": {"c();": 2},
    },
    {
        "name": "branch_return_region_proxy",
        "angr_test": "test_decompiling_abnormal_switch_case_within_a_loop_case_1",
        "semantic": "ReturnDuplicatorLow branch return region proxy",
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
        "contains": [
            "switch (x)",
            "case 1:",
            "case 2:",
            "if (a == b)",
            "return 7;",
            "return 8;",
        ],
        "absent": ["goto branch", "goto then", "goto else", "phi", "reg2mem"],
        "counts": {"return 7;": 2, "return 8;": 2},
    },
    {
        "name": "terminal_fork_return_region_proxy",
        "angr_test": "test_decompiling_abnormal_switch_case_within_a_loop_case_1",
        "semantic": "ReturnDuplicatorLow terminal fork return region proxy",
        "ir": r"""
define i32 @f(i32 %x, i32 %a, i32 %b) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

case1:
  br label %fork

case2:
  br label %fork

default:
  ret i32 0

fork:
  %cond = icmp eq i32 %a, %b
  br i1 %cond, label %ret, label %trap

ret:
  ret i32 7

trap:
  unreachable
}
""",
        "contains": ["switch (x)", "case 1:", "case 2:", "if (a == b)",
                     "return 7;", "return 0;"],
        "absent": ["goto fork", "goto ret", "goto trap", "phi", "reg2mem"],
        "counts": {"return 7;": 2},
    },
    {
        "name": "copied_return_tail_dephication_proxy",
        "angr_test": "test_decompiling_abnormal_switch_case_within_a_loop_case_1",
        "semantic": "ReturnDuplicatorLow copied Phi/vvar payload proxy",
        "args": ["--sailr-dephication-mode=angr"],
        "ir": r"""
define i32 @f(i32 %x, i32 %a, i32 %b) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

case1:
  br label %shared_tail

case2:
  br label %shared_tail

default:
  ret i32 0

shared_tail:
  %p = phi i32 [ %a, %case1 ], [ %b, %case2 ]
  %r = add i32 %p, 1
  br label %shared_ret

shared_ret:
  ret i32 %r
}
""",
        "contains": [
            "int p_copy",
            "p_copy1 = a;",
            "r = p_copy1 + 1;",
            "p = b;",
            "r = p + 1;",
            "return r;",
        ],
        "absent": ["phi", "reg2mem", "p_reg2mem"],
        "counts": {"return r;": 2},
    },
    {
        "name": "copied_return_tail_multi_vvar_proxy",
        "angr_test": "test_decompiling_abnormal_switch_case_within_a_loop_case_1",
        "semantic": "ReturnDuplicatorLow copied multi-vvar payload proxy",
        "args": ["--sailr-dephication-mode=angr"],
        "ir": r"""
define i32 @f(i32 %x, i32 %a, i32 %b, i32 %c, i32 %d) {
entry:
  switch i32 %x, label %default [
    i32 1, label %case1
    i32 2, label %case2
  ]

case1:
  br label %shared_tail

case2:
  br label %shared_tail

default:
  ret i32 0

shared_tail:
  %p = phi i32 [ %a, %case1 ], [ %b, %case2 ]
  %q = phi i32 [ %c, %case1 ], [ %d, %case2 ]
  %s = add i32 %p, %q
  br label %shared_ret

shared_ret:
  ret i32 %s
}
""",
        "contains": [
            "int p_copy",
            "int q_copy",
            " = a;",
            " = c;",
            "p = b;",
            "q = d;",
            "return s;",
        ],
        "absent": ["phi", "reg2mem", "p_reg2mem", "q_reg2mem"],
        "counts": {"return s;": 2},
    },
    {
        "name": "lowered_switch_default_cycle_regression",
        "angr_test": "test_megatest_arm64_freebsd",
        "semantic": "LoweredSwitchSimplifier default-cycle safety proxy",
        "ir": r"""
define i32 @main(i32 %x) {
entry:
  %cmp7 = icmp eq i32 %x, 7
  br i1 %cmp7, label %case7, label %check9

case7:
  ret i32 7

check9:
  %cmp9 = icmp eq i32 %x, 9
  br i1 %cmp9, label %case9, label %default

case9:
  ret i32 9

default:
  br label %check9
}
""",
        "contains": ["return 7;", "return 9;"],
        "absent": ["switch (x)", "case 7:", "case 9:"],
    },
    {
        "name": "switch_shared_default_proxy",
        "angr_test": "test_switch_case_header_mismatch_caused_by_cmovs",
        "semantic": "SwitchDefaultCaseDuplicator shared default proxy",
        "ir": r"""
declare void @a()
declare void @b()
declare void @c()
declare void @d()
declare void @e()

define i32 @f(i32 %x, i32 %y) {
entry:
  %choose = icmp eq i32 %y, 0
  br i1 %choose, label %sw0, label %sw1

sw0:
  switch i32 %x, label %shared_default [
    i32 1, label %case1
    i32 3, label %case3
  ]

sw1:
  switch i32 %x, label %shared_default [
    i32 2, label %case2
    i32 4, label %case4
  ]

case1:
  call void @a()
  ret i32 1

case2:
  call void @b()
  ret i32 2

case3:
  call void @d()
  ret i32 3

case4:
  call void @e()
  ret i32 4

shared_default:
  call void @c()
  ret i32 0
}
""",
        "contains": [
            "switch (x)",
            "case 1:",
            "case 2:",
            "case 3:",
            "case 4:",
            "default:",
            "goto structured_block_",
            "c();",
            "return 0;",
        ],
        "counts": {"switch (x)": 2},
    },
    {
        "name": "switch_reused_entry_proxy",
        "angr_test": "test_decompiling_reused_entries_between_switch_cases",
        "semantic": "SwitchReusedEntryRewriter reused case entry proxy",
        "ir": r"""
declare void @a()
declare void @b()
declare void @c()
declare void @d()
declare void @e()

define i32 @f(i32 %x, i32 %y) {
entry:
  %choose = icmp eq i32 %y, 0
  br i1 %choose, label %sw0, label %sw1

sw0:
  switch i32 %x, label %default0 [
    i32 1, label %shared_case
    i32 3, label %case3
  ]

sw1:
  switch i32 %x, label %default1 [
    i32 2, label %shared_case
    i32 4, label %case4
  ]

default0:
  call void @a()
  ret i32 0

default1:
  call void @b()
  ret i32 10

shared_case:
  call void @c()
  ret i32 20

case3:
  call void @d()
  ret i32 3

case4:
  call void @e()
  ret i32 4
}
""",
        "contains": [
            "switch (x)",
            "case 1:",
            "case 2:",
            "case 3:",
            "case 4:",
            "goto structured_block_",
            "c();",
            "return 20;",
        ],
        "counts": {"switch (x)": 2, "return 20;": 1},
    },
    {
        "name": "switch_case_default_overlap_proxy",
        "angr_test": "test_decompiling_abnormal_switch_case_case3",
        "semantic": "Switch default/case overlap output proxy",
        "ir": r"""
declare void @a()
declare void @b()
declare void @c()
declare void @d()

define i32 @f(i32 %sel, i32 %x, i32 %y) {
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
        "name": "condensing_real_lighttpd",
        "angr_test": "test_who_condensing_opt_reversion",
        "semantic": "CrossJumpReverter / real condensing sample",
        "input": Path(
            "/sn640/NotDec-Exp/Bench2/bin2llvm-ir/"
            "selected-targets-native/lighttpd/executable/module-all.ll"
        ),
        "expected_failure": "lighttpd module-all currently does not finish within the migration timeout",
        "timeout": 90,
        "contains": ["goto structured_block_1;", "goto structured_block_4;",
                      "goto structured_block_6;"],
        "absent": ["goto structured_block_24;\n    goto structured_block_24;"],
    },
]


def case_kind(case: dict) -> str:
    return case.get("kind", "real" if "input" in case else "proxy")


def output_metrics(output: str) -> dict:
    return {
        "switch_count": output.count("switch ("),
        "case_count": output.count("case "),
        "goto_count": output.count("goto "),
        "return_count": output.count("return "),
    }


def function_body(output: str) -> str:
    marker = "// ====== Function Definitions ======"
    start = output.find(marker)
    if start == -1:
        return output
    return output[start:]


def classify_failures(status: str, failures: list[str]) -> str:
    if status == "pass":
        return "pass"
    if status == "skip":
        return "missing-input"
    if any("command failed" in failure for failure in failures):
        return "runner-failure"
    if any("unexpected 'goto " in failure for failure in failures):
        return "unexpected-goto"
    if any(
        "missing 'switch (" in failure or "missing 'case " in failure
        for failure in failures
    ):
        return "missing-structure"
    return "output-mismatch"


def summarize_failure(failure: str) -> str:
    failure = failure.replace("\r\n", "\n").replace("\n", r"\n")
    if len(failure) <= MAX_FAILURE_DETAIL:
        return failure
    return failure[:MAX_FAILURE_DETAIL] + "... <truncated>"


def run_case(
    notdec_llvm2c: Path, work_dir: Path, case: dict, run_cache: dict
) -> tuple[str, list[str], dict, str]:
    input_path = case.get("input")
    if input_path is None:
        input_path = work_dir / f"{case['name']}.ll"
        input_path.write_text(case["ir"].strip() + "\n")
    elif not input_path.is_absolute():
        input_path = REPO_ROOT / input_path
    if not input_path.exists():
        failures = [f"{case['name']}: missing input file {input_path}"]
        return "skip", failures, {}, classify_failures("skip", failures)

    cache_key = (
        str(input_path),
        tuple(case.get("args", [])),
        case.get("timeout"),
    )
    if cache_key in run_cache:
        failures, classification = run_cache[cache_key]
        return "fail", [
            f"{case['name']}: reused cached result from same input",
            *list(failures),
        ], {}, classification

    output_path = work_dir / f"{case['name']}.c"
    try:
        proc = subprocess.run(
            [
                str(notdec_llvm2c),
                "--algo=structured-sailr",
                str(input_path),
                "-o",
                str(output_path),
            ] + case.get("args", []),
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            timeout=case.get("timeout"),
        )
    except subprocess.TimeoutExpired as exc:
        output = exc.stdout or ""
        if isinstance(output, bytes):
            output = output.decode(errors="replace")
        failures = [
            f"{case['name']}: command timed out after {case['timeout']}s\n{output}"
        ]
        run_cache[cache_key] = (list(failures), "timeout")
        return "fail", failures, {}, "timeout"
    if proc.returncode != 0:
        failures = [f"{case['name']}: command failed\n{proc.stdout}"]
        classification = classify_failures("fail", failures)
        run_cache[cache_key] = (list(failures), classification)
        return "fail", failures, {}, classification
    if not output_path.exists():
        failures = [f"{case['name']}: missing output file\n{proc.stdout}"]
        classification = classify_failures("fail", failures)
        run_cache[cache_key] = (list(failures), classification)
        return "fail", failures, {}, classification

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
    for needle, expected in case.get("counts", {}).items():
        actual = output.count(needle)
        if actual != expected:
            failures.append(
                f"{header}: expected {expected} x {needle!r}, got {actual}"
            )
    body = function_body(output)
    for needle, expected in case.get("body_counts", {}).items():
        actual = body.count(needle)
        if actual != expected:
            failures.append(
                f"{header}: expected {expected} x {needle!r} in function body, got {actual}"
            )
    status = "fail" if failures else "pass"
    result = (
        status,
        failures,
        output_metrics(output),
        classify_failures(status, failures),
    )
    return result


def write_report(path: Path, rows: list[dict]) -> None:
    fieldnames = [
        "name",
        "kind",
        "angr_test",
        "semantic",
        "status",
        "classification",
        "switch_count",
        "case_count",
        "goto_count",
        "return_count",
        "failures",
    ]
    with path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--notdec-llvm2c", required=True, type=Path)
    parser.add_argument("--report-csv", type=Path)
    args = parser.parse_args()

    failures = []
    rows = []
    with tempfile.TemporaryDirectory(prefix="notdec-sailr-bench2-") as tmp:
        work_dir = Path(tmp)
        run_cache = {}
        for case in CASES:
            status, case_failures, metrics, classification = run_case(
                args.notdec_llvm2c, work_dir, case, run_cache
            )
            if case.get("expected_failure"):
                if status == "fail":
                    status = "xfail"
                    classification = "expected-" + classification
                    case_failures.append(
                        "expected failure: " + case["expected_failure"]
                    )
                elif status == "pass":
                    status = "xpass"
                    classification = "unexpected-pass"
                    case_failures.append(
                        "unexpected pass: " + case["expected_failure"]
                    )
            if status in {"fail", "xpass"}:
                failures.extend(case_failures)
            rows.append({
                "name": case["name"],
                "kind": case_kind(case),
                "angr_test": case["angr_test"],
                "semantic": case["semantic"],
                "status": status,
                "classification": classification,
                "switch_count": metrics.get("switch_count", ""),
                "case_count": metrics.get("case_count", ""),
                "goto_count": metrics.get("goto_count", ""),
                "return_count": metrics.get("return_count", ""),
                "failures": " | ".join(
                    summarize_failure(failure) for failure in case_failures
                ),
            })

    if args.report_csv is not None:
        write_report(args.report_csv, rows)

    if failures:
        print("\n".join(failures))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
