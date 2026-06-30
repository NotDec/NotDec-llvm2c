#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[5]
SUITE_ROOT = Path(__file__).resolve().parent
MAX_OUTPUT_DETAIL = 1200


def format_command(command: list[str]) -> str:
    return " ".join(subprocess.list2cmdline([part]) for part in command)


def resolve_path(base: Path, value: str) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return (base / path).resolve()


def default_clang() -> Path:
    env = os.environ.get("NOTDEC_SOURCE_CASES_CLANG")
    if env:
        return Path(env)
    local = REPO_ROOT / "llvm-22.1.0.obj/bin/clang"
    if local.exists():
        return local
    return Path("clang")


def count_metric(output: str, metric: str) -> int:
    if metric == "goto":
        return len(re.findall(r"\bgoto\b", output))
    if metric == "break":
        return output.count("break;")
    if metric == "continue":
        return output.count("continue;")
    if metric == "switch":
        return len(re.findall(r"\bswitch\s*\(", output))
    if metric == "while":
        return len(re.findall(r"\bwhile\s*\(", output))
    if metric == "do":
        return len(re.findall(r"\bdo\s*\{", output))
    raise ValueError(f"unsupported metric: {metric}")


def merge_oracle(defaults: dict, local: dict) -> dict:
    # Global oracle holds suite-wide bad CFG shapes. Case oracle only adds or
    # narrows expectations for one source pattern.
    merged: dict = {}
    for source in (defaults, local):
        for key, value in source.items():
            if isinstance(value, list):
                merged.setdefault(key, []).extend(value)
            elif isinstance(value, dict):
                merged.setdefault(key, {}).update(value)
            else:
                merged[key] = value
    return merged


def check_oracle(case_name: str, oracle: dict, output: str) -> list[str]:
    failures: list[str] = []

    for needle in oracle.get("contains", []):
        if needle not in output:
            failures.append(f"{case_name}: missing {needle!r}")
    for needle in oracle.get("absent", []):
        if needle in output:
            failures.append(f"{case_name}: unexpected {needle!r}")
    for pattern in oracle.get("regex_contains", []):
        if re.search(pattern, output, re.MULTILINE) is None:
            failures.append(f"{case_name}: missing pattern {pattern!r}")
    for pattern in oracle.get("regex_absent", []):
        if re.search(pattern, output, re.MULTILINE) is not None:
            failures.append(f"{case_name}: unexpected pattern {pattern!r}")

    for metric, expected in oracle.get("counts", {}).items():
        actual = count_metric(output, metric)
        if actual != expected:
            failures.append(
                f"{case_name}: expected {metric}={expected}, got {actual}"
            )
    for metric, limit in oracle.get("max_counts", {}).items():
        actual = count_metric(output, metric)
        if actual > limit:
            failures.append(
                f"{case_name}: expected {metric}<={limit}, got {actual}"
            )
    for metric, limit in oracle.get("min_counts", {}).items():
        actual = count_metric(output, metric)
        if actual < limit:
            failures.append(
                f"{case_name}: expected {metric}>={limit}, got {actual}"
            )

    for before, after in oracle.get("ordered", []):
        before_index = output.find(before)
        if before_index < 0:
            failures.append(f"{case_name}: missing ordered marker {before!r}")
            continue
        after_index = output.find(after, before_index + len(before))
        if after_index < 0:
            failures.append(
                f"{case_name}: expected {after!r} after {before!r}"
            )

    return failures


def run_process(command: list[str], cwd: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )


def run_case(
    *,
    case: dict,
    manifest: dict,
    suite_root: Path,
    work_root: Path,
    clang: Path,
    notdec_llvm2c: Path,
) -> list[str]:
    name = case["name"]
    case_work = work_root / name
    case_work.mkdir(parents=True, exist_ok=True)

    source = resolve_path(suite_root, case["source"])
    opt_levels = case.get("opts", manifest.get("opts", ["O2"]))
    algorithms = case.get("algorithms", manifest.get("algorithms", ["structured-sailr"]))
    common_clang_args = manifest.get("clang_args", [])
    common_notdec_args = manifest.get("notdec_args", [])
    oracle_defaults = manifest.get("oracle", {})

    failures: list[str] = []
    for opt in opt_levels:
        for algorithm in algorithms:
            label = f"{name}:{opt}:{algorithm}"
            ir_path = case_work / f"{opt}.{algorithm}.ll"
            output_path = case_work / f"{opt}.{algorithm}.c"
            clang_command = [
                str(clang),
                "-S",
                "-emit-llvm",
                f"-{opt}",
                "-fno-discard-value-names",
                *common_clang_args,
                *case.get("clang_args", []),
                str(source),
                "-o",
                str(ir_path),
            ]
            clang_proc = run_process(clang_command, REPO_ROOT)
            if clang_proc.returncode != 0 or not ir_path.exists():
                failures.append(
                    f"{label}: clang failed\n$ {format_command(clang_command)}\n"
                    f"{clang_proc.stdout[-MAX_OUTPUT_DETAIL:]}"
                )
                continue

            notdec_command = [
                str(notdec_llvm2c),
                str(ir_path),
                "-o",
                str(output_path),
                f"--algo={algorithm}",
                *common_notdec_args,
                *case.get("notdec_args", []),
            ]
            notdec_proc = run_process(notdec_command, REPO_ROOT)
            if notdec_proc.returncode != 0 or not output_path.exists():
                failures.append(
                    f"{label}: notdec-llvm2c failed\n"
                    f"$ {format_command(notdec_command)}\n"
                    f"{notdec_proc.stdout[-MAX_OUTPUT_DETAIL:]}"
                )
                continue

            output = output_path.read_text()
            oracle = merge_oracle(oracle_defaults, case.get("oracle", {}))
            case_failures = check_oracle(label, oracle, output)
            for failure in case_failures:
                failures.append(
                    f"{failure}\noutput={output_path}\n"
                    f"{output[-MAX_OUTPUT_DETAIL:]}"
                )

    return failures


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--notdec-llvm2c", required=True, type=Path)
    parser.add_argument("--clang", default=default_clang(), type=Path)
    parser.add_argument(
        "--manifest",
        default=SUITE_ROOT / "manifest.json",
        type=Path,
    )
    parser.add_argument("--work-dir", type=Path)
    parser.add_argument("--keep-work-dir", action="store_true")
    args = parser.parse_args()

    manifest = json.loads(args.manifest.read_text())
    work_root = args.work_dir
    temp_dir = None
    if work_root is None:
        temp_dir = tempfile.mkdtemp(prefix="notdec-structuring-source-")
        work_root = Path(temp_dir)
    work_root.mkdir(parents=True, exist_ok=True)

    failures: list[str] = []
    try:
        for case in manifest["cases"]:
            failures.extend(
                run_case(
                    case=case,
                    manifest=manifest,
                    suite_root=args.manifest.parent,
                    work_root=work_root,
                    clang=args.clang,
                    notdec_llvm2c=args.notdec_llvm2c,
                )
            )
    finally:
        if temp_dir is not None and not args.keep_work_dir and not failures:
            shutil.rmtree(temp_dir)

    if failures:
        print(f"work_dir={work_root}")
        print("\n\n".join(failures))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
