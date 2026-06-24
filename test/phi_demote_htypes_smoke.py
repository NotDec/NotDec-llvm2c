#!/usr/bin/env python3
import argparse
import subprocess
import sys
import tempfile
from pathlib import Path


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--notdec", required=True, type=Path)
    args = parser.parse_args()

    project_root = Path(__file__).resolve().parents[3]
    input_path = project_root / "test/type-recovery/llvm-ir/cases/09_OffsetLoop.ll"
    with tempfile.TemporaryDirectory(prefix="notdec-phi-htypes-") as tmp:
        work_dir = Path(tmp)
        output_path = work_dir / "out.c"
        htypes_path = work_dir / "ValueCTypes.txt"
        proc = subprocess.run(
            [
                str(args.notdec),
                str(input_path),
                "-o",
                str(output_path),
                "--tr-level=2",
                "--frozen-tr-input-ir",
                "--dump-htypes",
                str(htypes_path),
                "--gen-work-dir",
            ],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )
        if proc.returncode != 0:
            print(proc.stdout)
            return 1
        if not htypes_path.exists():
            print("missing ValueCTypes.txt")
            return 1
        text = htypes_path.read_text()
        if "main::%stack1" not in text:
            print("missing demoted stack slot mapping")
            return 1
        if "phi" in text:
            print("unexpected phi key in ValueCTypes")
            return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
