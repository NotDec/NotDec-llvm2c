#!/usr/bin/env python3
import argparse
import subprocess
import sys
import tempfile
from pathlib import Path


IR = r"""
define i32 @main(i32 %x) {
entry:
  %cond = icmp eq i32 %x, 0
  br i1 %cond, label %ret, label %cont

ret:
  ret i32 1

cont:
  ret i32 0
}
"""


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--notdec-llvm2c", required=True, type=Path)
    args = parser.parse_args()

    failures = []
    with tempfile.TemporaryDirectory(prefix="notdec-structurer-registry-") as tmp:
        work_dir = Path(tmp)
        input_path = work_dir / "input.ll"
        input_path.write_text(IR.strip() + "\n")
        for algo in ("structured-goto", "structured-phoenix", "structured-sailr"):
            output_path = work_dir / f"{algo}.c"
            proc = subprocess.run(
                [str(args.notdec_llvm2c), f"--algo={algo}", str(input_path), "-o", str(output_path)],
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
            )
            if proc.returncode != 0:
                failures.append(f"{algo}: command failed\n{proc.stdout}")
            elif not output_path.exists():
                failures.append(f"{algo}: output file was not created")

    if failures:
        print("\n".join(failures))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
