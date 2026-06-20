#!/usr/bin/env python3
import argparse
import subprocess
import sys
from pathlib import Path


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--notdec-llvm2c", required=True, type=Path)
    args = parser.parse_args()

    proc = subprocess.run(
        [str(args.notdec_llvm2c), "--algo=structured-phoenix", "/tmp/notdec-while-linear-body.ll", "-o", "/tmp/structured-phoenix.out.c"],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    if proc.returncode != 0:
        print(proc.stdout)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
