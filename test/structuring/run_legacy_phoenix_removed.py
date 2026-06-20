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
        [str(args.notdec_llvm2c), "--algo=phoenix", "/tmp/notdec-while-linear-body.ll", "-o", "/tmp/legacy-phoenix.out.c"],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    if proc.returncode == 0:
        print("legacy phoenix entry still works")
        return 1
    if "Cannot find option named 'phoenix'" not in proc.stdout:
        print(proc.stdout)
        print("missing expected rejection message")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
