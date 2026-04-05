#!/bin/bash
set -euo pipefail

clang -Wdocumentation -fparse-all-comments -Xclang -ast-dump -fsyntax-only "$@"
