#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MANIFEST_PATH="$ROOT_DIR/crates/Cargo.toml"
LOG_DIR="$ROOT_DIR/target/.check-logs"

if [[ ! -f "$MANIFEST_PATH" ]]; then
  echo "error: workspace manifest not found at $MANIFEST_PATH" >&2
  exit 1
fi

echo "Running cargo fmt..."
cargo fmt --manifest-path "$MANIFEST_PATH" --all

mkdir -p "$LOG_DIR"
rm -f "$LOG_DIR/clippy.log" "$LOG_DIR/test.log"

echo "Running cargo clippy and cargo test in parallel..."
(
  CARGO_TARGET_DIR="$ROOT_DIR/crates/target-clippy" \
  cargo clippy --manifest-path "$MANIFEST_PATH" --all-targets --all-features -- -D warnings \
  >"$LOG_DIR/clippy.log" 2>&1
) &
CLIPPY_PID=$!

(
  CARGO_TARGET_DIR="$ROOT_DIR/crates/target-test" \
  cargo test --manifest-path "$MANIFEST_PATH" --all-targets --all-features \
  >"$LOG_DIR/test.log" 2>&1
) &
TEST_PID=$!

FAIL=0
wait "$CLIPPY_PID" || FAIL=1
wait "$TEST_PID" || FAIL=1

if [[ "$FAIL" -ne 0 ]]; then
  echo "checks failed"
  if [[ -f "$LOG_DIR/clippy.log" ]]; then
    echo "----- clippy log -----"
    cat "$LOG_DIR/clippy.log"
  fi
  if [[ -f "$LOG_DIR/test.log" ]]; then
    echo "----- test log -----"
    cat "$LOG_DIR/test.log"
  fi
  exit 1
fi

echo "All checks passed."
