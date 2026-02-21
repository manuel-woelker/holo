#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MANIFEST_PATH="$ROOT_DIR/crates/Cargo.toml"

if [[ ! -f "$MANIFEST_PATH" ]]; then
  echo "error: workspace manifest not found at $MANIFEST_PATH" >&2
  exit 1
fi

echo "Running cargo fmt..."
cargo fmt --manifest-path "$MANIFEST_PATH" --all

echo "Running cargo clippy..."
cargo clippy --manifest-path "$MANIFEST_PATH" --all-targets --all-features --no-deps -- -D warnings

echo "Running cargo test..."
cargo test --manifest-path "$MANIFEST_PATH" --all-targets --all-features

echo "All checks passed."
