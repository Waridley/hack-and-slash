set working-directory := "rs"

# sond-has development tasks
# Use `just --list` to see all available commands

# ── Run ──────────────────────────────────────────────────────────────────────

# Run the game desktop build (debug | release | release-debug)
run profile="debug":
    @just _run-{{profile}}

_run-debug:
    RUSTFLAGS="-Zthreads=16 -Clink-arg=-fuse-ld=mold -Zshare-generics=y" \
    RUST_LOG="info,sond_has=debug,sond_has_engine=debug,wgpu_hal::vulkan=error" \
    cargo run --profile desktop --package sond-has --bin sond-has \
        --features=bevy/file_watcher,bevy/asset_processor,debugging,dev_ui,dylib

_run-release:
    cargo run --profile desktop-release --package sond-has --bin sond-has

_run-release-debug:
    RUST_LOG="info,sond_has=debug,sond_has_engine=debug,wgpu_hal::vulkan=error" \
    cargo run --package sond-has --bin sond-has \
        --features=bevy/file_watcher,import_assets,debugging

# ── Editor ────────────────────────────────────────────────────────────────────

# Run the standalone editor
editor:
    cargo run --package sond-has-editor --profile desktop

# ── Lint / Format / Fix ──────────────────────────────────────────────────────

# Check code with clippy
clippy:
    cargo clippy --workspace --all-features

# Format code
fmt:
    cargo fmt --all -- --config imports_granularity=Crate

# Auto-fix warnings
fix:
    cargo fix --all-features --allow-dirty --allow-staged

# ── Test ──────────────────────────────────────────────────────────────────────

# Run engine tests
test-engine:
    cargo test --package sond-has-engine --no-default-features --features=testing

# Run game tests
test-game:
    cargo test --package sond-has

# Run rng tests
test-rng:
    cargo test --package sond-has-rng

# Run all unit tests
test-all: test-engine test-game test-rng

# Run visual tests (opens a window)
vis-test:
    cargo test --workspace --features=vis_test

# ── Profiling ─────────────────────────────────────────────────────────────────

# Build + run with Tracy profiling
tracy:
    cargo run --profile profiling --package sond-has --bin sond-has \
        --features=bevy/trace_tracy

# ── Web / Trunk ───────────────────────────────────────────────────────────────

# Build web version with trunk (debug | release)
trunk-build profile="release":
    @just _trunk-build-{{profile}}

_trunk-build-release:
    trunk build --release

_trunk-build-debug:
    trunk build --features="debugging"

# Serve web version on LAN
trunk-serve:
    trunk serve --public-url="/" --open --address="0.0.0.0" --features="debugging"

# Watch and rebuild web version
trunk-watch:
    trunk watch --features="debugging"

# ── Maintenance ───────────────────────────────────────────────────────────────

# Clean old build artifacts (3-days | installed)
sweep target="3-days":
    @just _sweep-{{target}}

_sweep-3-days:
    cargo sweep --time 3

_sweep-installed:
    cargo sweep --installed

# Sweep old artifacts, then clippy + rustfmt
cleanup: _sweep-3-days _sweep-installed clippy fmt

# Full validation: vis-test + cleanup
validate: vis-test cleanup
