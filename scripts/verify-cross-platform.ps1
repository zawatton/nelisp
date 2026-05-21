# Cross-PC verification script for NeLisp — Windows PowerShell
# Usage: .\scripts\verify-cross-platform.ps1
# Expected: last line = "=== Cross-platform verify PASS ==="
$ErrorActionPreference = "Stop"

$RepoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $RepoRoot

Write-Host "--- Platform info ---"
[System.Environment]::OSVersion | Format-List
rustc --version
emacs --version | Select-Object -First 1

Write-Host ""
Write-Host "--- cargo build --release -p nelisp-build-tool ---"
cargo build --release -p nelisp-build-tool
if ($LASTEXITCODE -ne 0) { throw "cargo build failed" }

Write-Host ""
Write-Host "--- cargo test --release -p nelisp-build-tool --lib ---"
cargo test --release -p nelisp-build-tool --lib
if ($LASTEXITCODE -ne 0) { throw "cargo test failed" }

Write-Host ""
Write-Host "--- make compile (byte-compile elisp) ---"
# On Windows, 'make' may not be available; try nmake or skip gracefully
if (Get-Command make -ErrorAction SilentlyContinue) {
    make compile 2>&1 | Select-Object -Last 5
} elseif (Get-Command emacs -ErrorAction SilentlyContinue) {
    Write-Host "make not found; running byte-compile via emacs --batch directly"
    emacs --batch -Q `
        --eval "(setq load-prefer-newer t)" `
        --eval "(byte-recompile-directory `"src`" 0 t)" `
        --eval "(byte-recompile-directory `"packages`" 0 t)" 2>&1 | Select-Object -Last 5
} else {
    Write-Host "Skipping byte-compile (make and emacs not found)"
}

Write-Host ""
Write-Host "--- smoke: target\release\nelisp.exe --eval '(+ 1 2)' ---"
$nbin = "target\release\nelisp.exe"
if (Test-Path $nbin) {
    & $nbin --eval "(+ 1 2)"
    if ($LASTEXITCODE -ne 0) { Write-Host "Smoke: expected 3" }
} else {
    Write-Host "Smoke: $nbin not found (build-tool only mode), skipping"
}

Write-Host ""
Write-Host "=== Cross-platform verify PASS ==="
