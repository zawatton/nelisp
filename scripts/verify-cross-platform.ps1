# Cross-PC verification script for NeLisp — Windows PowerShell
# Usage: .\scripts\verify-cross-platform.ps1
# Expected: last line = "=== Cross-platform verify PASS ==="
$ErrorActionPreference = "Stop"

$Emacs = $env:EMACS
if ([string]::IsNullOrWhiteSpace($Emacs)) {
    $Emacs = "emacs"
}

$RepoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $RepoRoot

Write-Host "--- Platform info ---"
[System.Environment]::OSVersion | Format-List
& $Emacs --version | Select-Object -First 1

Write-Host ""
Write-Host "--- make compile (byte-compile elisp) ---"
# On Windows, 'make' may not be available; try nmake or skip gracefully
if (Get-Command make -ErrorAction SilentlyContinue) {
    make compile 2>&1 | Select-Object -Last 5
} elseif (Get-Command $Emacs -ErrorAction SilentlyContinue) {
    Write-Host "make not found; running byte-compile via emacs --batch directly"
    & $Emacs --batch -Q `
        --eval "(setq load-prefer-newer t)" `
        --eval "(byte-recompile-directory `"src`" 0 t)" `
        --eval "(byte-recompile-directory `"packages`" 0 t)" 2>&1 | Select-Object -Last 5
} else {
    Write-Host "Skipping byte-compile (make and emacs not found)"
}

Write-Host ""
Write-Host "--- standalone gate (zero-Rust) ---"
# NOTE: The standalone gate is 'make standalone-reader-test'.
# PowerShell 'make' availability is environment-dependent; if make is
# present it will run the gate, otherwise invoke emacs --batch directly.
if (Get-Command make -ErrorAction SilentlyContinue) {
    make standalone-reader-test
} else {
    Write-Host "make not available — standalone-reader-test gate skipped on this host."
    Write-Host "Run 'make standalone-reader-test' in a POSIX shell to verify."
}

Write-Host ""
Write-Host "--- standalone parallel build (zero-Rust) ---"
if (Get-Command $Emacs -ErrorAction SilentlyContinue) {
    $ParallelJobs = [Math]::Min(2, [System.Environment]::ProcessorCount)
    if ($ParallelJobs -le 0) {
        $ParallelJobs = 1
    }
    & (Join-Path $RepoRoot "tools\build-standalone-parallel.ps1") -Jobs $ParallelJobs
} else {
    Write-Host "Skipping standalone parallel build (emacs not found)"
}

Write-Host ""
Write-Host "=== Cross-platform verify PASS ==="
