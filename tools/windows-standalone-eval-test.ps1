# Windows-native standalone eval smoke.
#
# Builds target\nelisp-standalone-eval.exe through the pure-elisp standalone
# builder and executes it on Windows.  No Rust toolchain is used.
#
# Usage:
#
#   .\tools\windows-standalone-eval-test.ps1
#   .\tools\windows-standalone-eval-test.ps1 -Op + -A 40 -B 2

[CmdletBinding()]
param(
    [string]$Emacs = $env:EMACS,
    [string]$Op = "+",
    [int]$A = 1,
    [int]$B = 2,
    [switch]$BuildOnly
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($Emacs)) {
    $Emacs = "emacs"
}

$RepoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $RepoRoot

switch ($Op) {
    "+" { $Expected = $A + $B }
    "-" { $Expected = $A - $B }
    "*" { $Expected = $A * $B }
    default { throw ("unsupported -Op " + $Op + " (use +, -, or *)") }
}

Write-Host "--- Windows standalone eval smoke ---"
[System.Environment]::OSVersion | Format-List
& $Emacs --version | Select-Object -First 1

$env:NELISP_STANDALONE_TARGET = "windows-x86_64"
$env:NELISP_FORM_OP = $Op
$env:NELISP_FORM_A = [string]$A
$env:NELISP_FORM_B = [string]$B

& $Emacs --batch -Q -L lisp -L src -L scripts `
    --eval "(setq load-prefer-newer t)" `
    -l nelisp-standalone-build `
    -f nelisp-standalone-build
$BuildCode = $LASTEXITCODE
if ($null -eq $BuildCode) {
    $BuildCode = 0
}
if ($BuildCode -ne 0) {
    Write-Host ("[windows-standalone-eval] FAIL: build exited " + $BuildCode)
    exit $BuildCode
}

$Exe = Join-Path $RepoRoot "target\nelisp-standalone-eval.exe"
if (-not (Test-Path $Exe)) {
    Write-Host ("[windows-standalone-eval] FAIL: missing " + $Exe)
    exit 1
}

if ($BuildOnly) {
    Write-Host ("[windows-standalone-eval] build-only PASS: " + $Exe)
    exit 0
}

& $Exe
$Code = $LASTEXITCODE
if ($null -eq $Code) {
    $Code = 0
}

if ($Code -eq $Expected) {
    Write-Host ("[windows-standalone-eval] PASS: " + $Exe +
                " -> exit " + $Code + " (expected " + $Expected + ")")
    exit 0
}

Write-Host ("[windows-standalone-eval] FAIL: " + $Exe +
            " -> exit " + $Code + " (expected " + $Expected + ")")
exit 1
