# Windows-native standalone reader smoke.
#
# Builds target\nelisp-standalone-reader.exe through the pure-elisp standalone
# builder and exercises embedded source, file-argument source, and REPL stdin /
# stdout on Windows.  No Rust toolchain is used.
#
# Usage:
#
#   .\tools\windows-standalone-reader-test.ps1
#   .\tools\windows-standalone-reader-test.ps1 -Source "(+ 40 2)" -Expected 42

[CmdletBinding()]
param(
    [string]$Emacs = $env:EMACS,
    [string]$Source = "(+ 40 2)",
    [int]$Expected = 42,
    [switch]$BuildOnly,
    [switch]$EmbeddedOnly
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($Emacs)) {
    $Emacs = "emacs"
}

$RepoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $RepoRoot

Write-Host "--- Windows standalone reader smoke ---"
[System.Environment]::OSVersion | Format-List
& $Emacs --version | Select-Object -First 1

$env:NELISP_STANDALONE_TARGET = "windows-x86_64"
$env:NELISP_SRC = $Source

& $Emacs --batch -Q -L lisp -L src -L scripts `
    --eval "(setq load-prefer-newer t)" `
    -l nelisp-standalone-build `
    -f nelisp-standalone-build-reader
$BuildCode = $LASTEXITCODE
if ($null -eq $BuildCode) {
    $BuildCode = 0
}
if ($BuildCode -ne 0) {
    Write-Host ("[windows-standalone-reader] FAIL: build exited " + $BuildCode)
    exit $BuildCode
}

$Exe = Join-Path $RepoRoot "target\nelisp-standalone-reader.exe"
if (-not (Test-Path $Exe)) {
    Write-Host ("[windows-standalone-reader] FAIL: missing " + $Exe)
    exit 1
}

if ($BuildOnly) {
    Write-Host ("[windows-standalone-reader] build-only PASS: " + $Exe)
    exit 0
}

& $Exe
$Code = $LASTEXITCODE
if ($null -eq $Code) {
    $Code = 0
}

if ($Code -eq $Expected) {
    Write-Host ("[windows-standalone-reader] PASS: " + $Exe +
                " src=" + $Source +
                " -> exit " + $Code + " (expected " + $Expected + ")")
} else {
    Write-Host ("[windows-standalone-reader] FAIL: " + $Exe +
                " src=" + $Source +
                " -> exit " + $Code + " (expected " + $Expected + ")")
    exit 1
}

if ($EmbeddedOnly) {
    exit 0
}

$SmokeDir = Join-Path $RepoRoot "target\windows-standalone-reader"
New-Item -ItemType Directory -Force -Path $SmokeDir | Out-Null

function Invoke-ReaderFileSmoke {
    param(
        [string]$Path,
        [string]$Source,
        [string]$Label
    )

    Set-Content -Path $Path -Encoding ascii -NoNewline -Value $Source
    & $Exe $Path
    $FileCode = $LASTEXITCODE
    if ($null -eq $FileCode) {
        $FileCode = 0
    }
    if ($FileCode -ne 42) {
        Write-Host ("[windows-standalone-reader] FAIL: " + $Label + " " + $Path +
                    " -> exit " + $FileCode + " (expected 42)")
        exit 1
    }
    Write-Host ("[windows-standalone-reader] PASS: " + $Label + " -> exit 42")
}

$FileSmoke = Join-Path $SmokeDir "file-smoke.el"
Invoke-ReaderFileSmoke -Path $FileSmoke -Source "(+ 39 3)`n" -Label "file arg"

$SpacedFileSmoke = Join-Path $SmokeDir "file smoke spaced.el"
Invoke-ReaderFileSmoke -Path $SpacedFileSmoke -Source "(* 6 7)`n" -Label "file arg with spaces"

$UnicodeFileSmoke = Join-Path $SmokeDir "unicode-あ.el"
Invoke-ReaderFileSmoke -Path $UnicodeFileSmoke -Source "(- 50 8)`n" -Label "unicode file arg"

$ReplOutput = @("(+ 40 2)", ",quit") | & $Exe repl --no-prompt
$ReplCode = $LASTEXITCODE
if ($null -eq $ReplCode) {
    $ReplCode = 0
}
if ($ReplCode -ne 0) {
    Write-Host ("[windows-standalone-reader] FAIL: repl exited " + $ReplCode)
    exit 1
}
if (-not (($ReplOutput -join "`n") -match "(^|`n)42(`n|$)")) {
    Write-Host "[windows-standalone-reader] FAIL: repl output did not contain 42"
    Write-Host ($ReplOutput -join "`n")
    exit 1
}
Write-Host "[windows-standalone-reader] PASS: repl stdin/stdout -> 42"
Write-Host "[windows-standalone-reader] all PASS - Windows-native standalone reader OK"
exit 0
