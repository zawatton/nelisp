# Windows-native standalone reader smoke.
#
# Builds target\nelisp.exe through the pure-elisp standalone
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

$Exe = Join-Path $RepoRoot "target\nelisp.exe"
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

function Assert-Output {
    param(
        [string]$Label,
        [string[]]$Output,
        [int]$Code,
        [string]$Expected
    )

    $Text = $Output -join "`n"
    if ($Code -ne 0) {
        Write-Host ("[windows-standalone-reader] FAIL: " + $Label +
                    " exited " + $Code)
        Write-Host $Text
        exit 1
    }
    if ($Text -ne $Expected) {
        Write-Host ("[windows-standalone-reader] FAIL: " + $Label +
                    " output mismatch")
        Write-Host ("expected: " + $Expected)
        Write-Host ("actual  : " + $Text)
        exit 1
    }
    Write-Host ("[windows-standalone-reader] PASS: " + $Label)
}

$HelpOutput = & $Exe --help
$HelpCode = $LASTEXITCODE
if ($null -eq $HelpCode) {
    $HelpCode = 0
}
if ($HelpCode -ne 0 -or -not (($HelpOutput -join "`n") -match "Usage: nelisp")) {
    Write-Host "[windows-standalone-reader] FAIL: --help"
    Write-Host ($HelpOutput -join "`n")
    exit 1
}
Write-Host "[windows-standalone-reader] PASS: --help"

$EvalOutput = & $Exe eval "(+ 40 2)"
$EvalCode = $LASTEXITCODE
if ($null -eq $EvalCode) {
    $EvalCode = 0
}
Assert-Output -Label "eval" -Output $EvalOutput -Code $EvalCode -Expected "42"

$DashEvalOutput = & $Exe -e "(vector 1 `"a`" nil t)"
$DashEvalCode = $LASTEXITCODE
if ($null -eq $DashEvalCode) {
    $DashEvalCode = 0
}
Assert-Output -Label "-e" -Output $DashEvalOutput -Code $DashEvalCode -Expected "[1 `"a`" nil t]"

$LoadOutput = & $Exe load $FileSmoke
$LoadCode = $LASTEXITCODE
if ($null -eq $LoadCode) {
    $LoadCode = 0
}
Assert-Output -Label "load" -Output $LoadOutput -Code $LoadCode -Expected "42"

$ReplOutput = @(
    "(+ 40 2)",
    "nil",
    "t",
    "(quote (1 2 3))",
    "(vector 1 `"a`" nil t)",
    "(exit)"
) | & $Exe repl --no-prompt
$ReplCode = $LASTEXITCODE
if ($null -eq $ReplCode) {
    $ReplCode = 0
}
if ($ReplCode -ne 0) {
    Write-Host ("[windows-standalone-reader] FAIL: repl exited " + $ReplCode)
    exit 1
}
Assert-Output -Label "repl stdin/stdout" -Output $ReplOutput -Code $ReplCode `
    -Expected "42`nnil`nt`n(1 2 3)`n[1 `"a`" nil t]"
Write-Host "[windows-standalone-reader] PASS: repl stdin/stdout -> 42"
Write-Host "[windows-standalone-reader] all PASS - Windows-native standalone reader OK"
exit 0
