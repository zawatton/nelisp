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

$RuntimeImage = Join-Path $SmokeDir "runtime-smoke.nlri"

$DumpRuntimeOutput = & $Exe dump-runtime-image $RuntimeImage "(setq base 40)"
$DumpRuntimeCode = $LASTEXITCODE
if ($null -eq $DumpRuntimeCode) {
    $DumpRuntimeCode = 0
}
Assert-Output -Label "dump-runtime-image" -Output $DumpRuntimeOutput `
    -Code $DumpRuntimeCode -Expected ""

$EvalRuntimeOutput = & $Exe eval-runtime-image $RuntimeImage "(setq add 2)" "(+ base add)"
$EvalRuntimeCode = $LASTEXITCODE
if ($null -eq $EvalRuntimeCode) {
    $EvalRuntimeCode = 0
}
Assert-Output -Label "eval-runtime-image" -Output $EvalRuntimeOutput `
    -Code $EvalRuntimeCode -Expected "42"

$ExecRuntimeOutput = & $Exe exec-runtime-image $RuntimeImage "(setq add 2)" "(+ base add)"
$ExecRuntimeCode = $LASTEXITCODE
if ($null -eq $ExecRuntimeCode) {
    $ExecRuntimeCode = 0
}
Assert-Output -Label "exec-runtime-image" -Output $ExecRuntimeOutput `
    -Code $ExecRuntimeCode -Expected ""

& $Exe exec-runtime-image $RuntimeImage
$ExecRuntimeMissingCode = $LASTEXITCODE
if ($null -eq $ExecRuntimeMissingCode) {
    $ExecRuntimeMissingCode = 0
}
if ($ExecRuntimeMissingCode -ne 1) {
    Write-Host ("[windows-standalone-reader] FAIL: exec-runtime-image missing form" +
                " -> exit " + $ExecRuntimeMissingCode + " (expected 1)")
    exit 1
}
Write-Host "[windows-standalone-reader] PASS: exec-runtime-image missing form -> exit 1"

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

$QuietReplOutput = @(
    "(defun hot () 1)",
    "(hot)",
    "(condition-case e (signal 'quit nil) (quit 42))",
    '(nelisp--write-stdout-bytes "explicit\n")',
    "(hot)",
    "(exit)"
) | & $Exe repl --no-prompt --no-print
$QuietReplCode = $LASTEXITCODE
if ($null -eq $QuietReplCode) {
    $QuietReplCode = 0
}
Assert-Output -Label "repl --no-print" -Output $QuietReplOutput `
    -Code $QuietReplCode -Expected "explicit"

& $Exe repl --bad
$BadReplCode = $LASTEXITCODE
if ($null -eq $BadReplCode) {
    $BadReplCode = 0
}
if ($BadReplCode -ne 2) {
    Write-Host ("[windows-standalone-reader] FAIL: repl bad option" +
                " -> exit " + $BadReplCode + " (expected 2)")
    exit 1
}
Write-Host "[windows-standalone-reader] PASS: repl bad option -> exit 2"

Write-Host "[windows-standalone-reader] all PASS - Windows-native standalone reader OK"
exit 0
