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

& $Exe --embedded
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
        [string]$Label,
        [int]$Expected
    )

    Set-Content -Path $Path -Encoding ascii -NoNewline -Value $Source
    & $Exe $Path
    $FileCode = $LASTEXITCODE
    if ($null -eq $FileCode) {
        $FileCode = 0
    }
    if ($FileCode -ne $Expected) {
        Write-Host ("[windows-standalone-reader] FAIL: " + $Label + " " + $Path +
                    " -> exit " + $FileCode + " (expected " + $Expected + ")")
        exit 1
    }
    Write-Host ("[windows-standalone-reader] PASS: " + $Label +
                " -> exit " + $Expected)
}

$FileSmoke = Join-Path $SmokeDir "file-smoke.el"
Invoke-ReaderFileSmoke -Path $FileSmoke -Source "(+ 40 3)`n" `
    -Label "file arg" -Expected 43

$SpacedFileSmoke = Join-Path $SmokeDir "file smoke spaced.el"
Invoke-ReaderFileSmoke -Path $SpacedFileSmoke -Source "(+ 40 4)`n" `
    -Label "file arg with spaces" -Expected 44

$UnicodeFileSmoke = Join-Path $SmokeDir "unicode-あ.el"
Invoke-ReaderFileSmoke -Path $UnicodeFileSmoke -Source "(+ 40 5)`n" `
    -Label "unicode file arg" -Expected 45

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

function Assert-CodeOutputContains {
    param(
        [string]$Label,
        [string[]]$Output,
        [int]$Code,
        [string]$Needle,
        [int]$ExpectedCode = 2
    )

    $Text = $Output -join "`n"
    if ($Code -ne $ExpectedCode) {
        Write-Host ("[windows-standalone-reader] FAIL: " + $Label +
                    " -> exit " + $Code + " (expected " + $ExpectedCode + ")")
        Write-Host $Text
        exit 1
    }
    if (-not ($Text.Contains($Needle))) {
        Write-Host ("[windows-standalone-reader] FAIL: " + $Label +
                    " output missing " + $Needle)
        Write-Host $Text
        exit 1
    }
    Write-Host ("[windows-standalone-reader] PASS: " + $Label +
                " -> exit " + $ExpectedCode)
}

function Convert-ReaderOutputText {
    param([string]$Text)

    $Normalized = $Text -replace "`r`n", "`n"
    if ($Normalized.EndsWith("`n")) {
        return $Normalized.Substring(0, $Normalized.Length - 1)
    }
    return $Normalized
}

function Invoke-ReaderWithInput {
    param(
        [string[]]$Arguments,
        [string]$InputText
    )

    $Psi = [System.Diagnostics.ProcessStartInfo]::new()
    $Psi.FileName = $Exe
    $Psi.UseShellExecute = $false
    $Psi.RedirectStandardInput = $true
    $Psi.RedirectStandardOutput = $true
    $Psi.RedirectStandardError = $true
    if ($Psi.GetType().GetProperty("StandardInputEncoding")) {
        $Psi.StandardInputEncoding = [System.Text.Encoding]::ASCII
    }
    if ($Psi.GetType().GetProperty("StandardOutputEncoding")) {
        $Psi.StandardOutputEncoding = [System.Text.Encoding]::UTF8
    }
    if ($Psi.GetType().GetProperty("StandardErrorEncoding")) {
        $Psi.StandardErrorEncoding = [System.Text.Encoding]::UTF8
    }
    if ($Psi.GetType().GetProperty("ArgumentList")) {
        foreach ($Arg in $Arguments) {
            [void]$Psi.ArgumentList.Add($Arg)
        }
    } else {
        $Psi.Arguments = (($Arguments | ForEach-Object {
            if ($_ -match '[\s"]') {
                '"' + ($_ -replace '"', '\"') + '"'
            } else {
                $_
            }
        }) -join " ")
    }

    $Proc = [System.Diagnostics.Process]::Start($Psi)
    $Proc.StandardInput.Write($InputText)
    $Proc.StandardInput.Close()
    $Stdout = $Proc.StandardOutput.ReadToEnd()
    $Stderr = $Proc.StandardError.ReadToEnd()
    $Proc.WaitForExit()

    [pscustomobject]@{
        ExitCode = $Proc.ExitCode
        Stdout = Convert-ReaderOutputText $Stdout
        Stderr = Convert-ReaderOutputText $Stderr
    }
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

$EvalOutput = & $Exe --eval "(+ 40 2)"
$EvalCode = $LASTEXITCODE
if ($null -eq $EvalCode) {
    $EvalCode = 0
}
Assert-Output -Label "--eval" -Output $EvalOutput -Code $EvalCode -Expected "42"

$LoadOutput = & $Exe --load $FileSmoke
$LoadCode = $LASTEXITCODE
if ($null -eq $LoadCode) {
    $LoadCode = 0
}
Assert-Output -Label "--load" -Output $LoadOutput -Code $LoadCode -Expected "43"

$BareEvalOutput = & $Exe eval "(+ 40 2)"
$BareEvalCode = $LASTEXITCODE
if ($null -eq $BareEvalCode) {
    $BareEvalCode = 0
}
Assert-CodeOutputContains -Label "bare eval rejected" -Output $BareEvalOutput `
    -Code $BareEvalCode -Needle "Arguments:"

$UnknownOptionOutput = & $Exe --bad-option
$UnknownOptionCode = $LASTEXITCODE
if ($null -eq $UnknownOptionCode) {
    $UnknownOptionCode = 0
}
Assert-CodeOutputContains -Label "unknown option rejected" `
    -Output $UnknownOptionOutput -Code $UnknownOptionCode -Needle "Arguments:"

$HelpExtraOutput = & $Exe --help extra
$HelpExtraCode = $LASTEXITCODE
if ($null -eq $HelpExtraCode) {
    $HelpExtraCode = 0
}
Assert-CodeOutputContains -Label "--help extra arg rejected" `
    -Output $HelpExtraOutput -Code $HelpExtraCode -Needle "Arguments:"

$EvalMissingOutput = & $Exe --eval
$EvalMissingCode = $LASTEXITCODE
if ($null -eq $EvalMissingCode) {
    $EvalMissingCode = 0
}
Assert-CodeOutputContains -Label "--eval missing expr rejected" `
    -Output $EvalMissingOutput -Code $EvalMissingCode -Needle "Arguments:"

$LoadMissingOutput = & $Exe --load
$LoadMissingCode = $LASTEXITCODE
if ($null -eq $LoadMissingCode) {
    $LoadMissingCode = 0
}
Assert-CodeOutputContains -Label "--load missing file rejected" `
    -Output $LoadMissingOutput -Code $LoadMissingCode -Needle "Arguments:"

$RdfSource = Join-Path $SmokeDir "rdf-source.txt"
Set-Content -Path $RdfSource -Encoding ascii -NoNewline -Value "hello"
$RdfProbe = Join-Path $SmokeDir "rdf-probe.el"
$RdfPath = $RdfSource.Replace("\", "/")
Set-Content -Path $RdfProbe -Encoding UTF8 -Value @(
    ("(length (rdf " + (ConvertTo-Json $RdfPath -Compress) + "))")
)
$RdfOutput = & $Exe --load $RdfProbe
$RdfCode = $LASTEXITCODE
if ($null -eq $RdfCode) {
    $RdfCode = 0
}
Assert-Output -Label "rdf file read" -Output $RdfOutput `
    -Code $RdfCode -Expected "5"

$WrfTarget = Join-Path $SmokeDir "wrf-target.txt"
$WrfProbe = Join-Path $SmokeDir "wrf-probe.el"
$WrfPath = $WrfTarget.Replace("\", "/")
$WrfContentLiteral = ConvertTo-Json "written" -Compress
Set-Content -Path $WrfProbe -Encoding UTF8 -Value @(
    ("(wrf " + (ConvertTo-Json $WrfPath -Compress) + " " + $WrfContentLiteral + ")")
)
$WrfOutput = & $Exe --load $WrfProbe
$WrfCode = $LASTEXITCODE
if ($null -eq $WrfCode) {
    $WrfCode = 0
}
Assert-Output -Label "wrf file write" -Output $WrfOutput `
    -Code $WrfCode -Expected "t"
if (-not (Test-Path $WrfTarget)) {
    throw "wrf file write did not create target"
}
$WrfContent = Get-Content -LiteralPath $WrfTarget -Raw
if ($WrfContent -ne "written") {
    throw ("wrf file write content mismatch: " + $WrfContent)
}

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

$RuntimeFnImage = Join-Path $SmokeDir "runtime-smoke-fn.nlri"

$DumpRuntimeFnOutput = & $Exe dump-runtime-image $RuntimeFnImage "(defun image-hot () 99)"
$DumpRuntimeFnCode = $LASTEXITCODE
if ($null -eq $DumpRuntimeFnCode) {
    $DumpRuntimeFnCode = 0
}
Assert-Output -Label "dump-runtime-image defun" -Output $DumpRuntimeFnOutput `
    -Code $DumpRuntimeFnCode -Expected ""

$EvalRuntimeFnOutput = & $Exe eval-runtime-image $RuntimeFnImage "(image-hot)"
$EvalRuntimeFnCode = $LASTEXITCODE
if ($null -eq $EvalRuntimeFnCode) {
    $EvalRuntimeFnCode = 0
}
Assert-Output -Label "eval-runtime-image defun" -Output $EvalRuntimeFnOutput `
    -Code $EvalRuntimeFnCode -Expected "99"

$RuntimeLoadSource = Join-Path $SmokeDir "runtime-load-src.el"
$RuntimeLoadImage = Join-Path $SmokeDir "runtime-load-smoke.nlri"
Set-Content -Path $RuntimeLoadSource `
    -Value @("(setq loaded-base 39)", "(defun loaded-hot () 3)") `
    -Encoding UTF8

$DumpRuntimeLoadOutput = & $Exe dump-runtime-image $RuntimeLoadImage `
    --load $RuntimeLoadSource "(setq loaded-add 0)"
$DumpRuntimeLoadCode = $LASTEXITCODE
if ($null -eq $DumpRuntimeLoadCode) {
    $DumpRuntimeLoadCode = 0
}
Assert-Output -Label "dump-runtime-image --load" -Output $DumpRuntimeLoadOutput `
    -Code $DumpRuntimeLoadCode -Expected ""

$EvalRuntimeLoadOutput = & $Exe eval-runtime-image $RuntimeLoadImage `
    "(+ loaded-base loaded-add (loaded-hot))"
$EvalRuntimeLoadCode = $LASTEXITCODE
if ($null -eq $EvalRuntimeLoadCode) {
    $EvalRuntimeLoadCode = 0
}
Assert-Output -Label "eval-runtime-image --load" -Output $EvalRuntimeLoadOutput `
    -Code $EvalRuntimeLoadCode -Expected "42"

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

if ($env:GITHUB_ACTIONS -eq "true") {
    Write-Host ("[windows-standalone-reader] SKIP: repl stdin/stdout on " +
                "GitHub-hosted Windows runner")
} else {
$ReplLines = @(
    "(+ 40 2)"
    "nil"
    "t"
    "(quote (1 2 3))"
    "(vector 1 `"a`" nil t)"
    "(exit)"
)
$ReplInputFile = Join-Path $SmokeDir "repl-input.el"
Set-Content -LiteralPath $ReplInputFile -Encoding ascii -Value $ReplLines
$ReplStdout = cmd /c ('type "' + $ReplInputFile + '" | "' + $Exe + '" --repl --no-prompt')
$ReplCode = $LASTEXITCODE
if ($null -eq $ReplCode) {
    $ReplCode = 0
}
if ($ReplCode -ne 0) {
    Write-Host ("[windows-standalone-reader] FAIL: repl exited " + $ReplCode)
    exit 1
}
$ReplText = $ReplStdout -join "`n"
$ExpectedReplText = "42`nnil`nt`n(1 2 3)`n[1 `"a`" nil t]"
if ($ReplText -ne $ExpectedReplText) {
    $ReplStdout = cmd /c ('type "' + $ReplInputFile + '" | "' + $Exe + '" --repl --no-prompt')
    $ReplCode = $LASTEXITCODE
    if ($null -eq $ReplCode) {
        $ReplCode = 0
    }
    $ReplText = $ReplStdout -join "`n"
}
Assert-Output -Label "repl stdin/stdout" -Output @($ReplText) -Code 0 `
    -Expected $ExpectedReplText
Write-Host "[windows-standalone-reader] PASS: repl stdin/stdout -> 42"

$NoArgsReplInputFile = Join-Path $SmokeDir "noargs-repl-input.el"
Set-Content -LiteralPath $NoArgsReplInputFile -Encoding ascii -Value "(exit)"
$NoArgsReplStdout = cmd /c ('type "' + $NoArgsReplInputFile + '" | "' + $Exe + '"')
$NoArgsReplCode = $LASTEXITCODE
if ($null -eq $NoArgsReplCode) {
    $NoArgsReplCode = 0
}
if ($NoArgsReplCode -ne 0) {
    Write-Host ("[windows-standalone-reader] FAIL: no-args repl exited " +
                $NoArgsReplCode)
    exit 1
}
$NoArgsReplText = $NoArgsReplStdout -join "`n"
Assert-Output -Label "no-args repl" -Output @($NoArgsReplText) `
    -Code 0 -Expected "nelisp> "

$QuietReplLines = @(
    "(defun hot () 1)"
    "(hot)"
    "(condition-case e (signal 'quit nil) (quit 42))"
    '(nelisp--write-stdout-bytes "explicit\n")'
    "(hot)"
    "(exit)"
)
$QuietInputFile = Join-Path $SmokeDir "quiet-repl-input.el"
Set-Content -LiteralPath $QuietInputFile -Encoding ascii -Value $QuietReplLines
$QuietReplStdout = cmd /c ('type "' + $QuietInputFile + '" | "' + $Exe + '" --repl --no-prompt --no-print')
$QuietReplCode = $LASTEXITCODE
if ($null -eq $QuietReplCode) {
    $QuietReplCode = 0
}
if ($QuietReplCode -ne 0) {
    Write-Host ("[windows-standalone-reader] FAIL: repl --no-print exited " +
                $QuietReplCode)
    exit 1
}
$QuietReplText = $QuietReplStdout -join "`n"
Assert-Output -Label "repl --no-print" -Output @($QuietReplText) `
    -Code 0 -Expected "explicit"
}

& $Exe --repl --bad
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
