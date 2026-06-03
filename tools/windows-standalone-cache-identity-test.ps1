# Windows-native standalone eval cache identity smoke.
#
# Builds target\nelisp-standalone-eval.exe once from a clean Windows target cache,
# builds it again from cached units, and verifies both PE images are byte-stable.
# No Rust toolchain is used.
#
# Usage:
#
#   .\tools\windows-standalone-cache-identity-test.ps1

[CmdletBinding()]
param(
    [string]$Emacs = $env:EMACS
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($Emacs)) {
    $Emacs = "emacs"
}

$RepoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $RepoRoot

Write-Host "--- Windows standalone cache identity smoke ---"
[System.Environment]::OSVersion | Format-List
& $Emacs --version | Select-Object -First 1

$env:NELISP_STANDALONE_TARGET = "windows-x86_64"
$env:NELISP_FORM_OP = "+"
$env:NELISP_FORM_A = "1"
$env:NELISP_FORM_B = "2"

$Exe = Join-Path $RepoRoot "target\nelisp-standalone-eval.exe"
$CacheGlob = Join-Path $RepoRoot "target\standalone-units\windows-x86_64-arena-*"

Remove-Item -Recurse -Force $CacheGlob -ErrorAction SilentlyContinue
Remove-Item -Force $Exe -ErrorAction SilentlyContinue

function Invoke-StandaloneEvalBuild {
    param(
        [string]$Label
    )

    & $Emacs --batch -Q -L lisp -L src -L scripts `
        --eval "(setq load-prefer-newer t)" `
        -l nelisp-standalone-build `
        -f nelisp-standalone-build
    $Code = $LASTEXITCODE
    if ($null -eq $Code) {
        $Code = 0
    }
    if ($Code -ne 0) {
        Write-Host ("[windows-standalone-cache] FAIL: " + $Label +
                    " build exited " + $Code)
        exit $Code
    }
    if (-not (Test-Path $Exe)) {
        Write-Host ("[windows-standalone-cache] FAIL: " + $Label +
                    " missing " + $Exe)
        exit 1
    }
}

Invoke-StandaloneEvalBuild -Label "fresh"
$FreshHash = (Get-FileHash -Algorithm SHA256 -Path $Exe).Hash

Invoke-StandaloneEvalBuild -Label "cached"
$CachedHash = (Get-FileHash -Algorithm SHA256 -Path $Exe).Hash

if ($FreshHash -ne $CachedHash) {
    Write-Host ("[windows-standalone-cache] FAIL: fresh/cache hash mismatch")
    Write-Host ("  fresh: " + $FreshHash)
    Write-Host ("  cached: " + $CachedHash)
    exit 1
}

Write-Host ("[windows-standalone-cache] PASS: fresh/cache SHA256 " + $FreshHash)
exit 0
