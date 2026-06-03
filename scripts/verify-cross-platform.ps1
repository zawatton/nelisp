# Cross-PC verification script for NeLisp - Windows PowerShell
# Usage: .\scripts\verify-cross-platform.ps1
# Expected: last line = "=== Cross-platform verify PASS ==="

[CmdletBinding()]
param(
    [string]$Emacs = $env:EMACS,
    [int]$ParallelJobs = 0,
    [switch]$SkipNativeSmokes
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($Emacs)) {
    $Emacs = "emacs"
}

$RepoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $RepoRoot

function Invoke-Checked {
    param(
        [string]$Label,
        [scriptblock]$Command
    )

    Write-Host ""
    Write-Host ("--- " + $Label + " ---")
    & $Command
    $Code = $LASTEXITCODE
    if ($null -eq $Code) {
        $Code = 0
    }
    if ($Code -ne 0) {
        throw ($Label + " failed with exit " + $Code)
    }
}

if ($ParallelJobs -le 0) {
    $ParallelJobs = [Math]::Min(2, [System.Environment]::ProcessorCount)
}
if ($ParallelJobs -le 0) {
    $ParallelJobs = 1
}

Write-Host "--- Platform info ---"
[System.Environment]::OSVersion | Format-List
& $Emacs --version | Select-Object -First 1

if (-not $SkipNativeSmokes) {
    Invoke-Checked "Windows OS compatibility ERT smoke" {
        & (Join-Path $RepoRoot "tools\windows-os-compat-test.ps1") `
            -Suite all `
            -Emacs $Emacs
    }

    Invoke-Checked "Windows x86_64 PE32+ self-host smoke" {
        & (Join-Path $RepoRoot "tools\windows-selfhost-test.ps1") `
            -Smoke all `
            -Emacs $Emacs
    }
}

Invoke-Checked "standalone parallel build (zero-Rust)" {
    & (Join-Path $RepoRoot "tools\build-standalone-parallel.ps1") `
        -Jobs $ParallelJobs `
        -Emacs $Emacs
}

Invoke-Checked "Windows standalone eval native smoke" {
    & (Join-Path $RepoRoot "tools\windows-standalone-eval-test.ps1") `
        -Emacs $Emacs
}

Write-Host ""
Write-Host "=== Cross-platform verify PASS ==="
