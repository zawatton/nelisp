# Cross-PC verification script for NeLisp - Windows PowerShell
# Usage: .\scripts\verify-cross-platform.ps1
# Expected: last line = "=== Cross-platform verify PASS ==="

[CmdletBinding()]
param(
    [string]$Emacs = $env:EMACS,
    [int]$ParallelJobs = 0,
    [switch]$SkipNativeSmokes,
    [switch]$IncludeTarball
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

function Invoke-WindowsStandaloneInstallSmoke {
    $Root = Join-Path ([System.IO.Path]::GetTempPath()) ("nelisp-install-smoke-" + [guid]::NewGuid().ToString("N"))
    $Prefix = Join-Path $Root "install"
    try {
        & (Join-Path $RepoRoot "release\stage-d-v3.0\install-v3.ps1") `
            -From (Join-Path $RepoRoot "dist") `
            -Prefix $Prefix `
            -Version "stage-d-v3.0"
        $InstallCode = $LASTEXITCODE
        if ($null -eq $InstallCode) {
            $InstallCode = 0
        }
        if ($InstallCode -ne 0) {
            throw ("installer failed with exit " + $InstallCode)
        }

        $Exe = Join-Path $Prefix "bin\nelisp.exe"
        $Output = & $Exe eval "(+ 40 2)"
        $Code = $LASTEXITCODE
        if ($null -eq $Code) {
            $Code = 0
        }
        $Text = $Output -join "`n"
        if ($Code -ne 0 -or $Text -ne "42") {
            throw ("installed nelisp.exe failed: exit " + $Code + " output " + $Text)
        }
        Write-Host "[installer] PASS: windows-x86_64 installed bin\nelisp.exe eval -> 42"
    } finally {
        Remove-Item -Recurse -Force -ErrorAction SilentlyContinue $Root
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

Invoke-Checked "Windows standalone cache identity smoke" {
    & (Join-Path $RepoRoot "tools\windows-standalone-cache-identity-test.ps1") `
        -Emacs $Emacs
}

Invoke-Checked "Windows standalone reader native smoke" {
    & (Join-Path $RepoRoot "tools\windows-standalone-reader-test.ps1") `
        -Emacs $Emacs
}

if ($IncludeTarball) {
    Invoke-Checked "Windows standalone tarball smoke" {
        & (Join-Path $RepoRoot "tools\build-standalone-tarball.ps1") `
            -Emacs $Emacs `
            -Version "stage-d-v3.0" `
            -Target "windows-x86_64"
        & (Join-Path $RepoRoot "tools\verify-standalone-tarball.ps1") `
            -Version "stage-d-v3.0" `
            -Target "windows-x86_64"
    }

    Invoke-Checked "Windows standalone installer smoke" {
        Invoke-WindowsStandaloneInstallSmoke
    }
}

Write-Host ""
Write-Host "=== Cross-platform verify PASS ==="
