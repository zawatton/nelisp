# Windows x86_64 self-host smoke test.
#
# Builds small PE32+ console executables through the pure-elisp PE writer,
# runs each on native Windows, and asserts exit code / stdout.  Run from
# PowerShell on a Windows machine with Emacs installed:
#
#   .\tools\windows-selfhost-test.ps1
#
# To run only one smoke:
#
#   .\tools\windows-selfhost-test.ps1 -Smoke virtualalloc
#
# To validate generation without running the EXEs:
#
#   .\tools\windows-selfhost-test.ps1 -EmitOnly
#
# No external compiler, linker, CRT, or code signing is required.

[CmdletBinding()]
param(
    [switch]$EmitOnly,
    [string[]]$Smoke = @("all"),
    [string]$Emacs = $env:EMACS
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($Emacs)) {
    $Emacs = "emacs"
}

$RepoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $RepoRoot

$OutDir = Join-Path $RepoRoot "target\windows-smoke"
New-Item -ItemType Directory -Force -Path $OutDir | Out-Null

$Smokes = @(
    @{
        Name = "exit42"
        Spec = "minimal-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "virtualalloc"
        Spec = "virtualalloc-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "arena"
        Spec = "virtualalloc-arena-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "writefile-stdout"
        Spec = "writefile-stdout-exit-42"
        ExpectedExit = 42
        ExpectedStdout = "hello from nelisp windows"
    }
)

$SmokeNames = @($Smokes | ForEach-Object { [string]$_.Name })
if ($Smoke -contains "all") {
    $SelectedSmokeNames = $SmokeNames
} else {
    $SelectedSmokeNames = @()
    foreach ($Name in $Smoke) {
        if (-not ($SmokeNames -contains $Name)) {
            throw ("unknown smoke '" + $Name + "'; expected one of: all, " + ($SmokeNames -join ", "))
        }
        $SelectedSmokeNames += $Name
    }
}
$SmokesToRun = @($Smokes | Where-Object { $SelectedSmokeNames -contains ([string]$_.Name) })

$Failed = $false

function Show-LogTail {
    param([string]$Path)
    if (Test-Path $Path) {
        Get-Content -Path $Path -Tail 8 | ForEach-Object {
            Write-Host ("    " + $_)
        }
    }
}

function Build-SmokeExe {
    param(
        [string]$Name,
        [string]$Spec,
        [string]$OutPath
    )

    $LogPath = Join-Path $OutDir ("nelisp-windows-" + $Name + ".build.log")
    Remove-Item -Force -ErrorAction SilentlyContinue $OutPath, $LogPath

    $oldSpec = $env:NELISP_WINDOWS_SPEC
    $oldOut = $env:NELISP_WINDOWS_OUT
    try {
        $env:NELISP_WINDOWS_SPEC = $Spec
        $env:NELISP_WINDOWS_OUT = $OutPath

        & $Emacs --batch -Q -L lisp -L scripts -l nelisp-windows-build `
            -f nelisp-windows-build-from-env *> $LogPath

        if ($LASTEXITCODE -ne 0) {
            Write-Host ("[windows] FAIL: " + $Name + " - build exit " + $LASTEXITCODE)
            Show-LogTail $LogPath
            return $false
        }
        if (-not (Test-Path $OutPath)) {
            Write-Host ("[windows] FAIL: " + $Name + " - EXE was not written")
            Show-LogTail $LogPath
            return $false
        }
        return $true
    } finally {
        $env:NELISP_WINDOWS_SPEC = $oldSpec
        $env:NELISP_WINDOWS_OUT = $oldOut
    }
}

function Run-SmokeExe {
    param(
        [string]$Name,
        [string]$ExePath,
        [int]$ExpectedExit,
        [AllowNull()][string]$ExpectedStdout
    )

    $output = & $ExePath 2>&1
    $exitCode = $LASTEXITCODE
    $stdout = if ($null -eq $output) { "" } else { ($output -join "`n") }

    if ($exitCode -ne $ExpectedExit) {
        Write-Host ("[windows] FAIL: " + $Name + " -> exit " + $exitCode + " (expected " + $ExpectedExit + ")")
        if ($stdout.Length -gt 0) {
            Write-Host ("    stdout: " + $stdout)
        }
        return $false
    }

    if ($null -ne $ExpectedStdout -and $stdout -ne $ExpectedStdout) {
        Write-Host ("[windows] FAIL: " + $Name + " -> stdout mismatch")
        Write-Host ("    got:      " + $stdout)
        Write-Host ("    expected: " + $ExpectedStdout)
        return $false
    }

    if ($null -eq $ExpectedStdout -and $stdout.Length -ne 0) {
        Write-Host ("[windows] FAIL: " + $Name + " -> unexpected stdout: " + $stdout)
        return $false
    }

    Write-Host ("[windows] PASS: " + $Name + " -> exit " + $exitCode)
    return $true
}

Write-Host "--- Windows x86_64 PE32+ self-host smoke ---"
[System.Environment]::OSVersion | Format-List
& $Emacs --version | Select-Object -First 1
Write-Host ("output: " + $OutDir)
Write-Host ("smokes: " + ($SelectedSmokeNames -join ", "))
Write-Host ""

foreach ($SmokeItem in $SmokesToRun) {
    $Name = [string]$SmokeItem.Name
    $Spec = [string]$SmokeItem.Spec
    $ExpectedExit = [int]$SmokeItem.ExpectedExit
    $ExpectedStdout = $SmokeItem.ExpectedStdout
    $ExePath = Join-Path $OutDir ("nelisp-windows-" + $Name + ".exe")

    if (-not (Build-SmokeExe -Name $Name -Spec $Spec -OutPath $ExePath)) {
        $Failed = $true
        continue
    }

    if ($EmitOnly) {
        Write-Host ("[windows] PASS: " + $Name + " -> built")
        continue
    }

    if (-not (Run-SmokeExe -Name $Name -ExePath $ExePath `
                -ExpectedExit $ExpectedExit -ExpectedStdout $ExpectedStdout)) {
        $Failed = $true
    }
}

if ($Failed) {
    exit 1
}

if ($EmitOnly) {
    Write-Host "[windows] all PASS - pure-elisp PE32+ emit-only smoke OK"
} else {
    Write-Host "[windows] all PASS - pure-elisp PE32+ native Windows x86_64 smoke OK"
}
exit 0
