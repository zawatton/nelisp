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
#   .\tools\windows-selfhost-test.ps1 -Smoke exit42
#   .\tools\windows-selfhost-test.ps1 -Smoke virtualalloc
#   .\tools\windows-selfhost-test.ps1 -Smoke virtualprotect-free
#   .\tools\windows-selfhost-test.ps1 -Smoke arena
#   .\tools\windows-selfhost-test.ps1 -Smoke writefile-stdout
#   .\tools\windows-selfhost-test.ps1 -Smoke readfile-stdin
#   .\tools\windows-selfhost-test.ps1 -Smoke createpipe
#   .\tools\windows-selfhost-test.ps1 -Smoke createfile-write
#   .\tools\windows-selfhost-test.ps1 -Smoke setfilepointer
#   .\tools\windows-selfhost-test.ps1 -Smoke getfiletype
#   .\tools\windows-selfhost-test.ps1 -Smoke getfileinformation
#   .\tools\windows-selfhost-test.ps1 -Smoke filemapping
#   .\tools\windows-selfhost-test.ps1 -Smoke getcurrentprocessid
#   .\tools\windows-selfhost-test.ps1 -Smoke duplicatehandle
#   .\tools\windows-selfhost-test.ps1 -Smoke sethandleinformation
#   .\tools\windows-selfhost-test.ps1 -Smoke getlasterror
#   .\tools\windows-selfhost-test.ps1 -Smoke getcommandline
#   .\tools\windows-selfhost-test.ps1 -Smoke commandlinetoargv
#   .\tools\windows-selfhost-test.ps1 -Smoke wsastartup
#   .\tools\windows-selfhost-test.ps1 -Smoke winsock-socket
#   .\tools\windows-selfhost-test.ps1 -Smoke createprocess
#   .\tools\windows-selfhost-test.ps1 -Smoke createthread
#
# To validate generation without running the EXEs:
#
#   .\tools\windows-selfhost-test.ps1 -EmitOnly
#
# To list available smoke names:
#
#   .\tools\windows-selfhost-test.ps1 -List
#
# No external compiler, linker, CRT, or code signing is required.

[CmdletBinding()]
param(
    [switch]$EmitOnly,
    [switch]$List,
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
        Name = "virtualprotect-free"
        Spec = "virtualprotect-free-exit-42"
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
    },
    @{
        Name = "readfile-stdin"
        Spec = "readfile-stdin-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
        StdinText = "nelisp readfile smoke"
    },
    @{
        Name = "createpipe"
        Spec = "createpipe-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "createfile-write"
        Spec = "createfile-write-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "setfilepointer"
        Spec = "setfilepointer-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "getfiletype"
        Spec = "getfiletype-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "getfileinformation"
        Spec = "getfileinformation-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "filemapping"
        Spec = "filemapping-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "getcurrentprocessid"
        Spec = "getcurrentprocessid-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "duplicatehandle"
        Spec = "duplicatehandle-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "sethandleinformation"
        Spec = "sethandleinformation-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "getlasterror"
        Spec = "getlasterror-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "getcommandline"
        Spec = "getcommandline-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "commandlinetoargv"
        Spec = "commandlinetoargv-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "wsastartup"
        Spec = "wsastartup-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "winsock-socket"
        Spec = "winsock-socket-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "createprocess"
        Spec = "createprocess-wait-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    },
    @{
        Name = "createthread"
        Spec = "createthread-wait-exit-42"
        ExpectedExit = 42
        ExpectedStdout = $null
    }
)

$SmokeNames = @($Smokes | ForEach-Object { [string]$_.Name })
if ($List) {
    Write-Host "available smokes:"
    Write-Host "  all"
    foreach ($SmokeItem in $Smokes) {
        $Line = "  " + [string]$SmokeItem.Name +
            " -> " + [string]$SmokeItem.Spec +
            " exit " + [int]$SmokeItem.ExpectedExit
        if ($null -ne $SmokeItem.ExpectedStdout) {
            $Line += " stdout `"" + [string]$SmokeItem.ExpectedStdout + "`""
        }
        Write-Host $Line
    }
    exit 0
}

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
    $oldErrorActionPreference = $ErrorActionPreference
    try {
        $env:NELISP_WINDOWS_SPEC = $Spec
        $env:NELISP_WINDOWS_OUT = $OutPath

        $ErrorActionPreference = "Continue"
        & $Emacs --batch -Q -L lisp -L src -L scripts -l nelisp-windows-build `
            -f nelisp-windows-build-from-env *> $LogPath
        $ErrorActionPreference = $oldErrorActionPreference

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
        $ErrorActionPreference = $oldErrorActionPreference
        $env:NELISP_WINDOWS_SPEC = $oldSpec
        $env:NELISP_WINDOWS_OUT = $oldOut
    }
}

function Run-SmokeExe {
    param(
        [string]$Name,
        [string]$ExePath,
        [int]$ExpectedExit,
        [AllowNull()][string]$ExpectedStdout,
        [AllowNull()][string]$StdinText
    )

    if ($null -ne $StdinText) {
        $output = $StdinText | & $ExePath 2>&1
    } else {
        $output = & $ExePath 2>&1
    }
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
    $StdinText = if ($SmokeItem.ContainsKey("StdinText")) {
        [string]$SmokeItem.StdinText
    } else {
        $null
    }
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
                -ExpectedExit $ExpectedExit -ExpectedStdout $ExpectedStdout `
                -StdinText $StdinText)) {
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
