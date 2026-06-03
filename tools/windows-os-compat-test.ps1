# Windows OS compatibility ERT smoke test.
#
# Runs the Emacs-side Windows compatibility tests for nelisp-stdlib-os.  This
# script does not build native EXEs; use tools/windows-selfhost-test.ps1 for the
# PE32+ executable smoke suite.
#
# Run from PowerShell on a Windows machine with Emacs installed:
#
#   .\tools\windows-os-compat-test.ps1
#
# Run a focused suite:
#
#   .\tools\windows-os-compat-test.ps1 -Suite timerfd
#   .\tools\windows-os-compat-test.ps1 -Suite peercred
#
# Run a raw ERT selector:
#
#   .\tools\windows-os-compat-test.ps1 -Selector timerfd-windows
#
# List available suites:
#
#   .\tools\windows-os-compat-test.ps1 -List

[CmdletBinding()]
param(
    [switch]$List,
    [string[]]$Suite = @("all"),
    [string]$Selector = "",
    [string]$Emacs = $env:EMACS
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($Emacs)) {
    $Emacs = "emacs"
}

$RepoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $RepoRoot

$OutDir = Join-Path $RepoRoot "target\windows-os-compat"
New-Item -ItemType Directory -Force -Path $OutDir | Out-Null

$Suites = @(
    @{
        Name = "all"
        Selector = "windows"
        Description = "all Windows OS compatibility ERT tests"
        ExpectedCount = 274
    },
    @{
        Name = "timerfd"
        Selector = "timerfd-windows"
        Description = "waitable timer-backed timerfd compatibility"
    },
    @{
        Name = "peercred"
        Selector = "getsockopt-peercred-windows"
        Description = "synthetic AF_UNIX socketpair peer credentials"
    },
    @{
        Name = "socketpair"
        Selector = "socketpair-windows"
        Description = "loopback-backed socketpair compatibility"
    },
    @{
        Name = "inotify"
        Selector = "inotify-windows"
        Description = "inotify fd compatibility with native directory change pump"
    },
    @{
        Name = "signals"
        Selector = "sigprocmask-windows\|signalfd-windows"
        Description = "emulated sigprocmask and synthetic signalfd"
    },
    @{
        Name = "eventfd"
        Selector = "eventfd-windows"
        Description = "synthetic eventfd compatibility"
    },
    @{
        Name = "fds"
        Selector = "open-windows\|close-windows\|read-windows\|write-windows\|fstat-windows\|lseek-windows\|fcntl-windows\|dup2-windows\|pipe-windows"
        Description = "Windows fd table, handle, dup, and basic I/O behavior"
    },
    @{
        Name = "process"
        Selector = "execve-windows\|create-process\|pidfd.*windows\|wait-windows\|kill-windows\|windows-create-thread\|windows-join"
        Description = "CreateProcess, pidfd-like handles, wait/kill, and thread handles"
    },
    @{
        Name = "sockets"
        Selector = "socket-windows\|bind-.*windows\|connect-.*windows\|accept-.*windows\|listen-windows\|shutdown-windows\|sendto-inet.*windows\|recvfrom-inet.*windows\|getsockname.*windows\|getpeername.*windows\|setsockopt.*windows\|getsockopt-int.*windows\|sendmsg-fds-windows\|recvmsg-fds-windows"
        Description = "Winsock socket, sockaddr, sockopt, and payload messaging paths"
    }
)

$SuiteNames = @($Suites | ForEach-Object { [string]$_.Name })

if ($List) {
    Write-Host "available Windows OS compatibility suites:"
    foreach ($SuiteItem in $Suites) {
        Write-Host ("  " + [string]$SuiteItem.Name + " -> " +
            [string]$SuiteItem.Selector + " - " +
            [string]$SuiteItem.Description)
    }
    Write-Host "raw selector:"
    Write-Host "  -Selector <ERT regexp>"
    exit 0
}

$DefaultSuiteSelected = (($Suite.Count -eq 1) -and ([string]$Suite[0] -eq "all"))

if ((-not [string]::IsNullOrWhiteSpace($Selector)) -and (-not $DefaultSuiteSelected)) {
    throw "Use either -Selector or -Suite, not both."
}

if (-not [string]::IsNullOrWhiteSpace($Selector)) {
    $SuitesToRun = @(@{
        Name = "custom"
        Selector = $Selector
        Description = "custom ERT selector"
    })
} elseif ($Suite -contains "all") {
    $SuitesToRun = @($Suites | Where-Object { [string]$_.Name -eq "all" })
} else {
    $SuitesToRun = @()
    foreach ($Name in $Suite) {
        if (-not ($SuiteNames -contains $Name)) {
            throw ("unknown suite '" + $Name + "'; expected one of: " + ($SuiteNames -join ", "))
        }
        $SuitesToRun += @($Suites | Where-Object { [string]$_.Name -eq $Name })
    }
}

$Failed = $false

function Show-LogTail {
    param([string]$Path)
    if (Test-Path $Path) {
        Get-Content -Path $Path -Tail 12 | ForEach-Object {
            Write-Host ("    " + $_)
        }
    }
}

function Run-ErtSuite {
    param(
        [string]$Name,
        [string]$Selector,
        [int]$ExpectedCount
    )

    $LogPath = Join-Path $OutDir ("nelisp-windows-os-" + $Name + ".log")
    Remove-Item -Force -ErrorAction SilentlyContinue $LogPath

    $oldSelector = $env:NELISP_WINDOWS_OS_SELECTOR
    try {
        $env:NELISP_WINDOWS_OS_SELECTOR = $Selector
        & $Emacs --batch -Q -L lisp -L src `
            -l test/nelisp-stdlib-os-test.el `
            --eval '(ert-run-tests-batch-and-exit (getenv "NELISP_WINDOWS_OS_SELECTOR"))' *> $LogPath

        if ($LASTEXITCODE -ne 0) {
            Write-Host ("[windows-os] FAIL: " + $Name + " selector '" + $Selector + "'")
            Show-LogTail $LogPath
            return $false
        }

        $Summary = Get-Content -Path $LogPath -Tail 3
        if ($ExpectedCount -gt 0) {
            $RanLine = @($Summary | Where-Object { $_ -match "Ran ([0-9]+) tests" }) |
                Select-Object -First 1
            if ($null -eq $RanLine) {
                Write-Host ("[windows-os] FAIL: " + $Name + " missing ERT test count")
                Show-LogTail $LogPath
                return $false
            }
            $ActualCount = [int]([regex]::Match([string]$RanLine, "Ran ([0-9]+) tests").Groups[1].Value)
            if ($ActualCount -lt $ExpectedCount) {
                Write-Host ("[windows-os] FAIL: " + $Name + " ran " + $ActualCount +
                    " tests (expected at least " + $ExpectedCount + ")")
                Show-LogTail $LogPath
                return $false
            }
        }
        Write-Host ("[windows-os] PASS: " + $Name + " selector '" + $Selector + "'")
        $Summary | ForEach-Object {
            if ($_.Length -gt 0) {
                Write-Host ("    " + $_)
            }
        }
        return $true
    } finally {
        $env:NELISP_WINDOWS_OS_SELECTOR = $oldSelector
    }
}

Write-Host "--- Windows OS compatibility ERT smoke ---"
[System.Environment]::OSVersion | Format-List
& $Emacs --version | Select-Object -First 1
Write-Host ("logs: " + $OutDir)
Write-Host ("suites: " + (($SuitesToRun | ForEach-Object { [string]$_.Name }) -join ", "))
Write-Host ""

foreach ($SuiteItem in $SuitesToRun) {
    $ExpectedCount = 0
    if ($SuiteItem.ContainsKey("ExpectedCount")) {
        $ExpectedCount = [int]$SuiteItem.ExpectedCount
    }
    if (-not (Run-ErtSuite -Name ([string]$SuiteItem.Name) `
                -Selector ([string]$SuiteItem.Selector) `
                -ExpectedCount $ExpectedCount)) {
        $Failed = $true
    }
}

if ($Failed) {
    exit 1
}

Write-Host "[windows-os] all PASS - Windows OS compatibility ERT smoke OK"
exit 0
