# Windows PowerShell multi-process parallel build of the standalone NeLisp eval binary.
#
# Spawns N Emacs batch workers, each compiling a round-robin slice of the
# cacheable units into target/standalone-units/, then runs the serial link step.
# This is the native PowerShell counterpart of tools/build-standalone-parallel.sh.
#
# Usage:
#
#   .\tools\build-standalone-parallel.ps1
#   .\tools\build-standalone-parallel.ps1 -Jobs 4
#   .\tools\build-standalone-parallel.ps1 -Clean
#   .\tools\build-standalone-parallel.ps1 -CompileOnly
#   .\tools\build-standalone-parallel.ps1 -Target windows-x86_64
#
# The build uses Emacs + pure elisp only.  It does not invoke Rust.

[CmdletBinding()]
param(
    [int]$Jobs = 0,
    [string]$Emacs = $env:EMACS,
    [string]$Target = "windows-x86_64",
    [switch]$Clean,
    [switch]$CompileOnly
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($Emacs)) {
    $Emacs = "emacs"
}

if ($Jobs -le 0) {
    $Jobs = [System.Environment]::ProcessorCount
}
if ($Jobs -le 0) {
    $Jobs = 1
}

$RepoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $RepoRoot

$CacheDir = Join-Path $RepoRoot "target\standalone-units"
$LogDir = Join-Path $RepoRoot "target\standalone-parallel"
New-Item -ItemType Directory -Force -Path $LogDir | Out-Null

if ($Clean -and (Test-Path $CacheDir)) {
    Remove-Item -Recurse -Force $CacheDir
}

Write-Host ("[parallel] compiling units with " + $Jobs + " worker(s)...")
Write-Host ("[parallel] target " + $Target)

$WorkerScript = {
    param(
        [string]$WorkerRepoRoot,
        [string]$WorkerEmacs,
        [string]$WorkerTarget,
        [int]$WorkerIndex,
        [int]$WorkerCount,
        [string]$WorkerLogPath
    )

    $ErrorActionPreference = "Stop"
    Set-Location $WorkerRepoRoot
    $env:NELISP_CHUNK_IDX = [string]$WorkerIndex
    $env:NELISP_CHUNK_N = [string]$WorkerCount
    $env:NELISP_STANDALONE_TARGET = $WorkerTarget

    function Join-NativeArguments {
        param([string[]]$Arguments)
        $Quoted = @()
        foreach ($Argument in $Arguments) {
            if ($Argument -match '[\s"]') {
                $Quoted += ('"' + ($Argument -replace '"', '\"') + '"')
            } else {
                $Quoted += $Argument
            }
        }
        return ($Quoted -join " ")
    }

    function Invoke-LoggedNative {
        param(
            [string]$FileName,
            [string[]]$Arguments,
            [string]$LogPath
        )
        $StdoutPath = $LogPath + ".stdout"
        $StderrPath = $LogPath + ".stderr"
        Remove-Item -Force -ErrorAction SilentlyContinue $StdoutPath, $StderrPath, $LogPath
        $Process = Start-Process `
            -FilePath $FileName `
            -ArgumentList (Join-NativeArguments $Arguments) `
            -WorkingDirectory (Get-Location).Path `
            -NoNewWindow `
            -Wait `
            -PassThru `
            -RedirectStandardOutput $StdoutPath `
            -RedirectStandardError $StderrPath
        foreach ($Path in @($StdoutPath, $StderrPath)) {
            if (Test-Path $Path) {
                Get-Content -Path $Path | ForEach-Object {
                    Add-Content -Path $LogPath -Value $_
                    Write-Host $_
                }
            }
        }
        return [int]$Process.ExitCode
    }

    try {
        $Code = Invoke-LoggedNative `
            -FileName $WorkerEmacs `
            -Arguments @(
                "--batch", "-Q", "-L", "lisp", "-L", "src", "-L", "scripts",
                "--eval", "(setq load-prefer-newer t)",
                "-l", "nelisp-standalone-build",
                "-f", "nelisp-standalone-compile-chunk"
            ) `
            -LogPath $WorkerLogPath
    } catch {
        $_ | Out-File -FilePath $WorkerLogPath -Append
        $Code = 1
    }

    [pscustomobject]@{
        Index = $WorkerIndex
        ExitCode = [int]$Code
        LogPath = $WorkerLogPath
    }
}

$WorkerJobs = @()
for ($Index = 0; $Index -lt $Jobs; $Index++) {
    $LogPath = Join-Path $LogDir ("chunk-" + $Index + "-of-" + $Jobs + ".log")
    $WorkerJobs += Start-Job `
        -Name ("nelisp-standalone-chunk-" + $Index) `
        -ScriptBlock $WorkerScript `
        -ArgumentList $RepoRoot, $Emacs, $Target, $Index, $Jobs, $LogPath
}

$Results = @()
foreach ($Job in $WorkerJobs) {
    Wait-Job $Job | Out-Null
    $Output = Receive-Job $Job
    Remove-Job $Job
    $ResultCandidates = @($Output | Where-Object {
        $_ -is [pscustomobject] -and
        $_.PSObject.Properties.Name -contains "ExitCode"
    })
    $Output | Where-Object {
        -not ($_ -is [pscustomobject] -and
               $_.PSObject.Properties.Name -contains "ExitCode")
    } | ForEach-Object { Write-Host $_ }
    if ($ResultCandidates.Count -gt 0) {
        $Result = $ResultCandidates[$ResultCandidates.Count - 1]
    } else {
        $Result = $null
    }
    if ($null -eq $Result) {
        $Results += [pscustomobject]@{
            Index = -1
            ExitCode = 1
            LogPath = "<missing worker result>"
        }
    } else {
        $Results += $Result
    }
}

$Failed = @($Results | Where-Object { $_.ExitCode -ne 0 })
if ($Failed.Count -gt 0) {
    foreach ($Result in $Failed) {
        Write-Host ("[parallel] FAIL: worker " + $Result.Index +
                    " exited " + $Result.ExitCode +
                    " log " + $Result.LogPath)
    }
    exit 1
}

if ($CompileOnly) {
    Write-Host "[parallel] compile-only PASS"
    exit 0
}

Write-Host "[parallel] linking (serial)..."
$LinkLogPath = Join-Path $LogDir "link.log"
$env:NELISP_STANDALONE_TARGET = $Target

function Join-NativeArguments {
    param([string[]]$Arguments)
    $Quoted = @()
    foreach ($Argument in $Arguments) {
        if ($Argument -match '[\s"]') {
            $Quoted += ('"' + ($Argument -replace '"', '\"') + '"')
        } else {
            $Quoted += $Argument
        }
    }
    return ($Quoted -join " ")
}

function Invoke-LoggedNative {
    param(
        [string]$FileName,
        [string[]]$Arguments,
        [string]$LogPath
    )
    $StdoutPath = $LogPath + ".stdout"
    $StderrPath = $LogPath + ".stderr"
    Remove-Item -Force -ErrorAction SilentlyContinue $StdoutPath, $StderrPath, $LogPath
    $Process = Start-Process `
        -FilePath $FileName `
        -ArgumentList (Join-NativeArguments $Arguments) `
        -WorkingDirectory (Get-Location).Path `
        -NoNewWindow `
        -Wait `
        -PassThru `
        -RedirectStandardOutput $StdoutPath `
        -RedirectStandardError $StderrPath
    foreach ($Path in @($StdoutPath, $StderrPath)) {
        if (Test-Path $Path) {
            Get-Content -Path $Path | ForEach-Object {
                Add-Content -Path $LogPath -Value $_
                Write-Host $_
            }
        }
    }
    return [int]$Process.ExitCode
}
$LinkCode = Invoke-LoggedNative `
    -FileName $Emacs `
    -Arguments @(
        "--batch", "-Q", "-L", "lisp", "-L", "src", "-L", "scripts",
        "--eval", "(setq load-prefer-newer t)",
        "-l", "nelisp-standalone-build",
        "-f", "nelisp-standalone-build"
    ) `
    -LogPath $LinkLogPath
if ($LinkCode -ne 0) {
    Write-Host ("[parallel] FAIL: link exited " + $LinkCode +
                " log " + $LinkLogPath)
    exit $LinkCode
}

Write-Host "[parallel] PASS: standalone parallel build completed"
