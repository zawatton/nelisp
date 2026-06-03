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
#
# The build uses Emacs + pure elisp only.  It does not invoke Rust.

[CmdletBinding()]
param(
    [int]$Jobs = 0,
    [string]$Emacs = $env:EMACS,
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

$WorkerScript = {
    param(
        [string]$WorkerRepoRoot,
        [string]$WorkerEmacs,
        [int]$WorkerIndex,
        [int]$WorkerCount,
        [string]$WorkerLogPath
    )

    $ErrorActionPreference = "Stop"
    Set-Location $WorkerRepoRoot
    $env:NELISP_CHUNK_IDX = [string]$WorkerIndex
    $env:NELISP_CHUNK_N = [string]$WorkerCount

    try {
        & $WorkerEmacs --batch -Q -L lisp -L src -L scripts `
            --eval "(setq load-prefer-newer t)" `
            -l nelisp-standalone-build `
            -f nelisp-standalone-compile-chunk 2>&1 |
            Tee-Object -FilePath $WorkerLogPath
        $Code = $LASTEXITCODE
        if ($null -eq $Code) {
            $Code = 0
        }
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
        -ArgumentList $RepoRoot, $Emacs, $Index, $Jobs, $LogPath
}

$Results = @()
foreach ($Job in $WorkerJobs) {
    Wait-Job $Job | Out-Null
    $Output = Receive-Job $Job
    Remove-Job $Job
    $Output | ForEach-Object { Write-Host $_ }
    $ResultCandidates = @($Output | Where-Object {
        $_ -is [pscustomobject] -and
        $_.PSObject.Properties.Name -contains "ExitCode"
    })
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
& $Emacs --batch -Q -L lisp -L src -L scripts `
    --eval "(setq load-prefer-newer t)" `
    -l nelisp-standalone-build `
    -f nelisp-standalone-build 2>&1 |
    Tee-Object -FilePath $LinkLogPath
$LinkCode = $LASTEXITCODE
if ($null -eq $LinkCode) {
    $LinkCode = 0
}
if ($LinkCode -ne 0) {
    Write-Host ("[parallel] FAIL: link exited " + $LinkCode +
                " log " + $LinkLogPath)
    exit $LinkCode
}

Write-Host "[parallel] PASS: standalone parallel build completed"
