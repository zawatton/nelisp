# Preflight and optionally dispatch the stage-d-v3.0 standalone workflow.

[CmdletBinding()]
param(
    [string]$NelispRef = "main",
    [string]$NelispEmacsRef = $NelispRef,
    [string]$AnvilRef = $NelispRef,
    [string]$Workflow = "stage-d-v3.0-standalone.yml",
    [string]$OutDir = "target\stage-d-v3.0-verification-logs",
    [switch]$Run,
    [switch]$Watch,
    [switch]$FetchLogs,
    [switch]$Help
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ($Help) {
    Write-Host "usage: tools\dispatch-standalone-workflow.ps1 [-NelispRef REF] [-NelispEmacsRef REF] [-AnvilRef REF] [-Workflow FILE] [-OutDir DIR] [-Run] [-Watch] [-FetchLogs]"
    exit 0
}

function Require-Command {
    param([string]$Name)
    if (-not (Get-Command $Name -ErrorAction SilentlyContinue)) {
        throw ("required command not found: " + $Name)
    }
}

function Test-RemoteBranch {
    param([string]$Repo, [string]$Ref)
    if ([string]::IsNullOrWhiteSpace($Ref)) {
        throw ("empty ref for " + $Repo)
    }
    $Url = "https://github.com/zawatton/$Repo.git"
    git ls-remote --exit-code --heads $Url $Ref *> $null
    if ($LASTEXITCODE -ne 0) {
        throw ("missing branch " + $Ref + " in " + $Repo)
    }
    Write-Host ("[standalone-dispatch] PASS: " + $Repo + " " + $Ref)
}

Require-Command "git"
if ($Run) {
    Require-Command "gh"
}

Test-RemoteBranch "nelisp" $NelispRef
Test-RemoteBranch "nelisp-emacs" $NelispEmacsRef
Test-RemoteBranch "anvil.el" $AnvilRef

$GhArgs = @(
    "workflow", "run", $Workflow,
    "--ref", $NelispRef,
    "--field", ("nelisp_emacs_ref=" + $NelispEmacsRef),
    "--field", ("anvil_ref=" + $AnvilRef)
)

if (-not $Run) {
    Write-Host ""
    Write-Host "Dry run. Dispatch with:"
    Write-Host ("gh " + ($GhArgs -join " "))
    Write-Host ""
    Write-Host "Then audit logs with:"
    Write-Host ("tools\fetch-standalone-verification-logs.ps1 -OutDir " + $OutDir)
    exit 0
}

& gh @GhArgs
if ($LASTEXITCODE -ne 0) {
    throw "gh workflow run failed"
}

Start-Sleep -Seconds 5
$RunsJson = gh run list --workflow $Workflow --branch $NelispRef --limit 1 --json databaseId,status,conclusion,headBranch
if ($LASTEXITCODE -ne 0) {
    throw "gh run list failed"
}
$Runs = @($RunsJson | ConvertFrom-Json)
if (-not $Runs -or $Runs.Count -lt 1) {
    throw ("no workflow run found for " + $Workflow + " on " + $NelispRef)
}
$RunId = [string]$Runs[0].databaseId
Write-Host ("[standalone-dispatch] run id: " + $RunId + " (" + $Runs[0].status + "/" + $Runs[0].conclusion + ")")

$WatchExitCode = 0
if ($Watch -or $FetchLogs) {
    gh run watch $RunId --exit-status
    $WatchExitCode = $LASTEXITCODE
    if (($WatchExitCode -ne 0) -and (-not $FetchLogs)) {
        throw ("workflow run failed: " + $RunId)
    }
}

if ($FetchLogs) {
    & (Join-Path $PSScriptRoot "fetch-standalone-verification-logs.ps1") -RunId $RunId -Workflow $Workflow -OutDir $OutDir
}

if ($WatchExitCode -ne 0) {
    throw ("workflow run failed: " + $RunId)
}
