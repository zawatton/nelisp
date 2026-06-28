# Download and audit stage-d-v3.0 standalone workflow verification logs.

[CmdletBinding()]
param(
    [string]$RunId,
    [string]$Workflow = "stage-d-v3.0-standalone.yml",
    [string]$OutDir = "target\stage-d-v3.0-verification-logs",
    [switch]$AuditOnly,
    [switch]$Help
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ($Help) {
    Write-Host "usage: tools\fetch-standalone-verification-logs.ps1 [-RunId ID] [-Workflow FILE] [-OutDir DIR] [-AuditOnly]"
    exit 0
}

function Require-Command {
    param([string]$Name)
    if (-not (Get-Command $Name -ErrorAction SilentlyContinue)) {
        throw ("required command not found: " + $Name)
    }
}

if (-not $AuditOnly) {
    Require-Command "gh"
}

if ((-not $AuditOnly) -and [string]::IsNullOrWhiteSpace($RunId)) {
    $RunsJson = gh run list --workflow $Workflow --limit 1 --json databaseId,status,conclusion,headBranch
    if ($LASTEXITCODE -ne 0) {
        throw "gh run list failed"
    }
    $Runs = @($RunsJson | ConvertFrom-Json)
    if (-not $Runs -or $Runs.Count -lt 1) {
        throw ("no workflow runs found for " + $Workflow)
    }
    $RunId = [string]$Runs[0].databaseId
    Write-Host ("[standalone-logs] using latest run " + $RunId + " (" + $Runs[0].status + "/" + $Runs[0].conclusion + ", " + $Runs[0].headBranch + ")")
}

if ($AuditOnly -and -not (Test-Path -LiteralPath $OutDir)) {
    throw ("audit directory not found: " + $OutDir)
}

New-Item -ItemType Directory -Force -Path $OutDir | Out-Null

if (-not $AuditOnly) {
    gh run download $RunId --pattern "stage-d-v3.0-*-log" --dir $OutDir
    if ($LASTEXITCODE -ne 0) {
        throw ("gh run download failed for run " + $RunId)
    }
}

$Expected = @(
    "linux-x86_64",
    "macos-aarch64",
    "macos-x86_64",
    "windows-x86_64"
)

$Missing = @()
foreach ($Platform in $Expected) {
    $Log = Get-ChildItem -LiteralPath $OutDir -Recurse -File -Filter ($Platform + ".log") | Select-Object -First 1
    if (-not $Log) {
        $Missing += $Platform
        continue
    }
    $Text = Get-Content -LiteralPath $Log.FullName -Raw
    if ($Text -notmatch "Cross-platform verify PASS") {
        throw ("verification log missing PASS marker: " + $Log.FullName)
    }
    if ($Text -notmatch "anvil-standalone-svg-smoke") {
        throw ("verification log missing anvil SVG smoke marker: " + $Log.FullName)
    }
    Write-Host ("[standalone-logs] PASS: " + $Platform + " " + $Log.FullName)
}

if ($Missing.Count -gt 0) {
    throw ("missing verification logs: " + ($Missing -join ", "))
}

Write-Host ("[standalone-logs] PASS: all verification logs downloaded to " + (Resolve-Path -LiteralPath $OutDir).Path)
