# Windows PowerShell verifier for the zero-Rust standalone NeLisp CLI tarball.

[CmdletBinding()]
param(
    [string]$Version = "stage-d-v3.0",
    [string]$Target = "windows-x86_64"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ($Target -ne "windows-x86_64") {
    throw ("unsupported -Target " + $Target + " (expected windows-x86_64)")
}

function Invoke-NelispTar {
    param(
        [string]$WorkingDirectory,
        [string[]]$Arguments
    )

    $Psi = [System.Diagnostics.ProcessStartInfo]::new()
    $Psi.FileName = "tar"
    $Psi.WorkingDirectory = $WorkingDirectory
    $Psi.UseShellExecute = $false
    $Psi.RedirectStandardOutput = $true
    $Psi.RedirectStandardError = $true
    foreach ($Argument in $Arguments) {
        [void]$Psi.ArgumentList.Add($Argument)
    }

    Write-Host ("[standalone-tarball] tar " + ($Arguments -join " "))
    $Process = [System.Diagnostics.Process]::Start($Psi)
    $Stdout = $Process.StandardOutput.ReadToEnd()
    $Stderr = $Process.StandardError.ReadToEnd()
    $Process.WaitForExit()
    if (-not [string]::IsNullOrEmpty($Stdout)) {
        Write-Host $Stdout.TrimEnd()
    }
    if (-not [string]::IsNullOrEmpty($Stderr)) {
        Write-Host $Stderr.TrimEnd()
    }
    return $Process.ExitCode
}

$RepoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $RepoRoot

$ArtifactName = "anvil-" + $Version + "-" + $Target
$TarFile = Join-Path $RepoRoot ("dist\" + $ArtifactName + ".tar.gz")
$ShaFile = $TarFile + ".sha256"

if (-not (Test-Path $TarFile)) {
    throw ("tarball missing: " + $TarFile)
}
if (-not (Test-Path $ShaFile)) {
    throw ("checksum missing: " + $ShaFile)
}

$Recorded = ((Get-Content -Path $ShaFile -TotalCount 1) -split "\s+")[0].ToLowerInvariant()
$Computed = (Get-FileHash -Algorithm SHA256 -Path $TarFile).Hash.ToLowerInvariant()
if ($Recorded -ne $Computed) {
    throw ("SHA mismatch: recorded " + $Recorded + " computed " + $Computed)
}
Write-Host ("[standalone-tarball] PASS: SHA256 " + $Computed)

$TestRoot = Join-Path ([System.IO.Path]::GetTempPath()) ("nelisp-standalone-verify-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Force -Path $TestRoot | Out-Null
try {
    $TarLeaf = Split-Path -Leaf $TarFile
    Copy-Item -Path $TarFile -Destination (Join-Path $TestRoot $TarLeaf)
    $TarCode = Invoke-NelispTar -WorkingDirectory $TestRoot -Arguments @(
        "-xzf",
        $TarLeaf
    )
    if ($TarCode -ne 0) {
        throw ("tar extract failed with exit " + $TarCode)
    }

    $InstallDir = Join-Path $TestRoot $ArtifactName
    $Exe = Join-Path $InstallDir "bin\nelisp.exe"
    foreach ($Rel in @("src", "scripts", "lisp", "VERSION", "PLATFORM", "MANIFEST.txt", "bin\nelisp.exe")) {
        $Path = Join-Path $InstallDir $Rel
        if (-not (Test-Path $Path)) {
            throw ("missing " + $Rel)
        }
    }
    if ((Get-Content -Path (Join-Path $InstallDir "VERSION") -TotalCount 1) -ne $Version) {
        throw "VERSION mismatch"
    }
    if ((Get-Content -Path (Join-Path $InstallDir "PLATFORM") -TotalCount 1) -ne $Target) {
        throw "PLATFORM mismatch"
    }
    $Manifest = Get-Content -Raw -Path (Join-Path $InstallDir "MANIFEST.txt")
    if ($Manifest -notmatch "standalone bin/nelisp\.exe") {
        throw "MANIFEST missing standalone bin entry"
    }
    Write-Host "[standalone-tarball] PASS: layout"

    $EvalOutput = & $Exe eval "(+ 40 2)"
    $EvalCode = $LASTEXITCODE
    if ($null -eq $EvalCode) {
        $EvalCode = 0
    }
    $EvalText = $EvalOutput -join "`n"
    if ($EvalCode -ne 0 -or $EvalText -ne "42") {
        throw ("eval failed: exit " + $EvalCode + " output " + $EvalText)
    }
    Write-Host "[standalone-tarball] PASS: bin\nelisp.exe eval"

    if ($env:GITHUB_ACTIONS -eq "true") {
        Write-Host ("[standalone-tarball] SKIP: bin\nelisp.exe repl on " +
                    "GitHub-hosted Windows runner")
    } else {
        $ReplOutput = @(
            "(+ 40 2)",
            "(vector 1 `"a`" nil t)",
            "(exit)"
        ) | & $Exe repl --no-prompt
        $ReplCode = $LASTEXITCODE
        if ($null -eq $ReplCode) {
            $ReplCode = 0
        }
        $ReplText = $ReplOutput -join "`n"
        if ($ReplCode -ne 0 -or $ReplText -ne "42`n[1 `"a`" nil t]") {
            throw ("repl failed: exit " + $ReplCode + " output " + $ReplText)
        }
        Write-Host "[standalone-tarball] PASS: bin\nelisp.exe repl"
    }
    Write-Host "[standalone-tarball] all PASS - Windows standalone tarball OK"
} finally {
    Remove-Item -Recurse -Force -ErrorAction SilentlyContinue $TestRoot
}
