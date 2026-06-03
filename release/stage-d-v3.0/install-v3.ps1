# release/stage-d-v3.0/install-v3.ps1 - stage-d-v3.0 Windows standalone installer

[CmdletBinding()]
param(
    [string]$From,
    [string]$Prefix = $env:ANVIL_PREFIX,
    [string]$Version = "stage-d-v3.0",
    [string]$ReleaseBaseUrl = $env:RELEASE_BASE_URL,
    [string]$Target = "windows-x86_64"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ($Target -ne "windows-x86_64") {
    throw ("unsupported -Target " + $Target + " (expected windows-x86_64)")
}

if ([string]::IsNullOrWhiteSpace($Prefix)) {
    $Base = $env:LOCALAPPDATA
    if ([string]::IsNullOrWhiteSpace($Base)) {
        $Base = $HOME
    }
    $Prefix = Join-Path $Base ("anvil-" + $Version)
}

if ([string]::IsNullOrWhiteSpace($ReleaseBaseUrl)) {
    $ReleaseBaseUrl = "https://github.com/zawatton/nelisp/releases/latest/download"
}

function Write-InstallLog {
    param([string]$Message)
    Write-Host ("  ==> " + $Message)
}

if (-not (Get-Command tar -ErrorAction SilentlyContinue)) {
    throw "missing tool: tar"
}

$Artifact = "anvil-" + $Version + "-" + $Target + ".tar.gz"
$Checksum = $Artifact + ".sha256"
$Work = Join-Path ([System.IO.Path]::GetTempPath()) ("anvil-install-v3-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Force -Path $Work | Out-Null

try {
    $TarPath = Join-Path $Work $Artifact
    $ShaPath = Join-Path $Work $Checksum

    if (-not [string]::IsNullOrWhiteSpace($From)) {
        Copy-Item -Path (Join-Path $From $Artifact) -Destination $TarPath
        Copy-Item -Path (Join-Path $From $Checksum) -Destination $ShaPath
    } else {
        Invoke-WebRequest -Uri ($ReleaseBaseUrl.TrimEnd("/") + "/" + $Artifact) -OutFile $TarPath
        Invoke-WebRequest -Uri ($ReleaseBaseUrl.TrimEnd("/") + "/" + $Checksum) -OutFile $ShaPath
    }

    $Recorded = ((Get-Content -Path $ShaPath -TotalCount 1) -split "\s+")[0].ToLowerInvariant()
    $Computed = (Get-FileHash -Algorithm SHA256 -Path $TarPath).Hash.ToLowerInvariant()
    if ($Recorded -ne $Computed) {
        throw ("checksum verify FAILED: recorded " + $Recorded + " computed " + $Computed)
    }
    Write-InstallLog ("checksum OK: " + $Artifact)

    New-Item -ItemType Directory -Force -Path $Prefix | Out-Null
    & tar -xzf $TarPath -C $Prefix --strip-components=1
    $TarCode = $LASTEXITCODE
    if ($null -eq $TarCode) {
        $TarCode = 0
    }
    if ($TarCode -ne 0) {
        throw ("tar extract failed with exit " + $TarCode)
    }

    $Exe = Join-Path $Prefix "bin\nelisp.exe"
    if (-not (Test-Path $Exe)) {
        throw ("installed bin\nelisp.exe missing: " + $Exe)
    }

    Write-InstallLog ("installed: " + $Prefix)
    Write-InstallLog ("next: add " + (Join-Path $Prefix "bin") + " to your PATH")
} finally {
    Remove-Item -Recurse -Force -ErrorAction SilentlyContinue $Work
}
