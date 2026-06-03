# Windows PowerShell builder for the zero-Rust standalone NeLisp CLI tarball.
#
# Produces dist\anvil-stage-d-v3.0-windows-x86_64.tar.gz containing:
#   bin\nelisp.exe
#   src\*.el
#   scripts\*.el
#   lisp\*.el
#   README.org / VERSION / PLATFORM / MANIFEST.txt

[CmdletBinding()]
param(
    [string]$Emacs = $env:EMACS,
    [string]$Version = "stage-d-v3.0",
    [string]$Target = "windows-x86_64"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($Emacs)) {
    $Emacs = "emacs"
}

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
$DistDir = Join-Path $RepoRoot "dist"
$StageDir = Join-Path $DistDir $ArtifactName
$TarFile = Join-Path $DistDir ($ArtifactName + ".tar.gz")
$ShaFile = $TarFile + ".sha256"
$StandaloneBin = Join-Path $RepoRoot "target\nelisp.exe"

Write-Host "[standalone-tarball] version $Version"
Write-Host "[standalone-tarball] target $Target"
Write-Host "[standalone-tarball] building standalone reader for $Target"

$HadOldTarget = Test-Path Env:NELISP_STANDALONE_TARGET
$OldTarget = $env:NELISP_STANDALONE_TARGET
try {
    $env:NELISP_STANDALONE_TARGET = $Target
    & $Emacs --batch -Q -L lisp -L src -L scripts `
        --eval "(setq load-prefer-newer t)" `
        -l nelisp-standalone-build `
        -f nelisp-standalone-build-reader
    $BuildCode = $LASTEXITCODE
    if ($null -eq $BuildCode) {
        $BuildCode = 0
    }
    if ($BuildCode -ne 0) {
        throw ("standalone reader build failed with exit " + $BuildCode)
    }
} finally {
    if ($HadOldTarget) {
        $env:NELISP_STANDALONE_TARGET = $OldTarget
    } else {
        Remove-Item Env:NELISP_STANDALONE_TARGET -ErrorAction SilentlyContinue
    }
}

if (-not (Test-Path $StandaloneBin)) {
    throw ("standalone binary missing: " + $StandaloneBin)
}

Remove-Item -Recurse -Force -ErrorAction SilentlyContinue $StageDir
$StageSubdirs = @(
    (Join-Path $StageDir "bin"),
    (Join-Path $StageDir "src"),
    (Join-Path $StageDir "scripts"),
    (Join-Path $StageDir "lisp")
)
New-Item -ItemType Directory -Force -Path $StageSubdirs | Out-Null

Copy-Item -Path $StandaloneBin -Destination (Join-Path $StageDir "bin\nelisp.exe")
Copy-Item -Path (Join-Path $RepoRoot "src\*.el") -Destination (Join-Path $StageDir "src")
Copy-Item -Path (Join-Path $RepoRoot "scripts\*.el") -Destination (Join-Path $StageDir "scripts") -ErrorAction SilentlyContinue
Copy-Item -Path (Join-Path $RepoRoot "lisp\*.el") -Destination (Join-Path $StageDir "lisp") -ErrorAction SilentlyContinue

if (Test-Path (Join-Path $RepoRoot "LICENSE")) {
    Copy-Item -Path (Join-Path $RepoRoot "LICENSE") -Destination $StageDir
}
if (Test-Path (Join-Path $RepoRoot "README-stage-d-v3.0.org")) {
    Copy-Item -Path (Join-Path $RepoRoot "README-stage-d-v3.0.org") `
        -Destination (Join-Path $StageDir "README.org")
} elseif (Test-Path (Join-Path $RepoRoot "README.org")) {
    Copy-Item -Path (Join-Path $RepoRoot "README.org") `
        -Destination (Join-Path $StageDir "README.org")
}

Set-Content -Path (Join-Path $StageDir "VERSION") -Encoding ascii -Value $Version
Set-Content -Path (Join-Path $StageDir "PLATFORM") -Encoding ascii -Value $Target
@(
    "zero-Rust standalone manifest",
    ("version    " + $Version),
    ("platform   " + $Target),
    "standalone bin/nelisp.exe",
    ("built      " + (Get-Date).ToUniversalTime().ToString("yyyy-MM-ddTHH:mm:ssZ"))
) | Set-Content -Path (Join-Path $StageDir "MANIFEST.txt") -Encoding ascii

Remove-Item -Force -ErrorAction SilentlyContinue $TarFile, $ShaFile
$TarCode = Invoke-NelispTar -WorkingDirectory $DistDir -Arguments @(
    "-czf",
    ($ArtifactName + ".tar.gz"),
    "--",
    $ArtifactName
)
if ($TarCode -ne 0) {
    throw ("tar failed with exit " + $TarCode)
}

$Hash = (Get-FileHash -Algorithm SHA256 -Path $TarFile).Hash.ToLowerInvariant()
Set-Content -Path $ShaFile -Encoding ascii -Value ($Hash + "  " + (Split-Path -Leaf $TarFile))

$SizeBytes = (Get-Item $TarFile).Length
if ($SizeBytes -ge 15 * 1024 * 1024) {
    throw ("tarball exceeds 15 MB cap: " + $SizeBytes + " bytes")
}

Remove-Item -Recurse -Force $StageDir
Write-Host ("[standalone-tarball] PASS: built " + $TarFile + " SHA256 " + $Hash)
