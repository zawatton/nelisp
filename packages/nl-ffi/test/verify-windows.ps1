param(
    [Parameter(Mandatory = $true)][string]$Binary,
    [string]$NotesRoot = $env:ANVIL_NOTES_DIR,
    [string]$AnvilRoot = $env:ANVIL_EL_DIR,
    [string]$Output = "target/native-ffi-memory"
)
$ErrorActionPreference = "Stop"
$repo = (Resolve-Path (Join-Path $PSScriptRoot "../../..")).Path
Push-Location $repo
try {
    $exe = (Resolve-Path $Binary).Path
    if (-not $NotesRoot) {
        $candidate = Get-Item $repo
        while ($candidate -and -not (Test-Path (Join-Path $candidate.FullName ".anvil-memory"))) {
            $candidate = $candidate.Parent
        }
        if (-not $candidate) { throw "Set -NotesRoot to the Notes workspace." }
        $NotesRoot = $candidate.FullName
    }
    if (-not $AnvilRoot) {
        $AnvilRoot = Join-Path $NotesRoot "dev/anvil.el"
        if (-not (Test-Path $AnvilRoot)) {
            $AnvilRoot = Join-Path $env:USERPROFILE ".emacs.d/external-packages/anvil.el"
        }
    }
    $savedSource = $env:NELISP_SQLITE_FFI_SOURCE
    try {
        $env:NELISP_SQLITE_FFI_SOURCE = (Resolve-Path (Join-Path $NotesRoot "dev/nelisp-emacs-lib/src/emacs-sqlite-ffi.el")).Path
        & $exe --load packages/nl-ffi/test/nl-ffi-memory-platform-smoke.el
        if ($LASTEXITCODE -ne 0) { throw "Owned memory check failed: $LASTEXITCODE" }
        & $exe --load test/standalone-sqlite-owned-check.el
        if ($LASTEXITCODE -ne 0) { throw "SQLite check failed: $LASTEXITCODE" }
        $python = Get-Command python -ErrorAction SilentlyContinue
        if (-not $python) { $python = Get-Command py }
        & $python.Source packages/nl-ffi/test/anvil-memory-parity.py --binary $exe --notes $NotesRoot --anvil $AnvilRoot --out $Output
        if ($LASTEXITCODE -ne 0) { throw "Anvil MCP check failed: $LASTEXITCODE" }
    }
    finally { $env:NELISP_SQLITE_FFI_SOURCE = $savedSource }
}
finally { Pop-Location }
