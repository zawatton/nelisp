# Check related repository refs before running the standalone CI workflow.

[CmdletBinding()]
param(
    [string]$NelispRef = "main",
    [string]$NelispEmacsRef = $NelispRef,
    [string]$AnvilRef = $NelispRef
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Test-RemoteBranch {
    param(
        [string]$Repo,
        [string]$Ref
    )

    if ([string]::IsNullOrWhiteSpace($Ref)) {
        throw ("empty ref for " + $Repo)
    }

    $Url = "https://github.com/zawatton/$Repo.git"
    git ls-remote --exit-code --heads $Url $Ref *> $null
    if ($LASTEXITCODE -ne 0) {
        throw ("missing branch " + $Ref + " in " + $Repo)
    }
    Write-Host ("[related-refs] PASS: " + $Repo + " " + $Ref)
}

Test-RemoteBranch "nelisp" $NelispRef
Test-RemoteBranch "nelisp-emacs" $NelispEmacsRef
Test-RemoteBranch "anvil.el" $AnvilRef

Write-Host ""
Write-Host "Dispatch command:"
Write-Host ("gh workflow run stage-d-v3.0-standalone.yml --ref " + $NelispRef + " --field nelisp_emacs_ref=" + $NelispEmacsRef + " --field anvil_ref=" + $AnvilRef)
