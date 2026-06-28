param(
    [string]$NelispExe,
    [string]$NelispEmacsSrc,
    [string]$AnvilDir,
    [string]$AnvilLauncherPath,
    [string]$SvgDir,
    [string]$WorkDir,
    [string]$BashExe,
    [ValidateSet("auto", "real-edit", "annotate", "outline", "extract", "basic", "batch", "entity", "late", "late-extract", "late-batch", "generate", "full")]
    [string]$Suite = "auto",
    [switch]$Launcher
)

$ErrorActionPreference = "Stop"

$RepoRoot = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot "..")).Path
if (-not $NelispExe) {
    $NelispExe = Join-Path $RepoRoot "target\nelisp.exe"
}
if (-not $NelispEmacsSrc) {
    $NelispEmacsSrc = (Resolve-Path -LiteralPath (Join-Path $RepoRoot "..\nelisp-emacs\src")).Path
}
if (-not $AnvilDir) {
    $AnvilDir = (Resolve-Path -LiteralPath (Join-Path $RepoRoot "..\anvil.el")).Path
}
if (-not $AnvilLauncherPath) {
    $AnvilLauncherPath = Join-Path $RepoRoot "bin\anvil"
}
if (-not $WorkDir) {
    $WorkDir = Join-Path $RepoRoot "target\anvil-standalone-svg-smoke"
}
if (-not $SvgDir) {
    if ($env:ANVIL_STANDALONE_SVG_DIR) {
        $SvgDir = $env:ANVIL_STANDALONE_SVG_DIR
    }
}

New-Item -ItemType Directory -Force -Path $WorkDir | Out-Null

$InputCopy = Join-Path $WorkDir "input.svg"
$Output = Join-Path $WorkDir "output.svg"
$BatchOutput = Join-Path $WorkDir "batch-output.svg"
$EntityInput = Join-Path $WorkDir "entity-input.svg"
$EntityExtractOutput = Join-Path $WorkDir "entity-extract.txt"
$EntityBatchOutput = Join-Path $WorkDir "entity-batch-output.svg"
$LateInput = Join-Path $WorkDir "late-input.svg"
$LateExtractOutput = Join-Path $WorkDir "late-extract.txt"
$LateBatchOutput = Join-Path $WorkDir "late-batch-output.svg"
$GeneratedOutput = Join-Path $WorkDir "generated.svg"
$EntitiesJson = Join-Path $WorkDir "entities.json"
$OsIdentityOutput = Join-Path $WorkDir "os-identity.txt"
$OutlineOutput = Join-Path $WorkDir "outline.txt"
$ExtractOutput = Join-Path $WorkDir "extract.txt"
Remove-Item -LiteralPath $Output, $BatchOutput, $EntityInput, $EntityExtractOutput, $EntityBatchOutput, $LateInput, $LateExtractOutput, $LateBatchOutput, $GeneratedOutput, $EntitiesJson, $OsIdentityOutput, $OutlineOutput, $ExtractOutput -Force -ErrorAction SilentlyContinue
Set-Content -LiteralPath $EntitiesJson -Encoding ascii -Value '[{"type":"text","layer":"codex-generate","text":"NELISP-EMACS-GENERATE","p1":[4,5],"height":7},{"type":"line","layer":"codex-generate","p1":[1,1],"p2":[20,1]}]'

$InputSourcePath = $null
$InputSourceHash = $null
$RealInput = $false
if ($SvgDir) {
    if (-not (Test-Path -LiteralPath $SvgDir)) {
        throw ("SvgDir not found: " + $SvgDir)
    }
    $InputSource = Get-ChildItem -LiteralPath $SvgDir -Filter *.svg -File |
        Select-Object -First 1
    if (-not $InputSource) {
        throw "No .svg files found under $SvgDir"
    }
    $InputSourcePath = $InputSource.FullName
    $RealInput = $true
    $InputSourceHash = (Get-FileHash -Algorithm SHA256 -LiteralPath $InputSourcePath).Hash
    Copy-Item -LiteralPath $InputSourcePath -Destination $InputCopy -Force
} else {
    $InputSourcePath = $InputCopy
    Set-Content -LiteralPath $InputCopy -Encoding ascii -Value '<svg xmlns="http://www.w3.org/2000/svg" width="80" height="40"><rect x="1" y="1" width="10" height="10"/></svg>'
}
$InputHash = (Get-FileHash -Algorithm SHA256 -LiteralPath $InputCopy).Hash
$LatePad = "x" * 9000
Set-Content -LiteralPath $LateInput -Encoding ascii -Value ('<svg xmlns="http://www.w3.org/2000/svg" width="80" height="40"><rect x="1" y="1" width="10" height="10"/><!--' + $LatePad + '--><text class="codex-late">NELISP-EMACS-LATE</text></svg>')
Set-Content -LiteralPath $EntityInput -Encoding ascii -Value '<svg xmlns="http://www.w3.org/2000/svg"><text class="codex-entity">A &amp; B</text></svg>'
if ($Suite -eq "auto") {
    if ($RealInput) {
        $Suite = "real-edit"
    } else {
        $Suite = "full"
    }
}
if ($RealInput -and $Suite -ne "real-edit") {
    throw "Real SVG mode only supports -Suite real-edit"
}
$RunIdentity = $Suite -in @("real-edit", "basic", "full")
$RunAnnotate = $Suite -in @("real-edit", "annotate", "outline", "extract", "basic", "batch", "full")
$RunOutline = $Suite -in @("real-edit", "outline", "basic", "full")
$RunExtract = (-not $RealInput) -and ($Suite -in @("extract", "basic", "full"))
$RunBatch = (-not $RealInput) -and ($Suite -in @("batch", "full"))
$RunEntity = (-not $RealInput) -and ($Suite -in @("entity", "full"))
$RunLateExtract = (-not $RealInput) -and ($Suite -in @("late", "late-extract", "full"))
$RunLateBatch = (-not $RealInput) -and ($Suite -in @("late", "late-batch", "full"))
$RunGenerate = (-not $RealInput) -and ($Suite -in @("generate", "full"))

function JsonString([string]$Value) {
    ConvertTo-Json $Value -Compress
}

function SlashPath([string]$Value) {
    $Value -replace "\\", "/"
}

function Find-AnvilBash {
    foreach ($Candidate in @(
        "C:\msys64\usr\bin\bash.exe",
        "C:\Program Files\Git\bin\bash.exe"
    )) {
        if (Test-Path $Candidate) {
            return $Candidate
        }
    }

    $Where = Get-Command bash.exe -ErrorAction SilentlyContinue
    if ($null -ne $Where) {
        return $Where.Source
    }
    return $null
}

$AnvilShims = Join-Path $AnvilDir "anvil-nelisp-shims.el"
$AnvilCad = Join-Path $AnvilDir "anvil-cad.el"
$NelispEmacsFiles = @(
    "files-runtime.el",
    "nelisp-coding.el",
    "nelisp-emacs-compat.el",
    "nelisp-emacs-compat-fileio.el",
    "emacs-string.el",
    "emacs-callproc.el",
    "emacs-vars.el",
    "emacs-os-detect.el",
    "emacs-buffer-builtins.el",
    "emacs-fileio-builtins.el"
) | ForEach-Object { Join-Path $NelispEmacsSrc $_ }

function Invoke-SmokeForms([string]$Name, [string[]]$Forms) {
    $SmokeEl = Join-Path $WorkDir ("anvil-standalone-svg-smoke-" + $Name + ".el")
    $LoadForms = @()
    $LoadForms += "(setq emacs-os-explicit-system-type (quote windows-nt))"
    $LoadForms += "(setq emacs-os-explicit-system-configuration `"x86_64-w64-mingw32`")"
    $LoadForms += "(load $(JsonString (SlashPath $AnvilShims)))"
    foreach ($File in $NelispEmacsFiles) {
        $LoadForms += "(load $(JsonString (SlashPath $File)))"
    }
    $LoadForms += "(load $(JsonString (SlashPath $AnvilCad)))"
    $LoadForms += $Forms
    Set-Content -LiteralPath $SmokeEl -Value ($LoadForms -join "`n") -Encoding ascii

    $OutputText = & $NelispExe --load $SmokeEl
    $Code = $LASTEXITCODE
    if ($null -eq $Code) {
        $Code = 0
    }
    if ($Code -ne 0) {
        throw "nelisp $Name exited with $Code"
    }
    return $OutputText
}

$RunOutput = @()
if ($Launcher) {
    if (-not $BashExe) {
        $BashExe = Find-AnvilBash
    }
    if (-not $BashExe) {
        throw "bash.exe not found; install MSYS2 or Git Bash, or omit -Launcher to test nelisp.exe --load directly"
    }

    $env:ANVIL_LISP_DIR = $AnvilDir
    $env:ANVIL_NELISP_EMACS_SRC_DIR = $NelispEmacsSrc

    function Invoke-AnvilLauncher([string]$Name, [string[]]$Arguments) {
        $OutputText = & $BashExe (SlashPath $AnvilLauncherPath) @Arguments
        $Code = $LASTEXITCODE
        if ($null -eq $Code) {
            $Code = 0
        }
        if ($Code -ne 0) {
            throw "bin/anvil $Name exited with $Code"
        }
        return $OutputText
    }

    if ($RunAnnotate) {
        $RunOutput += Invoke-AnvilLauncher "annotate" @(
            "cad-annotate-svg",
            "--in", (SlashPath $InputCopy),
            "--out", (SlashPath $Output),
            "--text", "NELISP-EMACS-SMOKE",
            "--layer", "codex-smoke",
            "--x", "12",
            "--y", "12",
            "--height", "8"
        )
    }
    if ($RunOutline) {
        $RunOutput += Invoke-AnvilLauncher "outline" @(
            "cad-read-outline-svg",
            "--in", (SlashPath $Output),
            "--out", (SlashPath $OutlineOutput)
        )
    }
    if ($RunExtract) {
        $RunOutput += Invoke-AnvilLauncher "extract" @(
            "cad-extract-svg",
            "--in", (SlashPath $Output),
            "--out", (SlashPath $ExtractOutput),
            "--type", "text"
        )
    }
    if ($RunBatch) {
        $RunOutput += Invoke-AnvilLauncher "batch" @(
            "cad-batch-update-svg",
            "--in", (SlashPath $Output),
            "--out", (SlashPath $BatchOutput),
            "--layer", "codex-smoke",
            "--type", "text",
            "--match-text", "NELISP-EMACS-SMOKE",
            "--find", "NELISP-EMACS-SMOKE",
            "--replace", "NELISP-EMACS-BATCH",
            "--set-layer", "codex-batch"
        )
    }
    if ($RunEntity) {
        $RunOutput += Invoke-AnvilLauncher "entity-extract" @(
            "cad-extract-svg",
            "--in", (SlashPath $EntityInput),
            "--out", (SlashPath $EntityExtractOutput),
            "--type", "text",
            "--layer", "codex-entity"
        )
        $RunOutput += Invoke-AnvilLauncher "entity-batch" @(
            "cad-batch-update-svg",
            "--in", (SlashPath $EntityInput),
            "--out", (SlashPath $EntityBatchOutput),
            "--layer", "codex-entity",
            "--type", "text",
            "--match-text", "A & B",
            "--find", "&",
            "--replace", "and",
            "--set-layer", "codex-entity-batch"
        )
    }
    if ($RunLateExtract) {
        $RunOutput += Invoke-AnvilLauncher "late-extract" @(
            "cad-extract-svg",
            "--in", (SlashPath $LateInput),
            "--out", (SlashPath $LateExtractOutput),
            "--type", "text",
            "--layer", "codex-late"
        )
    }
    if ($RunLateBatch) {
        $RunOutput += Invoke-AnvilLauncher "late-batch" @(
            "cad-batch-update-svg",
            "--in", (SlashPath $LateInput),
            "--out", (SlashPath $LateBatchOutput),
            "--layer", "codex-late",
            "--type", "text",
            "--match-text", "NELISP-EMACS-LATE",
            "--find", "NELISP-EMACS-LATE",
            "--replace", "NELISP-EMACS-LATE-BATCH",
            "--set-layer", "codex-late-batch"
        )
    }
    if ($RunGenerate) {
        $RunOutput += Invoke-AnvilLauncher "generate" @(
            "cad-generate-svg",
            "--out", (SlashPath $GeneratedOutput),
            "--entities-file", (SlashPath $EntitiesJson),
            "--overwrite"
        )
    }
    if ($RunIdentity) {
        $RunOutput += Invoke-AnvilLauncher "identity" @(
            "standalone-os-identity",
            "--out", (SlashPath $OsIdentityOutput)
        )
    }
} else {
    if ($RunIdentity) {
        $RunOutput += Invoke-SmokeForms "identity" @(
            "(wrf $(JsonString (SlashPath $OsIdentityOutput)) (format `"%S`" (list :system-type system-type :path-separator path-separator :exec-suffixes exec-suffixes :system-configuration system-configuration)))"
        )
    }
    if ($RunAnnotate) {
        $RunOutput += Invoke-SmokeForms "annotate" @(
            "(anvil-cad-annotate $(JsonString (SlashPath $InputCopy)) `"NELISP-EMACS-SMOKE`" `"12`" `"12`" `"codex-smoke`" `"8`" $(JsonString (SlashPath $Output)))"
        )
    }
    if ($RunOutline) {
        $RunOutput += Invoke-SmokeForms "outline" @(
            "(wrf $(JsonString (SlashPath $OutlineOutput)) (anvil-cad-read-outline $(JsonString (SlashPath $Output))))"
        )
    }
    if ($RunExtract) {
        $RunOutput += Invoke-SmokeForms "extract" @(
            "(wrf $(JsonString (SlashPath $ExtractOutput)) (anvil-cad-extract $(JsonString (SlashPath $Output)) `"`" `"text`"))"
        )
    }
    if ($RunBatch) {
        $RunOutput += Invoke-SmokeForms "batch" @(
            "(anvil-cad-batch-update $(JsonString (SlashPath $Output)) `"codex-smoke`" `"text`" `"NELISP-EMACS-SMOKE`" `"NELISP-EMACS-SMOKE`" `"NELISP-EMACS-BATCH`" `"codex-batch`" $(JsonString (SlashPath $BatchOutput)))"
        )
    }
    if ($RunEntity) {
        $RunOutput += Invoke-SmokeForms "entity" @(
            "(wrf $(JsonString (SlashPath $EntityExtractOutput)) (anvil-cad-extract $(JsonString (SlashPath $EntityInput)) `"codex-entity`" `"text`"))",
            "(anvil-cad-batch-update $(JsonString (SlashPath $EntityInput)) `"codex-entity`" `"text`" `"A & B`" `"&`" `"and`" `"codex-entity-batch`" $(JsonString (SlashPath $EntityBatchOutput)))"
        )
    }
    if ($RunLateExtract) {
        $RunOutput += Invoke-SmokeForms "late-extract" @(
            "(wrf $(JsonString (SlashPath $LateExtractOutput)) (anvil-cad-extract $(JsonString (SlashPath $LateInput)) `"codex-late`" `"text`"))"
        )
    }
    if ($RunLateBatch) {
        $RunOutput += Invoke-SmokeForms "late-batch" @(
            "(anvil-cad-batch-update $(JsonString (SlashPath $LateInput)) `"codex-late`" `"text`" `"NELISP-EMACS-LATE`" `"NELISP-EMACS-LATE`" `"NELISP-EMACS-LATE-BATCH`" `"codex-late-batch`" $(JsonString (SlashPath $LateBatchOutput)))"
        )
    }
    if ($RunGenerate) {
        $RunOutput += Invoke-SmokeForms "generate" @(
            "(anvil-cad-generate $(JsonString (SlashPath $GeneratedOutput)) `"[{\`"type\`":\`"text\`",\`"layer\`":\`"codex-generate\`",\`"text\`":\`"NELISP-EMACS-GENERATE\`",\`"p1\`":[4,5],\`"height\`":7},{\`"type\`":\`"line\`",\`"layer\`":\`"codex-generate\`",\`"p1\`":[1,1],\`"p2\`":[20,1]}]`" `"`" `"overwrite`")"
        )
    }
}
if ($RunAnnotate) {
    if (-not (Test-Path -LiteralPath $Output)) {
        throw "Output SVG was not created"
    }
    if (-not (Select-String -LiteralPath $Output -Pattern "NELISP-EMACS-SMOKE" -SimpleMatch -Quiet)) {
        throw "Output SVG does not contain smoke annotation"
    }
    if ((Get-FileHash -Algorithm SHA256 -LiteralPath $InputCopy).Hash -ne $InputHash) {
        throw "Input SVG copy was modified"
    }
    if ($RealInput -and ((Get-FileHash -Algorithm SHA256 -LiteralPath $InputSourcePath).Hash -ne $InputSourceHash)) {
        throw "Original SVG was modified"
    }
    if ((Get-FileHash -Algorithm SHA256 -LiteralPath $Output).Hash -eq $InputHash) {
        throw "Output SVG is identical to input"
    }
}
if ($RunOutline) {
    if (-not (Select-String -LiteralPath $OutlineOutput -Pattern ":format svg" -SimpleMatch -Quiet)) {
        throw "Outline output does not report SVG"
    }
}
if ($RunExtract) {
    if (-not (Select-String -LiteralPath $ExtractOutput -Pattern "NELISP-EMACS-SMOKE" -SimpleMatch -Quiet)) {
        throw "Extract output does not contain smoke annotation"
    }
}
if ($RunBatch) {
    if (-not (Select-String -LiteralPath $BatchOutput -Pattern "NELISP-EMACS-BATCH" -SimpleMatch -Quiet)) {
        throw "Batch output does not contain replacement marker"
    }
    if (-not (Select-String -LiteralPath $BatchOutput -Pattern 'class="codex-batch"' -SimpleMatch -Quiet)) {
        throw "Batch output does not contain updated layer"
    }
    if ((Get-FileHash -Algorithm SHA256 -LiteralPath $BatchOutput).Hash -eq
        (Get-FileHash -Algorithm SHA256 -LiteralPath $Output).Hash) {
        throw "Batch output is identical to annotated output"
    }
}
if ($RunEntity) {
    if (-not (Select-String -LiteralPath $EntityExtractOutput -Pattern ':text "A & B"' -SimpleMatch -Quiet)) {
        throw "Entity extract output does not contain decoded text"
    }
    if (-not (Select-String -LiteralPath $EntityBatchOutput -Pattern "A and B" -SimpleMatch -Quiet)) {
        throw "Entity batch output does not contain decoded replacement marker"
    }
    if (-not (Select-String -LiteralPath $EntityBatchOutput -Pattern 'class="codex-entity-batch"' -SimpleMatch -Quiet)) {
        throw "Entity batch output does not contain updated layer"
    }
}
if ($RunLateExtract) {
    if (-not (Select-String -LiteralPath $LateExtractOutput -Pattern "NELISP-EMACS-LATE" -SimpleMatch -Quiet)) {
        throw "Late extract output does not contain late marker"
    }
}
if ($RunLateBatch) {
    if (-not (Select-String -LiteralPath $LateBatchOutput -Pattern "NELISP-EMACS-LATE-BATCH" -SimpleMatch -Quiet)) {
        throw "Late batch output does not contain replacement marker"
    }
    if (-not (Select-String -LiteralPath $LateBatchOutput -Pattern 'class="codex-late-batch"' -SimpleMatch -Quiet)) {
        throw "Late batch output does not contain updated layer"
    }
}
if ($RunGenerate) {
    if (-not (Select-String -LiteralPath $GeneratedOutput -Pattern "NELISP-EMACS-GENERATE" -SimpleMatch -Quiet)) {
        throw "Generated SVG does not contain generated marker"
    }
}
if ($RunIdentity) {
    if (-not (Select-String -LiteralPath $OsIdentityOutput -Pattern ":system-type windows-nt" -SimpleMatch -Quiet)) {
        throw "Standalone OS identity does not report windows-nt"
    }
    if (-not (Select-String -LiteralPath $OsIdentityOutput -Pattern ':path-separator ";"' -SimpleMatch -Quiet)) {
        throw "Standalone OS identity does not report Windows path-separator"
    }
}

Write-Host "[anvil-standalone-svg-smoke] PASS"
Write-Host ("input : " + $InputSourcePath)
Write-Host ("copy  : " + $InputCopy)
Write-Host ("suite : " + $Suite)
if ($RealInput) {
    Write-Host ("source-sha256: " + $InputSourceHash)
    Write-Host ("copy-sha256  : " + $InputHash)
}
if ($RunAnnotate) {
    Write-Host ("output: " + $Output)
    if ($RealInput) {
        Write-Host ("output-sha256: " + (Get-FileHash -Algorithm SHA256 -LiteralPath $Output).Hash)
    }
}
if ($RunBatch) {
    Write-Host ("batch : " + $BatchOutput)
}
if ($RunEntity) {
    Write-Host ("entity-extract: " + $EntityExtractOutput)
    Write-Host ("entity-batch  : " + $EntityBatchOutput)
}
if ($RunLateExtract) {
    Write-Host ("late-extract: " + $LateExtractOutput)
}
if ($RunLateBatch) {
    Write-Host ("late-batch  : " + $LateBatchOutput)
}
if ($RunGenerate) {
    Write-Host ("gen   : " + $GeneratedOutput)
}
if ($RunOutput) {
    Write-Host ("nelisp: " + ($RunOutput -join "`n"))
}
