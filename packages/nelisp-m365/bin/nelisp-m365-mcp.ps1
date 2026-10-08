# Native stdio launcher. Configuration is data, never evaluated as PowerShell.
[CmdletBinding()]
param()
$ErrorActionPreference = 'Stop'
$pkg = Split-Path $PSScriptRoot -Parent
$root = Split-Path (Split-Path $pkg -Parent) -Parent
$utf8 = New-Object System.Text.UTF8Encoding($false)
# Stream.CopyToAsync does not flush the child's buffered stdin FileStream.
# Use dedicated byte pumps, flushing each available chunk in both directions.
if (-not ('NelispM365StdioRelay' -as [type])) {
    Add-Type -TypeDefinition @'
using System.IO;
using System.Threading.Tasks;
public static class NelispM365StdioRelay {
    public static Task Pump(Stream source, Stream destination) {
        return Task.Factory.StartNew(() => {
            byte[] bytes = new byte[4096];
            int count;
            while ((count = source.Read(bytes, 0, bytes.Length)) != 0) {
                destination.Write(bytes, 0, count);
                destination.Flush();
            }
        }, TaskCreationOptions.LongRunning);
    }
}
'@
}
function LispString([string]$value) {
    '"' + $value.Replace('\', '\\').Replace('"', '\"').Replace("`r", '\r').Replace("`n", '\n').Replace("`t", '\t') + '"'
}
$config = @{}
$configFile = $env:M365_CONFIG_FILE
if (!$configFile) { $configFile = Join-Path $env:APPDATA 'nelisp-m365/config' }
$keys = @('NELISP','M365_CLIENT_ID','M365_TENANT','M365_TOKEN_CACHE','M365_ENABLE_WRITE','M365_SCOPES','M365_DOWNLOAD_DIR','M365_LOG_FILE','M365_CURL')
if (Test-Path -LiteralPath $configFile) {
    foreach ($line in [IO.File]::ReadAllLines($configFile, $utf8)) {
        if (!$line.Trim() -or $line.TrimStart().StartsWith('#')) { continue }
        $pair = $line -split '=', 2
        if ($pair.Count -ne 2 -or $keys -notcontains $pair[0].Trim()) { throw 'Invalid nelisp-m365 config entry' }
        $config[$pair[0].Trim()] = $pair[1].Trim()
    }
}
function Setting([string]$key, [string]$fallback) {
    $value = [Environment]::GetEnvironmentVariable($key)
    if ($null -ne $value) { return $value }
    if ($config.ContainsKey($key)) { return $config[$key] }
    return $fallback
}
$state = Join-Path $env:LOCALAPPDATA 'nelisp-m365'
$runtime = Setting 'NELISP' (Join-Path (Split-Path $root -Parent) 'nelisp/target/nelisp.exe')
if (!(Test-Path -LiteralPath $runtime -PathType Leaf)) { throw "NeLisp binary not found: $runtime" }
$readScopes = 'offline_access User.Read Mail.Read Calendars.Read Files.Read Contacts.Read Tasks.Read Notes.Read'
$write = (Setting 'M365_ENABLE_WRITE' '') -eq '1'
$scopes = Setting 'M365_SCOPES' ($readScopes + $(if ($write) { ' Mail.ReadWrite Mail.Send Calendars.ReadWrite Files.ReadWrite Tasks.ReadWrite' }))
$token = [IO.Path]::GetFullPath((Setting 'M365_TOKEN_CACHE' (Join-Path $state 'token.json')))
$download = [IO.Path]::GetFullPath((Setting 'M365_DOWNLOAD_DIR' (Join-Path $state 'downloads')))
$log = Setting 'M365_LOG_FILE' (Join-Path $state 'mcp.log')
if ($log) { $log = [IO.Path]::GetFullPath($log) }
$curl = Setting 'M365_CURL' (Join-Path $env:SystemRoot 'System32/curl.exe')
# Restrict the private bootstrap directory before writing into it.
$temp = Join-Path ([IO.Path]::GetTempPath()) ('nelisp-m365-' + [Guid]::NewGuid().ToString('N'))
$process = $null
$processStarted = $false
$originalInputEncoding = [Console]::InputEncoding
try {
    $sid = [Security.Principal.WindowsIdentity]::GetCurrent().User
    $acl = New-Object Security.AccessControl.DirectorySecurity
    $acl.SetOwner($sid)
    $acl.SetAccessRuleProtection($true, $false)
    $rule = New-Object Security.AccessControl.FileSystemAccessRule($sid, 'FullControl', 'ContainerInherit,ObjectInherit', 'None', 'Allow')
    $acl.AddAccessRule($rule)
    [IO.Directory]::CreateDirectory($temp, $acl) | Out-Null
    $dirs = @((Split-Path $token -Parent), $download)
    if ($log) { $dirs += Split-Path $log -Parent }
    foreach ($dir in $dirs) {
        [IO.Directory]::CreateDirectory($dir) | Out-Null
    }
    if (Test-Path -LiteralPath $token -PathType Leaf) {
        $fileAcl = New-Object Security.AccessControl.FileSecurity
        $fileAcl.SetOwner($sid)
        $fileAcl.SetAccessRuleProtection($true, $false)
        $fileAcl.AddAccessRule((New-Object Security.AccessControl.FileSystemAccessRule($sid, 'FullControl', 'Allow')))
        [IO.File]::SetAccessControl($token, $fileAcl)
    }
    $boot = Join-Path $temp 'boot.el'
    $lines = @('(setq temporary-file-directory ' + (LispString ($temp.Replace('\','/') + '/')) + ')')
    $lines += '(load ' + (LispString (Join-Path $root 'packages/nelisp-json/src/nelisp-json.el')) + ' nil t)'
    foreach ($module in @('compat','curl','auth','graph','tools','mcp')) {
        $lines += '(load ' + (LispString (Join-Path $pkg "src/nelisp-m365-$module.el")) + ' nil t)'
    }
    $lines += '(setq nelisp-m365-compat--windows t nelisp-m365-compat--windows-known t)'
    $lines += '(setq nelisp-m365-compat--curl-cache ' + (LispString $curl) + ')'
    $lines += '(setq nelisp-m365-client-id ' + $(if (Setting 'M365_CLIENT_ID' '') { LispString (Setting 'M365_CLIENT_ID' '') } else { 'nil' }) + ')'
    $lines += '(setq nelisp-m365-tenant ' + (LispString (Setting 'M365_TENANT' 'consumers')) + ')'
    $lines += '(setq nelisp-m365-token-cache-file ' + (LispString $token) + ')'
    $lines += '(setq nelisp-m365-download-dir ' + (LispString $download) + ')'
    $lines += '(setq nelisp-m365-mcp-log-file ' + $(if ($log) { LispString $log } else { 'nil' }) + ')'
    $lines += '(setq nelisp-m365-write-enabled ' + $(if ($write) { 't' } else { 'nil' }) + ')'
    $lines += '(setq nelisp-m365-scopes (list ' + (($scopes.Split(@(' '), [StringSplitOptions]::RemoveEmptyEntries) | ForEach-Object { LispString $_ }) -join ' ') + '))'
    $lines += '(nelisp-m365-mcp-serve)'
    $lines += '(kill-emacs 0)'
    [IO.File]::WriteAllText($boot, ($lines -join "`n") + "`n", $utf8)
    # PowerShell native pipelines decode/re-encode stdout. Copy bytes instead.
    $start = New-Object Diagnostics.ProcessStartInfo
    $start.FileName = $runtime
    $start.Arguments = '--load "' + $boot + '"'
    $start.UseShellExecute = $false
    $start.CreateNoWindow = $true
    $start.RedirectStandardInput = $true
    $start.RedirectStandardOutput = $true
    $start.RedirectStandardError = $true
    $process = New-Object Diagnostics.Process
    $process.StartInfo = $start
    # .NET Framework creates StandardInput's StreamWriter using this encoding
    # and flushes its preamble at startup, even when we only use BaseStream.
    # Select UTF-8 without a BOM before Start, preventing an injected first byte.
    [Console]::InputEncoding = $utf8
    if (!$process.Start()) { throw 'Could not start NeLisp' }
    $processStarted = $true
    $inputCopy = [NelispM365StdioRelay]::Pump([Console]::OpenStandardInput(), $process.StandardInput.BaseStream)
    $outputCopy = [NelispM365StdioRelay]::Pump($process.StandardOutput.BaseStream, [Console]::OpenStandardOutput())
    $errorCopy = [NelispM365StdioRelay]::Pump($process.StandardError.BaseStream, [Console]::OpenStandardError())
    while (!$process.WaitForExit(100)) {
        if ($inputCopy.IsCompleted) { $process.StandardInput.Close() }
    }
    [void]$outputCopy.GetAwaiter().GetResult()
    [void]$errorCopy.GetAwaiter().GetResult()
    $exitCode = $process.ExitCode
} finally {
    if ($process) {
        if ($processStarted -and !$process.HasExited) { $process.Kill(); $process.WaitForExit() }
        $process.Dispose()
    }
    [Console]::InputEncoding = $originalInputEncoding
    if (Test-Path -LiteralPath $temp) {
        $resolvedTemp = [IO.Path]::GetFullPath($temp)
        $tempRoot = [IO.Path]::GetFullPath([IO.Path]::GetTempPath()).TrimEnd('\') + '\'
        if (!$resolvedTemp.StartsWith($tempRoot, [StringComparison]::OrdinalIgnoreCase) -or
            [IO.Path]::GetFileName($resolvedTemp) -notmatch '^nelisp-m365-[0-9a-f]{32}$') { throw 'Unsafe temporary cleanup target' }
        Remove-Item -LiteralPath $resolvedTemp -Recurse -Force
    }
}
exit $exitCode
