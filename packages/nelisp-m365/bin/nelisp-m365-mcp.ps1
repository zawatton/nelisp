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
$keys = @('NELISP','M365_CLIENT_ID','M365_TENANT','M365_TOKEN_CACHE','M365_ENABLE_WRITE','M365_SCOPES','M365_DOWNLOAD_DIR','M365_LOG_FILE','M365_CURL','M365_MCP_MODE','NELISP_SERVICE_SRC')
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

# --- Shared mode (default): one daemon for every MCP session -------------
# NeLisp Doc 213.  Instead of starting a NeLisp server per session, this
# PowerShell process is the session's stdio proxy and forwards each JSON-RPC
# line to one shared daemon built on NeLisp's nelisp-service package
# (loopback TCP, one printed Lisp object per line).  The daemon is started
# on demand and replaced when its version -- runtime, sources, settings --
# differs.  M365_MCP_MODE=serve restores one server per session.
$mode = Setting 'M365_MCP_MODE' 'shared'
$serviceSrc = Setting 'NELISP_SERVICE_SRC' (Join-Path (Split-Path $root -Parent) 'nelisp/packages/nelisp-service/src')
if ($mode -eq 'shared' -and (Test-Path -LiteralPath (Join-Path $serviceSrc 'nelisp-service-daemon.el'))) {
    if (-not ('NelispM365Shared' -as [type])) {
        Add-Type -TypeDefinition @'
using System;
using System.IO;
using System.Net.Sockets;
using System.Text;
// Byte-level reader for the nelisp-service wire format: one printed Lisp
// object per line, and a long top-level string sent as
// "(nelisp-service-blob N)" followed after the line by N raw UTF-8 bytes
// and a newline.  A StreamReader cannot count bytes, hence this class.
public class NelispM365Conn {
    readonly NetworkStream stream;
    byte[] buf = new byte[65536];
    int start, end;
    public NelispM365Conn(NetworkStream s) { stream = s; }
    bool Fill() {
        if (start == end) { start = end = 0; }
        if (end == buf.Length) {
            if (start > 0) { Array.Copy(buf, start, buf, 0, end - start); end -= start; start = 0; }
            else { Array.Resize(ref buf, buf.Length * 2); }
        }
        int n = stream.Read(buf, end, buf.Length - end);
        if (n <= 0) return false;
        end += n; return true;
    }
    public string ReadLine() {
        while (true) {
            int i = Array.IndexOf(buf, (byte)10, start, end - start);
            if (i >= 0) {
                int e = (i > start && buf[i - 1] == 13) ? i - 1 : i;
                string line = Encoding.UTF8.GetString(buf, start, e - start);
                start = i + 1; return line;
            }
            if (!Fill()) return null;
        }
    }
    public string ReadBlob(int n) {
        while (end - start < n + 1) { if (!Fill()) throw new IOException("connection closed"); }
        string text = Encoding.UTF8.GetString(buf, start, n);
        start += n + 1; return text;
    }
    public void WriteLine(string line) {
        byte[] b = Encoding.UTF8.GetBytes(line + "\n");
        stream.Write(b, 0, b.Length); stream.Flush();
    }
    // The payload after PREFIX ("(res 3 "): a Lisp string literal or a blob.
    public string ReadPayload(string line, int at) {
        const string blob = "(nelisp-service-blob ";
        if (string.CompareOrdinal(line, at, blob, 0, blob.Length) == 0) {
            int close = line.IndexOf(')', at);
            return ReadBlob(int.Parse(line.Substring(at + blob.Length, close - at - blob.Length)));
        }
        return NelispM365Shared.ReadString(line, at);
    }
}
public static class NelispM365Shared {
    // Read the Lisp string literal starting at line[start] == '"'.
    // nelisp-service escapes only \ and " (prin1) plus \n and \r itself.
    public static string ReadString(string line, int start) {
        var sb = new StringBuilder();
        for (int i = start + 1; i < line.Length; i++) {
            char c = line[i];
            if (c == '"') return sb.ToString();
            if (c == '\\' && i + 1 < line.Length) {
                char e = line[++i];
                if (e == 'n') sb.Append('\n');
                else if (e == 'r') sb.Append('\r');
                else if (e == 't') sb.Append('\t');
                else sb.Append(e);
            } else sb.Append(c);
        }
        throw new FormatException("unterminated string");
    }
}
'@
    }
    $svcState = Join-Path $state 'service'
    $svcTemp = Join-Path $svcState 'tmp'
    foreach ($dir in @($svcState, $svcTemp, (Split-Path $token -Parent), $download)) {
        [IO.Directory]::CreateDirectory($dir) | Out-Null
    }
    if ($log) { [IO.Directory]::CreateDirectory((Split-Path $log -Parent)) | Out-Null }
    $lockFile = Join-Path $svcState 'm365.lock'
    $stateFile = Join-Path $svcState 'm365.state'
    # Version: everything that changes what the daemon would answer.
    $sha = [Security.Cryptography.SHA256]::Create()
    $material = New-Object Text.StringBuilder
    $sources = @(Get-ChildItem -LiteralPath (Join-Path $pkg 'src') -Filter '*.el') + @(Get-ChildItem -LiteralPath $serviceSrc -Filter '*.el')
    foreach ($f in ($sources | Sort-Object FullName)) {
        [void]$material.Append($f.Name).Append([IO.File]::ReadAllText($f.FullName, $utf8))
    }
    [void]$material.Append((Get-Item -LiteralPath $runtime).LastWriteTimeUtc.Ticks)
    foreach ($v in @($scopes, $write, (Setting 'M365_CLIENT_ID' ''), (Setting 'M365_TENANT' 'consumers'), $token, $download, $log, $curl)) {
        [void]$material.Append('|').Append($v)
    }
    $version = -join ($sha.ComputeHash($utf8.GetBytes($material.ToString())) | Select-Object -First 8 | ForEach-Object { $_.ToString('x2') })
    # Daemon bootstrap: take the lock before loading anything, so a second
    # daemon started in the same window exits at once.
    # One bootstrap per version: its content is fixed by the version, so
    # concurrent sessions never rewrite a file another one is starting from.
    $daemonBoot = Join-Path $svcState ('m365-daemon-boot-' + $version + '.el')
    $d = @('(setq temporary-file-directory ' + (LispString ($svcTemp.Replace('\','/') + '/')) + ')')
    $d += '(add-to-list (quote load-path) ' + (LispString $serviceSrc) + ')'
    $d += '(require (quote nelisp-service-daemon))'
    $d += '(setq nelisp-service-state-directory ' + (LispString $svcState) + ')'
    $d += '(when (nelisp-service-daemon-acquire-lock "m365" 300) (progn'
    $d += '(load ' + (LispString (Join-Path $root 'packages/nelisp-json/src/nelisp-json.el')) + ' nil t)'
    foreach ($module in @('compat','curl','auth','graph','tools','mcp')) {
        $d += '(load ' + (LispString (Join-Path $pkg "src/nelisp-m365-$module.el")) + ' nil t)'
    }
    $d += '(setq nelisp-m365-compat--windows t nelisp-m365-compat--windows-known t)'
    $d += '(setq nelisp-m365-compat--curl-cache ' + (LispString $curl) + ')'
    $d += '(setq nelisp-m365-client-id ' + $(if (Setting 'M365_CLIENT_ID' '') { LispString (Setting 'M365_CLIENT_ID' '') } else { 'nil' }) + ')'
    $d += '(setq nelisp-m365-tenant ' + (LispString (Setting 'M365_TENANT' 'consumers')) + ')'
    $d += '(setq nelisp-m365-token-cache-file ' + (LispString $token) + ')'
    $d += '(setq nelisp-m365-download-dir ' + (LispString $download) + ')'
    $d += '(setq nelisp-m365-mcp-log-file ' + $(if ($log) { LispString $log } else { 'nil' }) + ')'
    $d += '(setq nelisp-m365-write-enabled ' + $(if ($write) { 't' } else { 'nil' }) + ')'
    $d += '(setq nelisp-m365-scopes (list ' + (($scopes.Split(@(' '), [StringSplitOptions]::RemoveEmptyEntries) | ForEach-Object { LispString $_ }) -join ' ') + '))'
    $d += '(nelisp-m365-mcp-log "shared daemon %s starting" ' + (LispString $version) + ')'
    $d += '(nelisp-service-daemon-run (nelisp-service-daemon-start "m365" :lock-held t :version ' + (LispString $version) +
          ' :idle-timeout 28800 :handler (lambda (_conn payload reply) (funcall reply (nelisp-m365-mcp-respond payload)) (nelisp-m365-mcp-collect-garbage))))))'
    $d += '(kill-emacs 0)'
    if (!(Test-Path -LiteralPath $daemonBoot)) {
        $tmpBoot = $daemonBoot + '.' + $PID
        [IO.File]::WriteAllText($tmpBoot, ($d -join "`n") + "`n", $utf8)
        try { [IO.File]::Move($tmpBoot, $daemonBoot) } catch { Remove-Item -LiteralPath $tmpBoot -Force -ErrorAction SilentlyContinue }
    }
    # Bootstraps of other versions are leftovers once no daemon runs them.
    Get-ChildItem -LiteralPath $svcState -Filter 'm365-daemon-boot*.el' | Where-Object { $_.FullName -ne $daemonBoot } |
        ForEach-Object { try { Remove-Item -LiteralPath $_.FullName -Force -ErrorAction Stop } catch { } }

    function Read-Plist([string]$file) {
        if (!(Test-Path -LiteralPath $file)) { return $null }
        try { return [IO.File]::ReadAllText($file, $utf8) } catch { return $null }
    }
    function Lock-Age {
        $text = Read-Plist $lockFile
        if (!$text) { return $null }
        if ($text -match ':started ([0-9.eE+]+)') {
            return ([DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds() / 1000.0) - [double]$Matches[1]
        }
        return 1e9
    }
    function Open-Daemon {
        # Returns a connection hashtable, 'version', or $null.
        $text = Read-Plist $stateFile
        if (!$text -or $text -notmatch ':port (\d+)') { return $null }
        $port = [int]$Matches[1]
        if ($text -notmatch ':token "([^"]*)"') { return $null }
        $tok = $Matches[1]
        try {
            $client = [Net.Sockets.TcpClient]::new()
            $client.Connect('127.0.0.1', $port)
            $stream = $client.GetStream()
            $stream.ReadTimeout = 10000
            $wire = [NelispM365Conn]::new($stream)
            $wire.WriteLine('(hello ' + (LispString $tok) + ' ' + (LispString $version) + ')')
            $reply = $wire.ReadLine()
            if ($reply -and $reply.StartsWith('(welcome')) {
                $stream.ReadTimeout = [Threading.Timeout]::Infinite
                return @{ client = $client; wire = $wire }
            }
            $client.Close()
            if ($reply -and $reply.StartsWith('(reject version')) { return 'version' }
        } catch { }
        return $null
    }
    function Connect-Daemon {
        $deadline = [DateTime]::UtcNow.AddSeconds(180)
        $spawnedAt = $null
        while ([DateTime]::UtcNow -lt $deadline) {
            $age = Lock-Age
            if (Test-Path -LiteralPath $stateFile) {
                $conn = Open-Daemon
                if ($conn -is [hashtable]) { return $conn }
                if ($conn -eq 'version') {
                    # The old daemon exits to make room; start ours once it is gone.
                    $wait = [DateTime]::UtcNow.AddSeconds(15)
                    while ((Test-Path -LiteralPath $lockFile) -and [DateTime]::UtcNow -lt $wait) { Start-Sleep -Milliseconds 200 }
                    $spawnedAt = $null; continue
                }
                if ($null -eq $age -or $age -gt 300) {
                    Remove-Item -LiteralPath $stateFile, $lockFile -Force -ErrorAction SilentlyContinue
                } else { Start-Sleep -Milliseconds 250 }
            } elseif ($null -ne $age) {
                if ($age -gt 300) { Remove-Item -LiteralPath $lockFile -Force -ErrorAction SilentlyContinue }
                else { Start-Sleep -Milliseconds 250 }
            } elseif ($null -eq $spawnedAt -or ([DateTime]::UtcNow - $spawnedAt).TotalSeconds -gt 300) {
                # Start-Process uses ShellExecute, so the daemon inherits none
                # of this session's handles and outlives it.
                Start-Process -FilePath $runtime -ArgumentList ('--load "' + $daemonBoot + '"') -WindowStyle Hidden | Out-Null
                $spawnedAt = [DateTime]::UtcNow
            } else { Start-Sleep -Milliseconds 250 }
        }
        throw 'nelisp-m365: could not reach the shared daemon'
    }
    function Send-Request($conn, [int]$id, [string]$json) {
        $conn.wire.WriteLine('(req ' + $id + ' ' + (LispString $json) + ')')
        while ($true) {
            $line = $conn.wire.ReadLine()
            if ($null -eq $line) { throw 'daemon connection closed' }
            if ($line.StartsWith("(res $id ")) {
                return $conn.wire.ReadPayload($line, ("(res $id ").Length)
            }
            if ($line.StartsWith("(err $id ")) {
                $msg = $conn.wire.ReadPayload($line, ("(err $id ").Length)
                return '{"jsonrpc":"2.0","id":null,"error":{"code":-32603,"message":' +
                       (ConvertTo-Json $msg -Compress) + '}}'
            }
        }
    }
    [Console]::InputEncoding = $utf8
    $stdin = New-Object IO.StreamReader([Console]::OpenStandardInput(), $utf8)
    $stdout = [Console]::OpenStandardOutput()
    # Connect before the first message so a cold daemon starts while the
    # client is still launching.
    $conn = Connect-Daemon
    $next = 0
    while ($null -ne ($line = $stdin.ReadLine())) {
        if (!$line.Trim()) { continue }
        $next++
        try { $reply = Send-Request $conn $next $line }
        catch {
            # The daemon went away (idle exit, replacement): resend once.
            try { $conn.client.Close() } catch { }
            $conn = Connect-Daemon
            $reply = Send-Request $conn $next $line
        }
        if ($reply) {
            $bytes = $utf8.GetBytes($reply + "`n")
            $stdout.Write($bytes, 0, $bytes.Length); $stdout.Flush()
        }
    }
    try { $conn.client.Close() } catch { }
    exit 0
}

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
