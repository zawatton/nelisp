@echo off
rem anvil.cmd --- Stage D Phase 6.4 Windows wrapper
rem
rem Lets cmd.exe / PowerShell users invoke `anvil` without an msys2
rem shell.  Forwards every argument to the bash launcher under
rem msys2 (mingw64).  Emacs binary is searched in:
rem
rem   1. %EMACS% if set and exists
rem   2. %ProgramFiles%\Emacs\emacs-XX.X\bin\emacs.exe (gnu.org build)
rem   3. C:\msys64\mingw64\bin\emacs.exe (msys2 mingw64)
rem
rem Claude Code .mcp.json snippet (PowerShell):
rem   {
rem     "mcpServers": {
rem       "anvil": {
rem         "command": "C:\\path\\to\\anvil.cmd",
rem         "args": ["mcp", "serve"]
rem       }
rem     }
rem   }

setlocal

rem 1. Locate this script's directory (no trailing backslash).
set "ANVIL_BIN_DIR=%~dp0"
if "%ANVIL_BIN_DIR:~-1%"=="\" set "ANVIL_BIN_DIR=%ANVIL_BIN_DIR:~0,-1%"
set "ANVIL_HOME=%ANVIL_BIN_DIR%\.."

rem 2. Find a bash interpreter (msys2 mingw64 preferred).
set "BASH_EXE="
if exist "C:\msys64\usr\bin\bash.exe" (
  set "BASH_EXE=C:\msys64\usr\bin\bash.exe"
) else if exist "C:\Program Files\Git\bin\bash.exe" (
  set "BASH_EXE=C:\Program Files\Git\bin\bash.exe"
) else (
  for /f "delims=" %%I in ('where bash.exe 2^>nul') do (
    if not defined BASH_EXE set "BASH_EXE=%%I"
  )
)

if not defined BASH_EXE (
  echo error: bash.exe not found.
  echo   anvil.cmd needs msys2 ^(C:\msys64^) or Git Bash ^(C:\Program Files\Git^).
  echo   Install msys2:  https://www.msys2.org/
  echo   then:  pacman -S mingw-w64-x86_64-emacs
  exit /b 1
)

rem 3. Forward to the bash launcher.  ANVIL_HOME is exported so the
rem    bash script can find src/ relative to this .cmd location.
"%BASH_EXE%" "%ANVIL_BIN_DIR%\anvil" %*
exit /b %ERRORLEVEL%
