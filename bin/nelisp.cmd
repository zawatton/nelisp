@echo off
setlocal
if not defined PYTHON set "PYTHON=python"
"%PYTHON%" "%~dp0..\tools\nelisp-project.py" %*
exit /b %ERRORLEVEL%
