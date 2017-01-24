@echo off
setlocal

call %~dp0\_env.cmd
path %EMACS24_HOME%\bin;%PATH%

emacs.exe %*
