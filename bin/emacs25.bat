@echo off
setlocal

call %~dp0\_env.cmd
path %EMACS25_HOME%\bin;%PATH%

emacs.exe %*
