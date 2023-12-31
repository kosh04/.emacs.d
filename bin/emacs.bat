@echo off
setlocal
call %~dp0\_env.cmd
%EMACS29_HOME%\bin\emacs.exe %*

