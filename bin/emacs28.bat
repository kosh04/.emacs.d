@echo off
setlocal
call %~dp0\_env.cmd
%EMACS28_HOME%\bin\emacs.exe %*
