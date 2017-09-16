@echo off
setlocal

call %~dp0\_env.cmd
%EMACS24_HOME%\bin\emacs %*
