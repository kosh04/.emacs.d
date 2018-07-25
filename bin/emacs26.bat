@echo off
setlocal

call %~dp0\_env.cmd
%EMACS26_HOME%\bin\emacs %*
