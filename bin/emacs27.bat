@echo off
setlocal

call %~dp0\_env.cmd
%EMACS27_HOME%\bin\emacs %*
