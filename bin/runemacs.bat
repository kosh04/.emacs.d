@echo off
setlocal

call %~dp0\_env.cmd
%EMACS_HOME%\bin\runemacs %*
