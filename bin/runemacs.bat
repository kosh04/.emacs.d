@echo off
setlocal

call %~dp0\_env.cmd
%EMACS25_HOME%\bin\runemacs %*
