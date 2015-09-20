@echo off
setlocal

call %~dp0\_env.cmd
call "%EMACS_HOME%\bin\runemacs.exe" %*
