@echo off
setlocal

set EMACS_HOME=%userprofile%\opt\emacs-24.5-IME-patched-generic-cpu

rem set CYGWIN_HOME=C:\cygwin
set CYGWIN_HOME=C:\cygwin64

rem set PATH=%PATH%;C:\opt\Git\bin
set PATH=%PATH%;%CYGWIN_HOME%\bin

call "%EMACS_HOME%\bin\runemacs.exe" %*
