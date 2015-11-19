@echo off
setlocal

set PATH=%PATH%;C:\Python27
set EMACS=%~dp0\emacs.bat

python %HOME%\.cask\bin\cask %*
