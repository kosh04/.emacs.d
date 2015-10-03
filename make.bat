@echo off
setlocal

set PATH=%PATH%;C:\opt\Gow\bin
set PATH=%PATH%;C:\Python27

set EMACS=%~dp0\bin\emacs.bat
set CASK=python %HOME%\.cask\bin\cask

make.exe %*