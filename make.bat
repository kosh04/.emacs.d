@echo off
setlocal

set PATH=C:\opt\Gow\bin;%PATH%
set PATH=C:\Python27;%PATH%

set EMACS=%~dp0bin\emacs.bat
set CASK=python %HOME%\.cask\bin\cask

make.exe %*