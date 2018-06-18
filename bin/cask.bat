@echo off
setlocal

set PATH=C:\Python27;%PATH%
set PATH=%PATH%;%USERPROFILE%\.cask\bin

set EMACS=%~dp0\emacs.bat

cask %*
