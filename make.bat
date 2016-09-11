@echo off
setlocal

set PATH=%PATH%;%~dp0bin
REM set PATH=%PATH%;C:\Python27
set PATH=C:\Python27;%PATH%
set PATH=%PATH%;C:\opt\Gow\bin
set PATH=%PATH%;%USERPROFILE%\.cask\bin

make.exe %*
