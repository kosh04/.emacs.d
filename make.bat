@echo off
setlocal

set PATH=%PATH%;C:\opt\Gow\bin
set PATH=%PATH%;C:\Python27
set PATH=%PATH%;%~dp0bin
set PATH=%PATH%;%USERPROFILE%\.cask\bin

make.exe %*
