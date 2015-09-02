@echo off
setlocal

set EMACS_HOME=%userprofile%\opt\emacs-24.5-IME-patched-generic-cpu

rem == C/Migemo ==
set PATH=%PATH%;%HOME%\opt\cmigemo

rem == Git ==
set PATH=%PATH%;C:\opt\Git\bin

rem == PuTTY ==
rem set PATH=%PATH%;%ProgramFiles(x86)%\PuTTY
set PATH=%PATH%;%HOME%\opt\putty-0.64-jp20150301

rem == Ruby ==
set PATH=%PATH%;C:\Ruby22-x64\bin

rem == Zeal ==
set PATH=%PATH%;%ProgramFiles(x86)%\Zeal

rem == Cygwin ==
rem NTEmacsÇëzíËÇµÇƒÇ¢ÇÈÇΩÇﬂÅACygwinÇ…àÀë∂ÇµÇΩê›íËÇÕÇ»ÇÈÇ◊Ç≠îÇØÇΩÇŸÇ§Ç™ÇÊÇ¢
set CYGWIN_HOME=C:\cygwin64
set PATH=%PATH%;%CYGWIN_HOME%\bin

call "%EMACS_HOME%\bin\runemacs.exe" %*
