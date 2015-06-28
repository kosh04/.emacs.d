@echo off
setlocal

set EMACS_HOME=%userprofile%\opt\emacs-24.5-IME-patched-generic-cpu

rem == Git ==
set PATH=%PATH%;C:\opt\Git\bin

rem == PuTTY ==
rem set PATH=%PATH%;%ProgramFiles(x86)%\PuTTY
set PATH=%PATH%;%HOME%\opt\putty-0.64-jp20150301

rem == Zeal ==
set PATH=%PATH%;%ProgramFiles(x86)%\Zeal

rem == Cygwin ==
rem NTEmacs��z�肵�Ă��邽�߁ACygwin�Ɉˑ������ݒ�͂Ȃ�ׂ��������ق����悢
set CYGWIN_HOME=C:\cygwin64
set PATH=%PATH%;%CYGWIN_HOME%\bin

call "%EMACS_HOME%\bin\runemacs.exe" %*
