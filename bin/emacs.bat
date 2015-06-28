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
rem NTEmacs‚ğ‘z’è‚µ‚Ä‚¢‚é‚½‚ßACygwin‚ÉˆË‘¶‚µ‚½İ’è‚Í‚È‚é‚×‚­”ğ‚¯‚½‚Ù‚¤‚ª‚æ‚¢
set CYGWIN_HOME=C:\cygwin64
set PATH=%PATH%;%CYGWIN_HOME%\bin

call "%EMACS_HOME%\bin\runemacs.exe" %*
