rem bin/_env.cmd --- Setup NTEmacs environment

set EMACS_HOME=%USERPROFILE%\opt\emacs-24.5-IME-patched-generic-cpu

rem TODO: do replace PATH to EMACSPATH ?
set EMACSPATH=

rem == GNU Aspell ==
rem http://aspell.net/win32/
set EMACSPATH=%EMACSPATH%;C:\opt\Aspell\bin

rem == C/Migemo ==
set EMACSPATH=%EMACSPATH%;%HOME%\opt\cmigemo

rem == Embeddable Common-Lisp ==
set EMACSPATH=%EMACSPATH%;C:\opt\ecl

rem == Git ==
set EMACSPATH=%EMACSPATH%;C:\opt\Git\bin

rem == PuTTY ==
rem set EMACSPATH=%EMACSPATH%;%ProgramFiles(x86)%\PuTTY
set EMACSPATH=%EMACSPATH%;%HOME%\opt\putty-0.64-jp20150301

rem == Ruby ==
set EMACSPATH=%EMACSPATH%;C:\Ruby22-x64\bin

rem SWI-Prolog
set EMACSPATH=%EMACSPATH%;C:\opt\swipl\bin

rem == Zeal ==
set EMACSPATH=%EMACSPATH%;%ProgramFiles(x86)%\Zeal

rem == Cygwin ==
rem NOTE: NOT recommended using Cygwin process in NTEmacs
set CYGWIN_HOME=C:\cygwin64
rem set EMACSPATH=%EMACSPATH%;%CYGWIN_HOME%\bin
