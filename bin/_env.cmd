rem Setup NTEmacs environment

set EMACS_HOME=%USERPROFILE%\opt\emacs-24.5-IME-patched-generic-cpu

rem TODO: do replace PATH to EMACSPATH ?
set EMACSPATH=

rem == GNU Aspell ==
rem http://aspell.net/win32/
set EMACSPATH=%EMACSPATH%;C:\opt\Aspell\bin

rem == LLVM/Clang ==
set EMACSPATH=%EMACSPATH%;C:\opt\LLVM\bin

rem == C/Migemo ==
set EMACSPATH=%EMACSPATH%;%HOME%\opt\cmigemo

rem == Common-Lisp ==
set EMACSPATH=%EMACSPATH%;C:\opt\ecl

rem == Git ==
set EMACSPATH=%EMACSPATH%;C:\opt\Git\bin

rem == Go ==
set EMACSPATH=%EMACSPATH%;%GOROOT%\bin
set EMACSPATH=%EMACSPATH%;%GOPATH%\bin

rem == PuTTY ==
rem set EMACSPATH=%EMACSPATH%;%ProgramFiles(x86)%\PuTTY
rem set EMACSPATH=%EMACSPATH%;%HOME%\opt\putty-0.66-jp20151110
set PATH=%PATH%;%HOME%\opt\putty-0.66-jp20151110

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
