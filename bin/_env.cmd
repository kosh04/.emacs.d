rem bin/_env.cmd --- Setup NTEmacs environment

set EMACS_HOME=%USERPROFILE%\opt\emacs-24.5-IME-patched-generic-cpu

rem == GNU Aspell ==
rem http://aspell.net/win32/
set PATH=%PATH%;C:\opt\Aspell\bin

rem == C/Migemo ==
set PATH=%PATH%;%HOME%\opt\cmigemo

rem == Git ==
set PATH=%PATH%;C:\opt\Git\bin

rem == PuTTY ==
rem set PATH=%PATH%;%ProgramFiles(x86)%\PuTTY
set PATH=%PATH%;%HOME%\opt\putty-0.64-jp20150301

rem == Ruby ==
set PATH=%PATH%;C:\Ruby22-x64\bin

rem SWI-Prolog
set PATH=%PATH%;C:\opt\swipl\bin

rem == Zeal ==
set PATH=%PATH%;%ProgramFiles(x86)%\Zeal

rem == Cygwin ==
rem NOTE: NOT recommended using Cygwin process in NTEmacs
set CYGWIN_HOME=C:\cygwin64
set PATH=%PATH%;%CYGWIN_HOME%\bin
