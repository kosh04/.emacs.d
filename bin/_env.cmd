rem Setup NTEmacs environment

set EMACS24_HOME=%USERPROFILE%\opt\emacs-24.5-IME-patched-generic-cpu
set EMACS25_HOME=%USERPROFILE%\opt\emacs-25.2
set EMACS26_HOME=%USERPROFILE%\opt\emacs-26.1-simple_ime-no_symbol
set EMACS26_HOME=%USERPROFILE%\opt\emacs-26.1-i686-minimum-ime
set EMACS26_HOME=%USERPROFILE%\opt\emacs-26.1

rem set EMACSPATH=
rem set EMACSPATH=%EMACS_HOME%\bin

rem == GNU Aspell ==
rem http://aspell.net/win32/
rem WARNING: aspell.exe release 0.60 or greater is required in Emacs26
rem set PATH=%PATH%;C:\opt\Aspell\bin

rem == LLVM/Clang ==
set PATH=%PATH%;C:\opt\LLVM\bin

rem == CMake ==
set PATH=%PATH%;C:\opt\cmake\bin

rem == C/Migemo ==
set PATH=%PATH%;%HOME%\opt\cmigemo

rem == Common-Lisp ==
set PATH=%PATH%;C:\opt\ecl

rem == Go ==
set PATH=%PATH%;%GOROOT%\bin
set PATH=%PATH%;%GOPATH%\bin

rem == PuTTY ==
rem set PATH=%PATH%;%ProgramFiles(x86)%\PuTTY
rem set PATH=%PATH%;%HOME%\opt\putty-0.66-jp20151110
set PATH=%PATH%;%HOME%\opt\putty-0.66-jp20151110

rem == Ruby ==
set PATH=%PATH%;C:\opt\Ruby23-x64\bin

rem SWI-Prolog
set PATH=%PATH%;C:\opt\swipl\bin

rem == Zeal ==
set PATH=%PATH%;%ProgramFiles(x86)%\Zeal

rem == Cygwin ==
rem NOTE: NOT recommended using Cygwin process in NTEmacs
set CYGWIN_HOME=C:\cygwin64
rem set PATH=%PATH%;%CYGWIN_HOME%\bin

rem set PATH=%PATH%;C:\opt\Gow\bin

rem == The Unarchiver ==
rem https://theunarchiver.com/command-line
rem lsar.exe for dired *.rar
set PATH=%PATH%;%HOME%\opt\unar
