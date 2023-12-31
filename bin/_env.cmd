rem Setup NTEmacs environment

set EMACS24_HOME=%USERPROFILE%\opt\emacs-24.5-IME-patched-generic-cpu
set EMACS25_HOME=%USERPROFILE%\opt\emacs-25.2
set EMACS26_HOME=%LOCALAPPDATA%\Programs\emacs-26.2
set EMACS27_HOME=%LOCALAPPDATA%\Programs\emacs-27.1-x86_64
set EMACS28_HOME=%LOCALAPPDATA%\Programs\emacs-28.1
set EMACS29_HOME=%LOCALAPPDATA%\Programs\emacs-29.1_2

set EMACS_HOME=%EMACS29_HOME%

rem set EMACSPATH=
rem set EMACSPATH=%EMACS_HOME%\bin

rem == Cygwin ==
rem NOTE: NOT recommended using Cygwin process in NTEmacs
rem set CYGWIN_HOME=C:\cygwin64
rem set PATH=%PATH%;%CYGWIN_HOME%\bin

rem dired の y (ファイルタイプ判別) で file コマンドを利用する
rem UNIX 由来のコマンド群を必要とするパッケージは色々ある (diff,grep,gzip,etc...)

rem call :add_path "C:\opt\Gow\bin"
rem call :add_path "%ProgramFiles%\Git\usr\bin"
call :add_path "C:\tools\git\usr\bin"

rem == GNU Aspell ==
rem http://aspell.net/win32/
rem WARNING: aspell.exe release 0.60 or greater is required in Emacs26
rem set PATH=%PATH%;C:\opt\Aspell\bin

rem == LLVM/Clang ==
call :add_path "C:\opt\LLVM\bin"

rem == CMake ==
call :add_path "C:\opt\cmake\bin"

rem == C/Migemo ==
call :add_path "%HOME%\opt\cmigemo"

rem == Common-Lisp ==
call :add_path "C:\opt\ecl"

rem == Go ==
call :add_path "%GOROOT%\bin"
call :add_path "%GOPATH%\bin"

rem == PuTTY ==
call :add_path "%HOME%\opt\putty-0.66-jp20151110"

rem == Ruby ==
call :add_path "C:\opt\Ruby23-x64\bin"

rem SWI-Prolog
call :add_path "C:\opt\swipl\bin"

rem == Zeal ==
rem set PATH=%PATH%;%ProgramFiles(x86)%\Zeal
call :add_path "%ProgramFiles(x86)%\Zeal"

exit /b

:add_path
if exist %1 set PATH=%PATH%;%~1
if not exist %1 echo Warning: non-existent path %1
exit /b
