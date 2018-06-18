@echo off
setlocal
rem https://gist.github.com/kosh04/37bb86efd8d5f7046a480f96684df4a9

set LLVM_ROOT=C:\opt\LLVM

set PATH=%PATH%;C:\MinGW64\bin
set PATH=%PATH%;%LLVM_ROOT%\bin
set PATH=%PATH%;C:\opt\cmake\bin
set PATH=%PATH%;%HOME%\.emacs.d\bin

set OPTS=^
-DLIBCLANG_LIBRARY="%LLVM_ROOT%/bin/libclang.dll" ^
-DLIBCLANG_INCLUDE_DIR="%LLVM_ROOT%/include" ^
-DCMAKE_INSTALL_PREFIX="%HOME%/.emacs.d/irony/"

set emacs_dir=%HOME%/opt/emacs-25.2

cmake %OPTS% -G "MinGW Makefiles" .. && cmake --build . --use-stderr --config Release --target install
