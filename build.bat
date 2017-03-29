@echo off

if "%1"=="-h"     goto HELP
if "%1"=="/h"     goto HELP
if "%1"=="--help" goto HELP

if "%1"=="-r"        goto RELEASE
if "%1"=="/r"        goto RELEASE
if "%1"=="--release" goto RELEASE

if "%1"=="-c"      goto CLEAN
if "%1"=="/c"      goto CLEAN
if "%1"=="--clean" goto CLEAN

if "%1"=="-p"      goto PAKET
if "%1"=="/p"      goto PAKET
if "%1"=="--paket" goto PAKET


@echo on
msbuild /property:Configuration=Debug /m:1
@echo off
goto END

:RELEASE
@echo on
msbuild /property:Configuration=Release /m:1
@echo off
goto END

:CLEAN
@echo on
msbuild /property:Configuration=Debug /t:Clean /m:1
msbuild /property:Configuration=Release /t:Clean /m:1
@echo off
goto END

:PAKET
@echo on
.paket\paket.exe install
@echo off
goto END

:HELP
echo Build Funcalc in debug mode.
echo Usage:
echo  build [OPTION]
echo.
echo Options:
echo   -r  /r  --release  - Build in release mode.
echo   -c  /c  --clean    - Remove all binaries.
echo   -p  /p  --paket    - Install required packages locally.
echo   -h  /h  --help     - Show this message.

:END
