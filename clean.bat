@echo off
msbuild /t:Clean /p:Configuration=Debug
msbuild /t:Clean /p:Configuration=Release
