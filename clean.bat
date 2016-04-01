@echo off
mxbuild /t:Clean /p:Configuration=Debug
msbuild /t:Clean /p:Configuration=Release
