@echo off
msbuild QuadRope\QuadRope.fsproj
msbuild QuadRope.Test\QuadRope.Test.fsproj
"QuadRope.Test\bin\Debug\QuadRope.Test.exe"
