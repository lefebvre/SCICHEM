setlocal
echo off

set COMPILER=intel
set   CONFIG=Release
set PLATFORM=x64
set   VS_VER=vs2013
set  VS_PATH=12.0

set SLN=EPRIintel.sln
SET VERSION_DIR=%CD%
pushd ..\..\
SET SCIPUFF_BASEDIR=%CD%
popd

SET addpath=%SCIPUFF_BASEDIR%\bin\vendor\%PLATFORM%;%VERSION_DIR%\%VS_VER%\bin\%COMPILER%\%PLATFORM%\%CONFIG%
echo Using exe from %VERSION_DIR%\%VS_VER%\bin\%COMPILER%\%PLATFORM%\%CONFIG%
SET path=%addpath%;%path%

START /D "C:\Program Files (x86)\Microsoft Visual Studio %VS_PATH%\Common7\IDE" /B .\devenv.exe %VERSION_DIR%\%VS_VER%\%SLN%
endlocal
