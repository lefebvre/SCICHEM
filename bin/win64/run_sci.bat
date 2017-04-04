@echo off
setlocal

SET SCIPUFF_BASEDIR=C:\SCICHEM

rem **** Set scipuff executable base directory
SET BINDIR=%SCIPUFF_BASEDIR%\bin\windows\x64

rem **** Set scipuff ini file
SET INIFILE=%BINDIR%\scipuff.ini

SET path=%addpath%;%path%

rem **** Run command line SCIPUFF exe
%BINDIR%/runSCI.exe -I:%INIFILE% -P:Scen01

endlocal