@echo off
setlocal

rem **** Set scipuff executable base directory
SET BINDIR=%~dp0%

rem **** Set scipuff ini file
SET INIFILE=%BINDIR%\scipuff.ini

SET path=%addpath%;%path%

rem **** Run SCIPUFFgui with inifile
START %BINDIR%\scipuffGUI.exe /I:%INIFILE%

endlocal