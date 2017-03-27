@echo off
setlocal

rem **** Set scipuff executable base directory
rem set BASEDIR=D:\EPRI\SCICHEM3.0b3\bin\windows\x64
set BASEDIR=%CD%

rem **** Set scipuff executable base directory
set BINDIR=%BASEDIR%

rem **** Set scipuff ini file
set INIFILE=%BASEDIR%\scipuff_b3.ini

set PATH=%BINDIR%

echo PATH = %PATH%
echo inifile = %INIFILE%
pause

rem **** Run command line SCIPUFF exe
rem set PrjName=By_size
rem runSCI.exe -I:%INIFILE% -P:%PrjName%

rem **** Run SCIPUFFgui with inifile
START scipuffGUI.exe /I:%INIFILE%

endlocal