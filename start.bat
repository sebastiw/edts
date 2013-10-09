@echo off
set "EDTS_HOME=%~dp0%"
set "PROJDIR=%1"
if %PROJDIR%=="" (set "PROJDIR=%HOME:\=/%")

set "ERL=%2"
if %ERL%=="" (set ERL="erl.exe")

set PLUGINDIR="%EDTS_HOME:\=/%plugins"
set "subs="

setlocal enabledelayedexpansion

for /f "tokens=*" %%G in ('dir /b /a:d "%EDTS_HOME%lib"') do ^
set "subs=!subs! %EDTS_HOME%lib\%%G\ebin"

call %ERL% ^
-sname edts ^
-edts project_data_dir '%PROJDIR%' ^
-edts plugin_dir '%PLUGINDIR%' ^
-pa %subs% ^
-s edts_app
