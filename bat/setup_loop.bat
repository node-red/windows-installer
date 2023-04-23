:: Excellent Syntax Reference:
:: https://ss64.com

@echo off
setlocal EnableDelayedExpansion

:: %1: Filter criteria for the tasklist command
IF %1.==. EXIT
:: %2: tag to look for in the output of tasklist command
IF %2.==. EXIT

echo|set /p="Waiting for the Visual Studio Installer to finish its job."
:: give the installer time to settle...
TIMEOUT 2 > nul

:check_for_installer_running

SET _continue=false

FOR /F "tokens=*" %%G IN ('tasklist /FI %1 /FO Table /NH') DO (
    CALL :check_loop "%%G" %2
    if errorlevel 1 SET _continue=true
)

IF !_continue!==true (
    echo|set /p="."
    TIMEOUT 3 > nul
    GOTO :check_for_installer_running
)
GOTO :EOF

:check_loop
    setlocal
    SET _line=%1
    SET _check=%2

    :: Try to replace %2 in the incoming string (%1)
    :: Set an errorlevel in case the result is different == the substring search was successful!
    CALL SET _result=%%_line:%_check%=%%
    If /i %_result% neq %_line% ( EXIT /b 1)
    GOTO :EOF