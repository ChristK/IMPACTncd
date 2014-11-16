@ECHO OFF
TITLE IMPACTncd by Chris Kypridemos
mode con:cols=67 lines=17
COLOR 3
REM goto permissionCheck

REM :permissionCheck
    REM echo Checking permissions...

    REM SET adminRights=0
    REM FOR /F %%i IN ('WHOAMI /PRIV /NH') DO (
        REM IF "%%i"=="SeTakeOwnershipPrivilege" SET adminRights=1
    REM )

    REM IF %adminRights% == 1 (
        REM echo Elevated permissions confirmed.

        REM goto checkPrivileges
    REM ) ELSE (
        REM echo Elevated permissions absent.
        REM echo.
        REM echo This file needs to be run as an Administrator.
        REM echo.
        REM echo Close this window, right-click on the file and click "Run as Administrator".
        REM echo   OR
        REM echo Log on to an Administrator account and run this file normally.

 
    REM )

REM :checkPrivileges
REM mkdir "%windir%\OEAdminCheck"
REM if '%errorlevel%' == '0' (
REM rmdir "%windir%\OEAdminCheck" & goto gotPrivileges 
REM ) else ( goto getPrivileges )

REM :getPrivileges
CLS
ECHO.
ECHO.
ECHO IMPACTncd
ECHO.
ECHO.
REM ECHO *****************************************************************
REM ECHO.
REM ECHO Administrator Rights are required for IMPACTncd
REM ECHO Invoking UAC for Privilege Escalation
REM ECHO.
REM ECHO *****************************************************************
REM ECHO.
REM ECHO.
REM ECHO.
REM ECHO.
REM ECHO.

REM ECHO Set UAC = CreateObject^("Shell.Application"^) > "%temp%\OEgetPrivileges.vbs"
REM ECHO UAC.ShellExecute %0, "", "", "runas", 1 >> "%temp%\OEgetPrivileges.vbs"
REM "%temp%\OEgetPrivileges.vbs"
REM exit /B

:gotPrivileges
REM pause >nul
REM "C:\Program files\R\R-3.1.1\bin\x64\Rscript.exe"  --default-packages=("data.table", "dplyr", "truncnorm", "stringr", "reshape2", "compiler", "survey", "doParallel", "doRNG", "foreach")  "%~dp0\IMPACTncd.R"
"C:\Program files\R\R-3.1.1\bin\x64\Rscript.exe" "%~dp0\IMPACTncdgui.R"
REM pause >nul

:END
ECHO.
ECHO.
ECHO IMPACTncd
ECHO.
ECHO.
ECHO *****************************************************************
ECHO.
ECHO The output files have been created in the %~dp0/output folder
ECHO.
ECHO *****************************************************************
ECHO.
ECHO.
ECHO.
ECHO.
