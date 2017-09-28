@echo off
rem
rem SUBDUE Release 4.2.d.2
rem

rem
rem OS is used to decide if batch file has to do any work
rem to initialize C setup.  NT does not require
rem any setup while 95/98 has some interesting twists...
rem
if "%1" == "/?" goto INTRO
if "%OS%" == "Windows_NT" goto SWITCH

rem ***********
rem * MS VC++ *
rem ***********
rem Check if vcvars32.bat has been executed
rem MSVCDIR is defined in vcvars.bat, so test for it...
rem if found - vcvars32.bat already initialized VC
rem so skip VC stuff below
if exist "%MSVCDIR%\bin\vcvars32.bat" goto SWITCH

echo.
echo VC++ compiler not found.
echo.

:INTRO
echo This is a script for compiling SUBDUE.
echo Type one of the following at the DOS command line.
echo.
echo make            or
echo make Subdue     -- compiles the serial version of SUBDUE
echo. 
echo make PVMSubdue  -- compiles the parallel version of SUBDUE
echo                    PVM 3.3 or newer must be installed and
echo                    configured for this to work!
echo.
echo make clean      -- deletes all object and executable files
echo.
goto ENDE

:SWITCH
if "%1" == "" goto Subdue
if "%1" == "Subdue" goto Subdue
if "%1" == "PVMSubdue" goto PVMSubdue
if "%1" == "clean" goto CLEAN

echo Your make "option" specification did not match. RETRY...
echo.
goto INTRO

:Subdue
echo.
echo Compiling Subdue
echo ------------------
nmake /f makefile.mak CFG="d4R - Win32 Release"
echo.
echo Compilation done.  See readme.txt for details.
echo The executable is in .\Release.
goto ENDE

:PVMSubdue
echo.
echo Compiling PVMSubdue
echo ------------------
nmake /f makefile.mak CFG="d4R - Win32 PVM Release"
copy PVM_Release\PVMSubdue.exe %PVM_ROOT%\bin\%PVM_ARCH%\
echo.
echo Compilation done.  See readme.txt for details.
echo The executable is in .\PVM_Release.
echo.
goto ENDE

:CLEAN
echo.
echo Cleaning...
echo --------------------
nmake /f makefile.mak CLEAN
echo.
echo Done.
echo.


:ENDE
