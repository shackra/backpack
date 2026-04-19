@echo off
setlocal enabledelayedexpansion

if "%~1"=="" goto :usage
set "ACTION=%~1"
set "FLAG=%~2"

REM --- Resolve Backpack root from script location ---
set "BACKPACK_DIR=%~dp0.."
for %%I in ("%BACKPACK_DIR%") do set "BACKPACK_DIR=%%~fI"

REM --- Verify installation ---
if not exist "%BACKPACK_DIR%\ensure.el" (
    echo Error: This script must be run from the Backpack installation directory>&2
    echo Expected to find ensure.el in: %BACKPACK_DIR%>&2
    exit /b 1
)
if not exist "%BACKPACK_DIR%\base-packages" (
    echo Error: This script must be run from the Backpack installation directory>&2
    echo Expected to find base-packages\ in: %BACKPACK_DIR%>&2
    exit /b 1
)

REM --- Convert backslashes to forward slashes for Emacs ---
set "EMACS_DIR=%BACKPACK_DIR:\=/%"

if "%ACTION%"=="ensure" goto :ensure
if "%ACTION%"=="gc" goto :gc
echo Error: unknown action '%ACTION%'>&2
echo Available actions: ensure, gc>&2
exit /b 1

:ensure
echo Backpack: Synchronizing packages...
echo Backpack directory: %BACKPACK_DIR%
echo.
emacs --batch --eval "(setq user-emacs-directory \"%EMACS_DIR%/\")" -l "%EMACS_DIR%/ensure.el"
exit /b %errorlevel%

:gc
set "DRY_RUN=nil"
if "%FLAG%"=="--dry-run" set "DRY_RUN=t"
if "%FLAG%"=="-n" set "DRY_RUN=t"
if "!DRY_RUN!"=="t" (
    echo Backpack: Garbage collection ^(dry run^)...
) else (
    echo Backpack: Garbage collection...
)
echo Backpack directory: %BACKPACK_DIR%
echo.
emacs --batch --eval "(setq user-emacs-directory \"%EMACS_DIR%/\")" --eval "(setq backpack-gc-dry-run !DRY_RUN!)" -l "%EMACS_DIR%/gc.el"
exit /b %errorlevel%

:usage
echo Error: missing argument>&2
echo Usage: backpack ^<action^> [flags]>&2
echo.>&2
echo Available actions:>&2
echo   ensure          Install and build all packages ^(without activation^)>&2
echo   gc [--dry-run]  Remove orphaned packages no longer needed>&2
exit /b 1