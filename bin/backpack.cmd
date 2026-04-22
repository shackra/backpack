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

REM --- Convert backslashes to forward slashes for Emacs.
REM     NOTE: do not name this BACKPACK_EMACS_DIR or anything that case-
REM     insensitively matches "emacs_dir" -- on Windows that would shadow
REM     Emacs's own reserved emacs_dir variable (see src/emacs.c
REM     decode_env_path) and break data-directory / exec-directory
REM     resolution, causing fatal "charsets: No such file or directory". ---
set "BACKPACK_DIR_FWD=%BACKPACK_DIR:\=/%"

REM --- Defensive: clear any inherited emacs_dir so a parent shell that
REM     happens to export it cannot poison Emacs's path resolution. ---
set "emacs_dir="

if "%ACTION%"=="ensure" goto :ensure
if "%ACTION%"=="gc" goto :gc
if "%ACTION%"=="bench" goto :bench
echo Error: unknown action '%ACTION%'>&2
echo Available actions: ensure, gc, bench>&2
exit /b 1

:ensure
echo Backpack: Synchronizing packages...
echo Backpack directory: %BACKPACK_DIR%
echo.
emacs --batch --eval "(setq user-emacs-directory \"%BACKPACK_DIR_FWD%/\")" -l "%BACKPACK_DIR_FWD%/ensure.el"
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
emacs --batch --eval "(setq user-emacs-directory \"%BACKPACK_DIR_FWD%/\")" --eval "(setq backpack-gc-dry-run !DRY_RUN!)" -l "%BACKPACK_DIR_FWD%/gc.el"
exit /b %errorlevel%

:bench
echo Backpack: Running benchmark suite...
echo Backpack directory: %BACKPACK_DIR%
echo.
echo Tip: set BACKPACK_BENCH_FILTER=substring to run a subset
echo      set BACKPACK_BENCH_ITERATIONS=N to override the default sample count
echo.
emacs --batch --eval "(setq user-emacs-directory \"%BACKPACK_DIR_FWD%/\")" -l "%BACKPACK_DIR_FWD%/bench.el"
exit /b %errorlevel%

:usage
echo Error: missing argument>&2
echo Usage: backpack ^<action^> [flags]>&2
echo.>&2
echo Available actions:>&2
echo   ensure          Install and build all packages ^(without activation^)>&2
echo   gc [--dry-run]  Remove orphaned packages no longer needed>&2
echo   bench           Run the Backpack benchmark suite ^(A/B vs :os windows^)>&2
exit /b 1