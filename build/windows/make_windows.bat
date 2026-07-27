@echo off
setlocal EnableDelayedExpansion
::=============================================================================
:: ELMFIRE native Windows build (Intel Fortran ifx + Intel MPI), no Visual
:: Studio IDE required.
::
:: This mirrors the Release|x64 / Debug|x64 configurations of
:: build\visual_studio\elmfire\elmfire.vfproj, but drives ifx directly so the
:: build can run from a plain terminal, a VS Code task, or CI. The .sln remains
:: the IDE-based alternative; keep the two in sync if you change flags.
::
:: Prerequisites
::   * Intel oneAPI Base + HPC Toolkit (provides ifx and Intel MPI).
::   * GDAL command-line tools on PATH at *run* time (gdal_translate, gdalinfo,
::     gdalsrsinfo, gdaltransform) -- ELMFIRE shells out to them.
::
:: Usage (from an ordinary cmd.exe; setvars.bat is invoked automatically if ifx
:: is not already on PATH):
::   make_windows.bat                 build elmfire, Release
::   make_windows.bat debug           build elmfire, Debug
::   make_windows.bat clean           remove obj\ and bin\
::
:: Only elmfire is built. Upstream also builds elmfire_post, but nothing in the
:: WUINITY pipeline invokes it -- the realization runner reads ELMFIRE's GeoTIFF
:: outputs directly -- and skipping it keeps the shipped set down to one exe
:: plus impi.dll. Its sources are still in build\source for the Linux makefile.
::
:: Environment overrides
::   ONEAPI_ROOT     oneAPI install root (used to find setvars.bat)
::   I_MPI_ROOT      Intel MPI root (used for the MPI module/lib paths)
::   ELMFIRE_LOWMEM  if set, build without the _SMOKE/_WUI/_UMDSPOTTING/
::                   _SUPPRESSION precompiler flags, output suffix _lowmem
::=============================================================================

cd /d "%~dp0"

set "REPO_ROOT=%~dp0..\.."
set "SRC=%REPO_ROOT%\build\source"
set "BUILD_TYPE=release"
set "DO_CLEAN="

:: --- Parse arguments -------------------------------------------------------
:parse_args
if "%~1"=="" goto args_done
if /i "%~1"=="release"  set "BUILD_TYPE=release" & shift & goto parse_args
if /i "%~1"=="debug"    set "BUILD_TYPE=debug"   & shift & goto parse_args
if /i "%~1"=="clean"    set "DO_CLEAN=1"         & shift & goto parse_args
echo ERROR: unrecognized argument "%~1"
echo Usage: make_windows.bat [release^|debug] [clean]
exit /b 1
:args_done

if defined DO_CLEAN (
   echo Cleaning obj\ and bin\
   if exist "%~dp0obj" rmdir /s /q "%~dp0obj"
   if exist "%~dp0bin" rmdir /s /q "%~dp0bin"
   exit /b 0
)

:: --- Locate the Intel Fortran compiler -------------------------------------
where ifx >nul 2>&1
if errorlevel 1 (
   echo ifx not on PATH; sourcing oneAPI setvars.bat...
   set "SETVARS="
   if defined ONEAPI_ROOT if exist "%ONEAPI_ROOT%\setvars.bat" set "SETVARS=%ONEAPI_ROOT%\setvars.bat"
   if not defined SETVARS if exist "%ProgramFiles(x86)%\Intel\oneAPI\setvars.bat" set "SETVARS=%ProgramFiles(x86)%\Intel\oneAPI\setvars.bat"
   if not defined SETVARS if exist "%ProgramFiles%\Intel\oneAPI\setvars.bat" set "SETVARS=%ProgramFiles%\Intel\oneAPI\setvars.bat"
   if not defined SETVARS (
      echo ERROR: could not find oneAPI setvars.bat. Install the Intel oneAPI
      echo        HPC Toolkit, or set ONEAPI_ROOT to its install directory.
      exit /b 1
   )
   :: setvars.bat pushd's into each component's env\ and runs `call vars.bat`
   :: relatively; NoDefaultCurrentDirectoryInExePath=1 (set by some shells and
   :: security policies) makes every one of those calls fail. Clear it for the
   :: duration of this script -- setlocal keeps the change local.
   set "NoDefaultCurrentDirectoryInExePath="
   call "!SETVARS!" intel64 >nul
   where ifx >nul 2>&1
   if errorlevel 1 (
      echo ERROR: setvars.bat ran but ifx is still not on PATH.
      exit /b 1
   )
)

:: --- Check for the MSVC toolset --------------------------------------------
:: ifx compiles Fortran on its own but links against the MSVC C runtime and the
:: Windows SDK. Without the "Desktop development with C++" workload installed,
:: compilation succeeds and only the link fails, with cryptic lld-link errors.
set "MSVC_OK="
if defined LIB for %%D in ("%LIB:;=" "%") do if exist "%%~D\libcmt.lib" set "MSVC_OK=1"
if not defined MSVC_OK (
   echo ERROR: MSVC libraries not found ^(libcmt.lib is not on %%LIB%%^).
   echo        ifx links with the Microsoft toolset. Install the "Desktop
   echo        development with C++" workload -- MSVC plus Windows SDK -- from
   echo        the Visual Studio Installer or the standalone Build Tools, then
   echo        re-run this script so setvars.bat can pick it up.
   exit /b 1
)

:: --- Locate Intel MPI (mpi_f08 module files + impi.lib) --------------------
:: elmfire_vars/subs/post all `USE MPI_F08`, so the compiler needs the Intel MPI
:: module directory and the linker needs impi.lib -- exactly what the .vfproj
:: configures via AdditionalIncludeDirectories / AdditionalDependencies.
if not defined I_MPI_ROOT (
   if exist "%ProgramFiles(x86)%\Intel\oneAPI\mpi\latest\include\mpi" (
      set "I_MPI_ROOT=%ProgramFiles(x86)%\Intel\oneAPI\mpi\latest"
   ) else if exist "%ProgramFiles%\Intel\oneAPI\mpi\latest\include\mpi" (
      set "I_MPI_ROOT=%ProgramFiles%\Intel\oneAPI\mpi\latest"
   )
)
if not defined I_MPI_ROOT (
   echo ERROR: Intel MPI not found. Set I_MPI_ROOT to your Intel MPI directory
   echo        ^(the one containing include\mpi and lib^).
   exit /b 1
)
set "MPI_INC=%I_MPI_ROOT%\include\mpi"
set "MPI_LIB=%I_MPI_ROOT%\lib"
if not exist "%MPI_INC%" (
   echo ERROR: MPI module directory not found: !MPI_INC!
   exit /b 1
)
if exist "%MPI_LIB%\release\impi.lib" set "MPI_LIB=%MPI_LIB%\release"
if not exist "%MPI_LIB%\impi.lib" (
   echo ERROR: impi.lib not found under !I_MPI_ROOT!\lib
   exit /b 1
)

:: --- Version banner check --------------------------------------------------
:: make_gnu.sh rewrites VERSIONSTRING in elmfire.f90 from the VERSION file. We
:: only warn here rather than edit: an in-place rewrite on Windows would flip
:: the file to CRLF and dirty the whole file in git.
set "ELMFIRE_VER="
if exist "%REPO_ROOT%\VERSION" set /p ELMFIRE_VER=<"%REPO_ROOT%\VERSION"
if defined ELMFIRE_VER (
   findstr /c:"VERSIONSTRING='ELMFIRE %ELMFIRE_VER%'" "%SRC%\elmfire.f90" >nul 2>&1
   if errorlevel 1 echo WARNING: VERSIONSTRING in elmfire.f90 does not match VERSION ^(%ELMFIRE_VER%^).
)

:: --- Precompiler flags (mirrors make_gnu.sh) -------------------------------
if defined ELMFIRE_LOWMEM (
   echo LOWMEM build, removing precompiler flags: smoke wui umdspotting suppression
   set "DEFS="
   set "SUFFIX=_lowmem"
) else (
   set "DEFS=/D_SMOKE /D_WUI /D_UMDSPOTTING /D_SUPPRESSION"
   set "SUFFIX="
)

:: --- Compiler / linker flags (mirrors elmfire.vfproj x64 configurations) ---
if /i "%BUILD_TYPE%"=="debug" (
   set "FFLAGS=/nologo /fpp /Od /debug:full /Z7 /check:all /traceback /fpe:0 /Qinit:snan,arrays /warn:all /heap-arrays /assume:byterecl /recursive"
   set "LDFLAGS=/DEBUG"
   set "DBG=_debug"
) else (
   set "FFLAGS=/nologo /fpp /O3 /Qipo /fpe:0 /traceback /heap-arrays /assume:byterecl /recursive"
   set "LDFLAGS="
   set "DBG="
)
:: 320 MB reserved stack/heap and /LARGEADDRESSAWARE, as in the .vfproj.
set "LDFLAGS=%LDFLAGS% /STACK:327680000,327680000 /HEAP:327680000,327680000 /LARGEADDRESSAWARE /SUBSYSTEM:CONSOLE /INCREMENTAL:NO"

set "BIN=%~dp0bin"
if not exist "%BIN%" mkdir "%BIN%"

:: ===========================================================================
:: elmfire
:: ===========================================================================
:: Compile order matters: modules must precede their users. Same order as
:: ELMFIRE_OBJECTS in make_gnu.sh.
set "ELMFIRE_SRCS=elmfire_vars.f90 sort.for elmfire_subs.f90 elmfire_init.f90 elmfire_namelists.f90 elmfire_spread_rate.f90 elmfire_ignition.f90 elmfire_io.f90 elmfire_spotting.f90 elmfire_suppression.f90 elmfire_spotting_superseded.f90 elmfire_calibration.f90 elmfire_level_set.f90 elmfire.f90"
if defined ELMFIRE_LOWMEM set "ELMFIRE_SRCS=elmfire_vars.f90 sort.for elmfire_subs.f90 elmfire_init.f90 elmfire_namelists.f90 elmfire_spread_rate.f90 elmfire_ignition.f90 elmfire_io.f90 elmfire_spotting.f90 elmfire_spotting_superseded.f90 elmfire_calibration.f90 elmfire_level_set.f90 elmfire.f90"

call :build_target elmfire "%ELMFIRE_SRCS%" "" || exit /b 1

:: --- Stage the Intel redistributable next to the executable ----------------
:: elmfire.exe imports exactly one non-system DLL, impi.dll -- the Fortran and
:: C runtimes are linked statically. Windows resolves imports from the exe's
:: own directory first, so copying it there makes the binary runnable anywhere.
:: Without it the loader refuses to start the exe at all -- "the code execution
:: cannot proceed because impi.dll was not found" -- on a double-click, and
:: equally in any process that spawns elmfire without having sourced
:: setvars.bat first (WUInity's runner, for one, which inherits its parent's
:: environment, not a oneAPI one). Intel MPI is redistributable under the Intel
:: Simplified Software License; see mpi\...\share\doc\mpi\license.txt.
call :stage_dll impi.dll "%I_MPI_ROOT%\bin" "%I_MPI_ROOT%\bin\release"

echo.
echo Build complete: %BIN%
dir /b "%BIN%\*.exe"
exit /b 0

:: ===========================================================================
:: :stage_dll <dll> [<candidate dir> ...]
:: Copies <dll> into bin\ from the first candidate directory that holds it,
:: falling back to whatever is on PATH (setvars.bat puts both the MPI and the
:: compiler redist directories there). Always overwrites, so a rebuild after a
:: oneAPI upgrade refreshes the staged copy instead of keeping a stale one.
:: Only warns on failure: the build itself succeeded, and the binaries still
:: run in a shell where setvars.bat has been sourced.
:: ===========================================================================
:stage_dll
set "DLL=%~1"
shift
:stage_dll_dirs
if "%~1"=="" goto :stage_dll_path
if exist "%~1\%DLL%" (
   copy /y "%~1\%DLL%" "%BIN%\" >nul
   echo   COPY    %DLL%
   exit /b 0
)
shift
goto :stage_dll_dirs
:stage_dll_path
for /f "delims=" %%I in ('where %DLL% 2^>nul') do (
   copy /y "%%I" "%BIN%\" >nul
   echo   COPY    %DLL% ^(from PATH^)
   exit /b 0
)
echo WARNING: %DLL% not found. The binaries in %BIN% will only run in a shell
echo          where setvars.bat has been sourced.
exit /b 0

:: ===========================================================================
:: :build_target <name> <source list> <extra flags>
:: Compiles each source to obj\<name>-<cfg>\ then links the executable.
:: ===========================================================================
:build_target
set "TGT=%~1"
set "SRCS=%~2"
set "EXTRA=%~3"
set "OBJDIR=%~dp0obj\%TGT%-%BUILD_TYPE%"
set "EXE=%BIN%\%TGT%%SUFFIX%%DBG%.exe"

echo.
echo === Building %TGT% (%BUILD_TYPE%) ===
if exist "%OBJDIR%" rmdir /s /q "%OBJDIR%"
mkdir "%OBJDIR%"

set "OBJS="
for %%F in (%SRCS%) do (
   echo   FC      %%F
   ifx /c %DEFS% %FFLAGS% %EXTRA% /I"%MPI_INC%" /module:"%OBJDIR%" /object:"%OBJDIR%\%%~nF.obj" "%SRC%\%%F"
   if errorlevel 1 (
      echo BUILD FAILED while compiling %%F
      exit /b 1
   )
   set "OBJS=!OBJS! "%OBJDIR%\%%~nF.obj""
)

echo   LD      %TGT%
ifx %FFLAGS% %EXTRA% /exe:"%EXE%" !OBJS! /link /libpath:"%MPI_LIB%" impi.lib %LDFLAGS%
if errorlevel 1 (
   echo BUILD FAILED while linking %TGT%
   exit /b 1
)
exit /b 0
