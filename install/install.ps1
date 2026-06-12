# ELMFIRE installer for Windows (PowerShell)
# Downloads the latest pre-built binary from GitHub Releases.
#
# Usage (run in PowerShell as Administrator):
#   irm https://raw.githubusercontent.com/nick-cloudfire/elmfire/main/install/install.ps1 | iex
#   .\install.ps1                          # install to %LOCALAPPDATA%\ELMFIRE
#   .\install.ps1 -Prefix C:\ELMFIRE       # install to custom directory
#   .\install.ps1 -Version v2026.0519      # install specific version

[CmdletBinding()]
param(
    [string]$Prefix   = "$env:LOCALAPPDATA\ELMFIRE",
    [string]$Version  = "",
    [switch]$AddToPath
)

$ErrorActionPreference = "Stop"
$Repo = "nick-cloudfire/elmfire"
$Asset = "elmfire-windows-x64.zip"
$BinDir = Join-Path $Prefix "bin"

New-Item -ItemType Directory -Force -Path $BinDir | Out-Null

# Resolve version
if (-not $Version) {
    Write-Host "Fetching latest release..."
    $release = Invoke-RestMethod "https://api.github.com/repos/$Repo/releases/latest"
    $Version = $release.tag_name
}

if (-not $Version) {
    Write-Error "Could not determine latest version. Pass -Version vX.Y.Z explicitly."
    exit 1
}

$DownloadUrl = "https://github.com/$Repo/releases/download/$Version/$Asset"
$TmpFile = Join-Path $env:TEMP "elmfire-$Version.zip"

Write-Host "Downloading ELMFIRE $Version..."
Invoke-WebRequest -Uri $DownloadUrl -OutFile $TmpFile -UseBasicParsing

Write-Host "Extracting to $BinDir..."
Expand-Archive -Path $TmpFile -DestinationPath $BinDir -Force
Remove-Item $TmpFile

Write-Host ""
Write-Host "ELMFIRE $Version installed to $BinDir"

# Add to PATH for current user
$CurrentPath = [System.Environment]::GetEnvironmentVariable("Path", "User")
if ($CurrentPath -notlike "*$BinDir*") {
    if ($AddToPath) {
        [System.Environment]::SetEnvironmentVariable("Path", "$CurrentPath;$BinDir", "User")
        Write-Host "Added $BinDir to your user PATH."
        Write-Host "Restart your terminal for the change to take effect."
    } else {
        Write-Host ""
        Write-Host "To add ELMFIRE to your PATH permanently, run:"
        Write-Host "  [System.Environment]::SetEnvironmentVariable('Path', `$env:Path + ';$BinDir', 'User')"
        Write-Host "Or re-run this script with -AddToPath"
    }
}

# Dependency checks
Write-Host ""
$mpirun = Get-Command mpirun -ErrorAction SilentlyContinue
if (-not $mpirun) {
    Write-Warning "Intel MPI runtime not found in PATH."
    Write-Warning "Install Intel oneAPI HPC Toolkit (free) from:"
    Write-Warning "  https://www.intel.com/content/www/us/en/developer/tools/oneapi/hpc-toolkit.html"
    Write-Warning "After installing, source setvars.bat or open the oneAPI command prompt."
}

$gdal = Get-Command gdal_translate -ErrorAction SilentlyContinue
if (-not $gdal) {
    Write-Warning "GDAL not found in PATH."
    Write-Warning "Install OSGeo4W (includes GDAL) from: https://trac.osgeo.org/osgeo4w/"
    Write-Warning "Or install via conda: conda install -c conda-forge gdal"
}

Write-Host ""
Write-Host "Run 'elmfire' in an Intel MPI-enabled terminal to verify the installation."
