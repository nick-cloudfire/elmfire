#!/usr/bin/env bash
# ELMFIRE installer for Linux and macOS
# Downloads the latest pre-built binary from GitHub Releases.
#
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/nick-cloudfire/elmfire/main/install/install.sh | bash
#   bash install.sh                        # auto-detect platform
#   bash install.sh --prefix /opt/elmfire  # install to custom dir
#   bash install.sh --version v2026.0519   # install specific version

set -euo pipefail

REPO="nick-cloudfire/elmfire"
INSTALL_PREFIX="${ELMFIRE_INSTALL_DIR:-$HOME/.local}"
VERSION=""

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --prefix) INSTALL_PREFIX="$2"; shift 2 ;;
    --version) VERSION="$2"; shift 2 ;;
    *) echo "Unknown argument: $1"; exit 1 ;;
  esac
done

BIN_DIR="$INSTALL_PREFIX/bin"
mkdir -p "$BIN_DIR"

# Detect platform
OS=$(uname -s)
ARCH=$(uname -m)

case "$OS" in
  Linux)
    case "$ARCH" in
      x86_64) ASSET="elmfire-linux-x86_64.tar.gz" ;;
      *) echo "Unsupported Linux architecture: $ARCH. Please compile from source."; exit 1 ;;
    esac
    MPI_HINT="sudo apt-get install -y openmpi-bin gdal-bin   # Debian/Ubuntu"
    ;;
  Darwin)
    case "$ARCH" in
      arm64)   ASSET="elmfire-macos-arm64.tar.gz" ;;
      x86_64)  ASSET="elmfire-macos-x86_64.tar.gz" ;;
      *) echo "Unsupported macOS architecture: $ARCH. Please compile from source."; exit 1 ;;
    esac
    MPI_HINT="brew install open-mpi gdal"
    ;;
  *)
    echo "Unsupported OS: $OS. For Windows use install/install.ps1."
    exit 1
    ;;
esac

# Resolve version
if [[ -z "$VERSION" ]]; then
  echo "Fetching latest release..."
  VERSION=$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" \
    | grep '"tag_name"' | head -1 | cut -d'"' -f4)
fi

if [[ -z "$VERSION" ]]; then
  echo "Could not determine latest version. Pass --version vX.Y.Z explicitly."
  exit 1
fi

DOWNLOAD_URL="https://github.com/$REPO/releases/download/$VERSION/$ASSET"
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

echo "Downloading ELMFIRE $VERSION ($ASSET)..."
curl -fsSL --progress-bar "$DOWNLOAD_URL" -o "$TMPDIR/$ASSET"

echo "Extracting to $BIN_DIR..."
tar -xzf "$TMPDIR/$ASSET" -C "$BIN_DIR"
chmod +x "$BIN_DIR"/elmfire*

echo ""
echo "ELMFIRE $VERSION installed to $BIN_DIR"

# PATH check
if [[ ":$PATH:" != *":$BIN_DIR:"* ]]; then
  echo ""
  echo "Add the following to your shell profile (~/.bashrc, ~/.zshrc, etc.):"
  echo "  export PATH=\"$BIN_DIR:\$PATH\""
fi

# Dependency check
echo ""
if ! command -v mpirun &>/dev/null; then
  echo "MPI runtime not found. Install it with:"
  echo "  $MPI_HINT"
fi
if ! command -v gdal_translate &>/dev/null; then
  echo "GDAL not found. Install it with:"
  echo "  $MPI_HINT"
fi

echo ""
echo "Run 'elmfire --help' to verify the installation."
