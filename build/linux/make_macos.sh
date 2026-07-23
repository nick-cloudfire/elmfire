#!/bin/bash

# ELMFIRE macOS build wrapper.
#
# macOS ships a POSIX userland, so once the GNU-only bits in make_gnu.sh are
# made portable (readlink/sed) the same GNU/gfortran build path works here.
# This wrapper only sets the macOS-specific bits and then delegates to
# make_gnu.sh (which lives alongside this script; the "linux" directory name is
# historical and now hosts the shared GNU build for both Linux and macOS).
#
# Prerequisites (Homebrew):
#   brew install gcc open-mpi gdal
# Homebrew installs gfortran/mpifort on PATH. If your Homebrew Fortran compiler
# is versioned (e.g. gfortran-14), export ELMFIRE_FCOMPL_SERIAL_GNU /
# ELMFIRE_FCOMPL_MPI_GNU to point at it before running this script.
#
# Usage mirrors make_gnu.sh:
#   ./make_macos.sh            # full build
#   ./make_macos.sh elmfire    # fast build (main elmfire executable only)

# Operate from this script's directory (portable; no readlink -f).
cd "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Architecture tuning. Apple Silicon (arm64) gfortran rejects -march=native, so
# use -mcpu=native there; Intel Macs use the make_gnu.sh default (-march=native).
ARCH="$(uname -m)"
if [ "$ARCH" = "arm64" ]; then
   export ELMFIRE_ARCH="${ELMFIRE_ARCH:--mcpu=native}"
   echo "Detected Apple Silicon (arm64); using ELMFIRE_ARCH=$ELMFIRE_ARCH"
else
   echo "Detected Intel macOS ($ARCH); using make_gnu.sh default ELMFIRE_ARCH (-march=native)"
fi

# Compiler defaults (Homebrew). Override by exporting these before invocation.
export ELMFIRE_FCOMPL_SERIAL_GNU=${ELMFIRE_FCOMPL_SERIAL_GNU:-gfortran}
export ELMFIRE_FCOMPL_MPI_GNU=${ELMFIRE_FCOMPL_MPI_GNU:-mpifort}

exec ./make_gnu.sh "$@"
