#!/bin/bash
# Build ELMFIRE on macOS using gfortran (Homebrew gcc) + OpenMPI
#
# Prerequisites (install once with Homebrew):
#   brew install gcc open-mpi gdal
#
# Optional env overrides:
#   export ELMFIRE_FCOMPL_MPI_GNU=mpifort
#   export ELMFIRE_INSTALL_DIR=/usr/local/bin

ELMFIRE_VER=2026.0519.memopt

export ELMFIRE_FCOMPL_SERIAL_GNU=${ELMFIRE_FCOMPL_SERIAL_GNU:-gfortran}
export ELMFIRE_FCOMPL_MPI_GNU=${ELMFIRE_FCOMPL_MPI_GNU:-mpifort}
export ELMFIRE_INSTALL_DIR=${ELMFIRE_INSTALL_DIR:-$(pwd)/bin}

mkdir -p "$ELMFIRE_INSTALL_DIR"

if [ -z ${ELMFIRE_LOWMEM} ]; then
    export ELMFIRE_PRECOMPILER_FLAGS="-D_SMOKE -D_WUI -D_UMDSPOTTING -D_SUPPRESSION"
    export ELMFIRE_BIN_SUFFIX=""
    export ELMFIRE_OBJECTS="elmfire_vars.o sort.o elmfire_subs.o elmfire_init.o elmfire_namelists.o elmfire_spread_rate.o  elmfire_ignition.o elmfire_io.o elmfire_spotting.o elmfire_suppression.o elmfire_spotting_superseded.o elmfire_calibration.o elmfire_level_set.o elmfire.o"
else
    echo "LOWMEM build, removing precompiler flags: smoke wui umdspotting suppression"
    export ELMFIRE_PRECOMPILER_FLAGS=""
    export ELMFIRE_BIN_SUFFIX="_lowmem"
    export ELMFIRE_OBJECTS="elmfire_vars.o sort.o elmfire_subs.o elmfire_init.o elmfire_namelists.o elmfire_spread_rate.o  elmfire_ignition.o elmfire_io.o elmfire_spotting.o elmfire_spotting_superseded.o elmfire_calibration.o elmfire_level_set.o elmfire.o"
fi

MAKEFILE_DIR="$(cd "$(dirname "$0")/../linux" && pwd)"

echo "Making elmfire (macOS, gfortran+OpenMPI)"
mkdir -p elmfire_build && cd elmfire_build
rm -f *.o *.mod elmfire
make -f "$MAKEFILE_DIR/Makefile_elmfire" gnu_mpi_macos
cp -f elmfire "$ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER"
ln -fs "$ELMFIRE_INSTALL_DIR/elmfire_$ELMFIRE_VER" "$ELMFIRE_INSTALL_DIR/elmfire$ELMFIRE_BIN_SUFFIX"

echo "Making elmfire_debug (macOS, gfortran+OpenMPI)"
rm -f *.o *.mod elmfire_debug
make -f "$MAKEFILE_DIR/Makefile_elmfire" gnu_mpi_debug_macos
cp -f elmfire_debug "$ELMFIRE_INSTALL_DIR/elmfire_debug_$ELMFIRE_VER"
ln -fs "$ELMFIRE_INSTALL_DIR/elmfire_debug_$ELMFIRE_VER" "$ELMFIRE_INSTALL_DIR/elmfire_debug$ELMFIRE_BIN_SUFFIX"
rm -f *.o *.mod

echo "Making elmfire_post (macOS)"
cd ..
mkdir -p elmfire_post_build && cd elmfire_post_build
rm -f *.o *.mod elmfire_post
make -f "$MAKEFILE_DIR/Makefile_elmfire_post" gnu_macos
cp -f elmfire_post "$ELMFIRE_INSTALL_DIR/elmfire_post_$ELMFIRE_VER"
ln -fs "$ELMFIRE_INSTALL_DIR/elmfire_post_$ELMFIRE_VER" "$ELMFIRE_INSTALL_DIR/elmfire_post$ELMFIRE_BIN_SUFFIX"

echo "Making elmfire_post_debug (macOS)"
rm -f *.o *.mod elmfire_post_debug
make -f "$MAKEFILE_DIR/Makefile_elmfire_post" gnu_debug_macos
cp -f elmfire_post_debug "$ELMFIRE_INSTALL_DIR/elmfire_post_debug_$ELMFIRE_VER"
ln -fs "$ELMFIRE_INSTALL_DIR/elmfire_post_debug_$ELMFIRE_VER" "$ELMFIRE_INSTALL_DIR/elmfire_post_debug$ELMFIRE_BIN_SUFFIX"

echo "Cleaning up"
cd ..
rm -rf elmfire_build elmfire_post_build

echo "Done. Binaries installed to $ELMFIRE_INSTALL_DIR"
exit 0
