#!/bin/bash

set -ex

cpp -P  time.F90 >time.0.f90
cpp -P  plib.F90 >plib.0.f90

cat \
    global.f90 \
    sn.f90 \
    data.f90 \
    geom.f90 \
    control.f90 \
    time.0.f90 \
    plib.0.f90 \
    mms.f90 \
    solvar.f90 \
    dealloc.f90 \
    utils.f90 \
    version.f90 \
    input.f90 \
    setup.f90 \
    output.f90 \
    expxs.f90 \
    thrd_comm.f90 \
    dim1_sweep.f90 \
    dim3_sweep.f90 \
    mkba_sweep.f90 \
    octsweep.f90 \
    sweep.f90 \
    inner.f90 \
    outer.f90 \
    analyze.f90 \
    translv.f90 \
    snap_main.f90 \
    > main.f90
