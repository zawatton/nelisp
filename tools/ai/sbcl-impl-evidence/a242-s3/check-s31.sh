#!/bin/bash
# Doc 242 S3.1: quiet paired runs, build31 (Doc 241 final) vs the Doc 242 build:
# bare start and the Doc 240 launcher (hash-get1024, N=16384) each faster by at
# least 5%, results equal.  Reuses the Doc 241 S4.1 check with other binaries.
set -u
B=${COLD_LOAD_BUILDS:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds}
HERE=$(cd "$(dirname "$0")" && pwd) || exit 1
IMAGE_SIZE_REF_BIN=${COLD_LOAD_REF:-$B/build-on31/nelisp-dynamic} \
IMAGE_SIZE_BIN=${COLD_LOAD_BIN:-$B/build-on33/nelisp-dynamic} \
IMAGE_SIZE_REF_DRIVERS=${COLD_LOAD_REF_DRIVERS:-$B/run/b31} \
IMAGE_SIZE_DRIVERS=${COLD_LOAD_DRIVERS:-$B/run/b33} \
IMAGE_SIZE_MARGIN=0.05 \
exec bash "$HERE/../a241-s4/check-s41.sh"
