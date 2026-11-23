#!/usr/bin/env bash
# nl-cache-gc.sh --- collect a shared unit cache.  Usage: nl-cache-gc.sh DIR [DAYS]
# Removes *.unit entries not read for DAYS days (default 14; atime is not
# trusted on noatime mounts, so mtime is used) and orphaned *.tmp files older
# than one day.  Run it while no build uses DIR as a write target; a reader
# that loses a file treats it as a miss, so a concurrent read is safe.
set -u
dir=${1:?usage: nl-cache-gc.sh DIR [DAYS]}; days=${2:-14}
[ -d "$dir" ] || { echo "no such dir: $dir" >&2; exit 2; }
find "$dir" -type f -name '*.unit' -mtime +"$days" -print -delete
find "$dir" -type f -name '*.tmp' -mtime +1 -print -delete
