#!/bin/sh
# Run bolotest twice and require byte-identical frames.
#
# usage: compare_runs.sh <bolotest> <workdir> <pumpA> <pumpB> <frames>
set -e

if [ $# -ne 5 ]; then
  echo "usage: $0 <bolotest> <workdir> <pumpA> <pumpB> <frames>" >&2
  exit 2
fi

BOLOTEST=$1
WORKDIR=$2
PUMP_A=$3
PUMP_B=$4
FRAMES=$5

rm -rf "$WORKDIR/a" "$WORKDIR/b"
mkdir -p "$WORKDIR/a" "$WORKDIR/b"

"$BOLOTEST" --frames "$FRAMES" --pump "$PUMP_A" --out "$WORKDIR/a"
"$BOLOTEST" --frames "$FRAMES" --pump "$PUMP_B" --out "$WORKDIR/b"

for a in "$WORKDIR"/a/*.ppm; do
  b="$WORKDIR/b/$(basename "$a")"
  if ! cmp -s "$a" "$b"; then
    echo "FAIL: $a and $b differ" >&2
    exit 1
  fi
done

echo "OK: $FRAMES frames identical at pump $PUMP_A vs $PUMP_B"
