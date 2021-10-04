#!/usr/bin/env bash

set -ex

# Target directory to deploy stdlib to
destdir="${DESTDIR:-forlab-fpm}"

# Additional files to include
include=(
  "fpm.toml"
  "LICENSE"
  "README.md"
)

mkdir -p "$destdir/src" "$destdir/test"

# Collect stdlib source files
cp src/*  "$destdir/src/"
cp -r test/* "$destdir/test/"

# Include additional files
cp "${include[@]}" "$destdir/"

# List stdlib-fpm package contents
ls -R "$destdir"