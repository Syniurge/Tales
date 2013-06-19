#!/bin/bash

mkdir kompare
cd kompare

mkdir orig mod
cd mod

for f in ../../*.{h,cpp,td}; do
	ln -s "$f" ./
	ln -s "$f.orig" "../orig/$(basename "$f")"
done

cd ..
kompare orig mod
cd ..

rm -rf kompare