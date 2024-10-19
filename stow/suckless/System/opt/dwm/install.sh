#!/bin/bash

set -e

if [ -d ./dwm/ ]; then
	read -p "delete the folder first? (y/n): " option
	[[ "$option" != "y" ]] && exit 1
	rm -rf ./dwm/
fi

git clone --depth 1 https://git.suckless.org/dwm

cp *diff dwm/
cp config.h dwm/
cd dwm/

for patchfile in $(ls *diff); do
	patch -p1 < $patchfile
done

make
# you can `make install` now or `ln -s` the binary, or add this path to $PATH
