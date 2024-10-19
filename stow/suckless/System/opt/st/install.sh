#!/bin/bash

set -e

suckless_program="st"
theme="dark"

function read_yn {
  msg="$1"
  read -p "${msg} (y/n): " option
  if [ "$option" != "y" ]; then
    return 1
  fi
  return 0
}

function unedit_program {
  [[ ! -d ./${suckless_program}/ ]] && return
  cd ./${suckless_program}/
  git reset --hard
  touch sample.diff
  rm *diff
}

function check_output_exists {
  if [ -d ./${suckless_program}/ ]; then
    # TODO avoid deleting anything else?
    read_yn "delete folder first?" && \
      rm -rf ./${suckless_program}/ && \
      return 0
          read_yn "git restart --hard and delete project's diff?" && \
            unedit_program && \
            return 1
                      exit 1
  fi
}

function download_program {
  git clone --depth 1 https://git.suckless.org/${suckless_program}
}

function apply_patches {
  cp ../*diff .

  for patchfile in $(ls *diff); do
    patch -p1 < $patchfile
  done
}

# you can `make install` now or `ln -s` the binary, or add this path to $PATH

check_output_exists
download_program

# make sure to cd into program
cd ${suckless_program}

apply_patches

cp ../config.${theme}.h config.h
make
