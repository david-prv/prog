#! /bin/bash

# Gist: https://gist.github.com/david-prv/236c7e44b23f5fa71e4c2819276c9b0e

# check for arguments
if [ $# -eq 0 ]
then
	# no args passed
	echo "Usage: ./convert_ppm.sh <input file> <output file>"
	exit 1
fi

# check for imagemagick dependency
pacman -Qi imagemagick > /dev/null

if [ $? -eq 0 ]
then
	# imagemagick exists
	convert $1 -compress none -set comment \"\" $2
	sed -i '/^#/d' $2
	exit 0
else
	# missing dependency
	echo "Missing dependency 'imagemagick'. Installing..."
	sudo pacman -S imagemagick --noconfirm
	
	# continue converting
	convert $1 -compress none -set comment \"\" $2
	sed -i '/^#/d' $2
	exit 0
fi
