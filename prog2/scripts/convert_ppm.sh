#! /bin/bash

# check for arguments
if [ $# -eq 0 ]
then
	# no args passed
	echo "Usage: ./convert_ppm.sh <input file> <output file>"
	exit 1
fi

# check for imagemagick dependency
pacman -Qi imagemagick > /dev/null

if [ $? -ne 0 ]
then
	# missing dependency
	echo "Missing dependency 'imagemagick'. Installing..."
	sudo pacman -S imagemagick --noconfirm
fi
	
# converting
convert $1 -compress none -set comment \"\" $2
sed -i '/^#/d' $2
