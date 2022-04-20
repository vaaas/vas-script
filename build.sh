#!/bin/sh

# variables
name='vsc'
version='0.0.1'
arch='amd64'
dir="$name"_"$version"_"$arch"

# dirs
mkdir -p $dir/DEBIAN $dir/usr/share/guile/vas-script $dir/usr/bin
cp -pr --parents vsc.scm compiler.scm parser.scm util.scm lang $dir/usr/share/guile/vas-script
cp vsc $dir/usr/bin

# control file
cat <<EOF > $dir/DEBIAN/control
Package: $name
Version: $version
Architecture: $arch
Maintainer: Vasileios Pasialiokis <pasivasi@outlook.com>
Description: Vas Script - Scheme to anything compiler
Depends: guile-2.0 | guile-3.0
EOF

chmod 755 $dir/DEBIAN

# build
dpkg-deb --build --root-owner-group $dir

# cleanup
rm -rf $dir
