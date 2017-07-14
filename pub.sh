#!/usr/bin/env bash

rm -rf ~/.maya-game
mkdir ~/.maya-game
cp -R out/* ~/.maya-game
echo "now do: git checkout gh-pages"
echo "then pub.sh"
