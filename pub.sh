#!/usr/bin/env bash

rm -rf assets images
rm index.html
rm app.js
cp -R  ~/.maya-game/* .
echo "now do: git add . && git commit -m 'updates' && git push"
