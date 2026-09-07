#!/usr/bin/env bash

APP=$(ls /Applications/ /Applications/Utilities/ /System/Applications/ /System/Applications/Utilities/ 2>/dev/null | \
    grep '\.app$' | \
    sed 's/\.app$//g' | \
    sort -u | \
    choose -n 15 -p "Launch: ")

[ -n "$APP" ] && open -a "$APP.app"
