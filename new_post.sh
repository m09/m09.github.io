#!/bin/bash

read -r -p "Post name > "
title="$(<<< "${reply}" \
    iconv -f utf8 -t ascii//translit \
    | tr '[:upper:]' '[:lower:]' \
    | tr -dc 'a-z0-9. _-' \
    | tr ' ' '-')"

filename="$(date "+%Y-%m-%d")-$title.md"
echo "---
title: $title
author: Mog
---" > "posts/$filename"

emacs "posts/$filename"
