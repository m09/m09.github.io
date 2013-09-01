#!/bin/bash

read -r -p "Post name > "
title=${REPLY}
title_clean="$(<<< "$title" \
    iconv -f utf8 -t ascii//translit \
    | tr '[:upper:]' '[:lower:]' \
    | tr -dc 'a-z0-9. _-' \
    | tr ' ' '-')"

filename="$(date "+%Y-%m-%d")-$title_clean.md"
echo "---
title: $title
author: Mog
---" > "posts/$filename"

emacs "posts/$filename"
