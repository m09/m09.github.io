#!/bin/bash

read -r -p "Event name > "
title=${REPLY}
title_clean="$(<<< "$title" \
    iconv -f utf8 -t ascii//translit \
    | tr ' [:upper:]' '-[:lower:]' \
    | tr -dc 'a-z0-9._-')"

filename="posts/$(date "+%Y-%m-%d")-$title_clean.md"
echo "---
title: $title
author: Mog
---" > "$filename"

emacs "$filename" &
