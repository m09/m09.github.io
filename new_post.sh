#!/bin/bash

read -r -p "Post title > "
title=${REPLY}
title_clean="$(<<< "$title" \
    iconv -f utf8 -t ascii//translit \
    | tr ' [:upper:]' '-[:lower:]' \
    | tr -dc 'a-z0-9._-')"

filename="posts/$(date "+%Y-%m-%d")-$title_clean.md"
echo "---
title: $title
author: m09
---" > "$filename"

if [[ $VISUAL ]]; then
    $VISUAL "$filename" &
    disown
elif [[ $EDITOR ]]; then
    $EDITOR "$filename"
else
    {
        echo -n 'Please set your VISUAL and EDITOR '
        echo 'variables. Defaulting to nano.'
    } >&2
    nano "$filename"
fi
