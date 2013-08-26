#!/bin/bash

username=mog
server=88.191.117.112
blog_folder=/home/mog/blog/_site
web_folder=www

rsync --archive --compress --delete \
    "${blog_folder%/}/" \
    "$username@$server:$web_folder"
