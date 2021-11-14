#!/bin/bash
# for branch in $(git branch --all | grep '^\s*remotes' | egrep --invert-match '(:?HEAD|master)$'); do
#     git branch --track "${branch##*/}" "$branch"
# done

for branch in $(git branch -a); do if echo $branch | grep -q remotes && ! echo $branch | grep -q HEAD; then git checkout "${branch#remotes/origin/}"; git pull; fi; done
