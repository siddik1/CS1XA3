#!/bin/bash


val= (git status | grep "Your branch is up-to-date")

if [$val == "Your branch is up-to-date"]
then
echo "Everything's up to date, good job!"
else
echo "Uh-oh! You might want to pull/push"


git diff > changes.log


tag=(grep -r "#TODO" --exclude="todo.log" --exclude="ProjectAnalyze.sh" --exclude="changes.log")
$tag > todo.log


find -name "*.hs" | xargs -I {} ghc -fno-code {} &> error.log
