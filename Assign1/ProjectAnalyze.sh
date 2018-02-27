#!/bin/bash


function check_status() {
val= (git status | grep "Your branch is up-to-date")

if [$val == "Your branch is up-to-date"]
then
echo "Everything's up to date, good job!"
else
echo "Uh-oh! You might want to pull/push"
fi
}

function check_changes() {
git diff > changes.log
}

function check_TODO() { 
tag=(grep -r "#TODO" --exclude="todo.log" --exclude="ProjectAnalyze.sh" --exclude="changes.log")
$tag > todo.log
}

function check_haskellerrors () {
find -name "*.hs" | xargs -I {} ghc -fno-code {} &> error.log
}
