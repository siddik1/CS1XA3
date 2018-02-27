#!/bin/bash

function repo_check() {
local=$(git rev-parse master)
remote=$(git rev-parse origin/master)
if [ $local == $remote ]
then
echo "Local Repo is up to date with remote repo"
else
echo "Local Repo is not up to date with remote repo"
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

function jumpup_directories() {
LIMIT=$1
P=$PWD
for ((i=1; i <= $ans; i++))
do
    P=$P/..
done
cd $P
export MPWD=$P
}

function jumpdown_directories() {
LIMIT=$1
P=$MPWD
for ((i=1; i <= LIMIT; i++))
do
    P=${P%/..}
done
cd $P
export MPWD=$P
}

repo_check
check_changes
check_TODO
check_haskellerrors
jumpup_directories
jumpdown_directories
