#!/bin/bash

# This is a script to help switch the fsm install path between local and prod 

set -e # exit if there is an error
set -o pipefail
set -u # exit if unset var is called 


repo="https://github.com/morazanm/fsm.git"

function installFSM() {
  if [ $1 ]; then
    cd ..
    echo "\n\n***Install Local Dev FSM at $(pwd)***\n\n"
    raco pkg install 
  else 
    echo "\n\n***Installing Production FSM from $repo***\n\n"
    raco pkg install $repo
  fi
}

function removeFSM() {
  if [ $(raco pkg show | grep -c fsm) -ge 1 ]; then 
    echo "\n\n***Found FSM Install.. Removing...***\n\n"
    raco pkg remove fsm
  fi
}




echo "Would you like to install Local or Prod FSM?"
echo "[1] - Local"
echo "[2] - Global"
read option

if [ $option -eq 1 ]; then 
  removeFSM
  installFSM true
elif [ $option -eq  2 ]; then
  removeFSM
  installFSM
else 
 echo "Invlaid option supplied. Exiting" 
 exit 1
fi

