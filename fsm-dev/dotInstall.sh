#!/bin/bash 
OS=$(uname)
function testcmd() {
command -v "$1" >/dev/null
}

function installMacOS() {
  if testcmd brew; then
    brew install graphviz
  elif testcmd port; then
    sudo port install graphviz
  else
    echo "Please install homebrew or macPorts and try again"
    exit 1
  fi
}

function installLinux() {
  echo "TODO LINUX"
}

if testcmd dot; then
  echo "Dot executable is already installed at: $(command -v dot)"
elif [ "$OS" == "Darwin" ]; then
  installMacOS
elif [ "$OS" == "Linux" ]; then
  installLinux
else
  echo "OS: $OS is not supported for install script"
fi
