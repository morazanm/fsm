#!/bin/bash 

set -e
set -o pipefail
set -u

OS=$(uname)
DOWNLOAD_SITE=https://graphviz.org/download/
BLUE='\033[1;34m'
NC='\033[0m' # No Color


function testcmd() {
  command -v "$1" >/dev/null
}

function installMacOS() {
  if testcmd brew; then
    brew install graphviz
    echo -e "n\Graphviz is now installed"
  elif testcmd port; then
    sudo port install graphviz
    echo -e "\nGraphviz is now installed"
  else
    echo "Please install homebrew or macPorts package manager and try again"
    exit 1
  fi
}

function installLinux() {
  PS3="Enter the number retailed to your linux distrbution: "
  select distro in "Debian/Ubuntu" "Fedora/CentOS" "Arch" "Other"
  do
    case $distro in
      "Debian/Ubuntu")
        echo -e "\nPlease run the below command in your terminal:"
        printf "\t${BLUE}sudo apt install graphviz${NC}\n"
        break;;
      "Fedora/CentOS")
        echo -e "\nPlease run the below command in your terminal:"
        printf "\t{$BLUE}sudo yum install graphviz${NC}\n"
        break;;
      "Arch")
        echo -e "\nPlease run the below command in your terminal:"
        printf "\t${BLUE}sudo pacman -Sy graphviz${NC}\n"
        break;;
      "Other")
        echo -e "\nPlease visit ${BLUE}${DOWNLOAD_SITE}${NC} to install graphviz on your machine"
        break;;
      *)
        echo "Invalid option $REPLY"
        ;;
    esac
  done
}

if testcmd dot; then
  echo "Dot executable is already installed at: $(command -v dot)"
elif [ "$OS" == "Darwin" ]; then
  installMacOS
elif [ "$OS" == "Linux" ]; then
  installLinux
else
  echo "OS: $OS is not supported for install script. Please vist ${BLUE}${DOWNLOAD_SITE}${NC} to install graphviz on your machine"
fi

exit 0
