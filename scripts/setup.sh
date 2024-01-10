#!/usr/bin/env bash

OS=""
VER=""

function check_os() {
  if [ "$(uname)" == "Darwin" ]; then
    # macOS
    OS="macOS"
    VER=$(sw_vers -productVersion)
  elif [ "$(uname)" == "Windows" ]; then
    # Windows
    OS="Windows"
    # Get the version number using the wmic command
    VER=$(wmic os get Caption | grep -v Caption | tr -d '[\r\n]')
  elif [ -f /etc/os-release ]; then
    # Linux: freedesktop.org and systemd
    . /etc/os-release
    OS=$NAME
    VER=$VERSION_ID
  elif type lsb_release >/dev/null 2>&1; then
    # Linux: linuxbase.org
    OS=$(lsb_release -si)
    VER=$(lsb_release -sr)
  elif [ -f /etc/lsb-release ]; then
    # Linux: For some versions of Debian/Ubuntu without lsb_release command
    . /etc/lsb-release
    OS=$DISTRIB_ID
    VER=$DISTRIB_RELEASE
  elif [ -f /etc/debian_version ]; then
    # Linux: Older Debian/Ubuntu/etc.
    OS=Debian
    VER=$(cat /etc/debian_version)
  elif [ -f /etc/SuSe-release ]; then
    # Linux: Older SuSE/etc.
    ...
  else
    # Fall back to uname, e.g. "Linux <version>", also works for BSD, etc.
    OS=$(uname -s)
    VER=$(uname -r)
  fi

    echo "Operating system: $OS"
    echo "Version: $VER"

}



function install_emacs(){
    echo "======> Installing emacs <======"	
    sudo pacman -Syu emacs ripgrep
    rm -rf $HOME/.emacs.d/
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
    $HOME/.emacs.d/bin/doom install
    export PATH=$PATH:$HOME/.emacs.d/doom/bin
    doom doctor
}


function check_init(){
    package=emacs
    if pacman -Qs $package > /dev/null
    then
        echo "The package $package is installed"
        install_dotfiles
    else
        install_emacs
        install_dotfiles

    fi
}

function install_dotfiles(){
	echo "======> installing dotfiles <======"
	cd ~/.dotfiles

	# Emacs config files
	echo "======> symlinking emacs files"
	rm -rf $HOME/.doom.d
	ln -s $HOME/.dotfiles/.doom.d  $HOME/.doom.d

	echo "======> setup complete <======"
}


function run_script(){
    check_init;
}

