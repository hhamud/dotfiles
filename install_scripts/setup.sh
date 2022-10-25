#!/usr/bin/env sh

function install_emacs(){
    echo "======> Installing emacs <======";	
    sudo pacman -Syu emacs ripgrep;
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d;
    rm -rf /home/$USER/.emacs.d/;
    /home/$USER/.emacs.d/bin/doom install;
    export PATH=$PATH:$HOME/.emacs.d/doom/bin;
    doom doctor;
}

function check_init(){
    package=emacs
    if pacman -Qs $package > /dev/null;
    then
        echo "The package $package is installed";
    else
        install_emacs;
        install_dotfiles;

    fi

}

function install_dotfiles(){
    DIR="$HOME/.dotfiles"
    if [ -d  "$DIR" ]; then
        echo "dotfiles already exist"
    else
    	echo "======> installing dotfiles <======";
        cd ~/.dotfiles;
	# bash files
	rm -rf $HOME/.bashrc $HOME/.bash_profile
	ln -s $HOME/.dotfiles/.bashrc $HOME/.bashrc
	ln -s $HOME/.dotfiles/.bash_profile  $HOME/.bash_profile

	# Emacs config files
	rm -rf $HOME/.doom.d
	ln -s $HOME/.doom.d  $HOME/.doom.d

	#vim config files
	rm -rf $HOME/.config
	ln -s $HOME/.config  $HOME/.config
    fi
}


function run_script(){
    check_init;
}

run_script;
