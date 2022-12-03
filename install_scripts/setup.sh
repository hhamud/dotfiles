#!/usr/bin/env bash



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
	# bash files
	echo "======> symlinking bash files"
	rm -rf $HOME/.bashrc $HOME/.bash_profile
	ln -s $HOME/.dotfiles/.bashrc $HOME/.bashrc
	ln -s $HOME/.dotfiles/.bash_profile  $HOME/.bash_profile

	# Emacs config files
	echo "======> symlinking emacs files"
	rm -rf $HOME/.doom.d
	ln -s $HOME/.dotfiles/.doom.d  $HOME/.doom.d

	#vim config files
	echo "======> symlinking vim files"
	rm -rf $HOME/.config
	ln -s $HOME/.dotfiles/.config  $HOME/.config
	
	echo "======> setup complete <======"
}


function run_script(){
    check_init;
}

run_script;
