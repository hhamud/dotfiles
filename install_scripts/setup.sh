#!/usr/bin/env sh

function install_emacs(){
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
    DIR="/home/$USER/.dotfiles"
    if [ -d  "$DIR" ]; then
        echo "dotfiles already exist"
    else
        echo "syncing dotfiles"
        cd ~/.dotfiles;
        for file in *; do
            ln -sf ~/.dotfiles/"$file" ~/."$file"
        done
    fi
}


function run_script(){
    check_init;
}

run_script;
