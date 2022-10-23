#!/usr/bin/env sh

install_emacs(){
    sudo pacman -Syu emacs ripgrep;
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d;
    rm -rf /home/$USER/.emacs.d/;
    /home/$USER/.emacs.d/bin/doom install;
    export PATH=$PATH:$HOME/.emacs.d/doom/bin;
    doom doctor;
}

install_dotfiles(){
    cd ~/.dotfiles;
    for file in *; do
        ln -sf ~/.dotfiles/"$file" ~/."$file"
    done
}



run_script(){
    install_emacs;
    install_dotfiles;
}
