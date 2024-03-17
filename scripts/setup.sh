#!/usr/bin/env bash

# Function to detect the operating system and version
detect_os() {
    OS=""
    VER=""

    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        # Linux
        if [ -f /etc/os-release ]; then
            # freedesktop.org and systemd
            OS=$NAME
            VER=$VERSION_ID
        elif type lsb_release >/dev/null 2>&1; then
            # linuxbase.org
            OS=$(lsb_release -si)
            VER=$(lsb_release -sr)
        elif [ -f /etc/lsb-release ]; then
            # For some versions of Debian/Ubuntu without lsb_release command
            OS=$DISTRIB_ID
            VER=$DISTRIB_RELEASE
        elif [ -f /etc/debian_version ]; then
            # Older Debian/Ubuntu/etc.
            OS=Debian
            VER=$(cat /etc/debian_version)
        else
            # Fall back to uname
            OS=$(uname -s)
            VER=$(uname -r)
        fi
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS
        OS="macOS"
        VER=$(sw_vers -productVersion)
    elif [[ "$OSTYPE" == "msys" ]]; then
        # Windows 10 (MSYS environment)
        OS="Windows 10"
        VER=$(wmic os get Caption | grep -v Caption | tr -d '\n\r')
    elif [[ "$OSTYPE" == "freebsd"* ]]; then
        # FreeBSD
        OS=$(uname -s)
        VER=$(uname -r)
    else
        # Other Unix-based systems
        OS=$(uname -s)
        VER=$(uname -r)
    fi

    echo "Operating system: $OS"
    echo "Version: $VER"
}

# Function to install Emacs ripgrep and git
install_emacs() {
    echo "======> Installing Emacs and ripgrep <======"

    if [[ "$OS" == "macOS" ]]; then
        brew install emacs ripgrep git
    elif [[ "$OS" == "Debian" || "$OS" == "Ubuntu" ]]; then
        sudo apt-get update
        sudo apt-get install -y emacs ripgrep git
    elif [[ "$OS" == "Fedora" || "$OS" == "CentOS" || "$OS" == "RHEL" ]]; then
        sudo dnf install -y emacs ripgrep git
    elif [[ "$OS" == "Arch Linux" ]]; then
        sudo pacman -Syu emacs ripgrep git
    elif [[ "$OS" == "Windows 10" ]]; then
        echo "Installing Emacs and ripgrep on Windows 10..."

        # Install Chocolatey package manager
        if ! command -v choco >/dev/null 2>&1; then
            echo "Installing Chocolatey..."
            powershell.exe -NoProfile -InputFormat None -ExecutionPolicy Bypass -Command "[System.Net.ServicePointManager]::SecurityProtocol = 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))" && SET "PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin"
        fi

        # Install Emacs and ripgrep using Chocolatey
        choco install emacs ripgrep git -y
    else
        echo "Unsupported operating system: $OS"
        return
    fi

}

install_doom(){
    rm -rf "$HOME/.emacs.d/"
    git clone --depth 1 https://github.com/hlissner/doom-emacs "$HOME/.emacs.d"
    "$HOME/.emacs.d/bin/doom" install
    export PATH="$PATH:$HOME/.emacs.d/doom/bin"
    doom doctor
}

# Function to check if Emacs is installed and install if necessary
check_init() {
    if command -v emacs >/dev/null 2>&1; then
        echo "Emacs is installed."
        install_doom
        install_dotfiles
    else
        install_emacs
        install_doom
        install_dotfiles
    fi
}

# Function to install dotfiles
install_dotfiles() {
    echo "======> Installing dotfiles <======"

    if [ -d "$HOME/.dotfiles" ]; then
        cd "$HOME/.dotfiles" || return

    else
        echo "Dotfiles directory not found: $HOME/.dotfiles"
        echo "cloning dotfiles"
        git clone https://github.com/hhamud/dotfiles.git "$HOME"
    fi

    # Emacs config files
    echo "======> Symlinking Emacs files"
    rm -rf "$HOME/.doom.d"
    ln -s "$HOME/.dotfiles/.doom.d" "$HOME/.doom.d"
    echo "======> Setup complete <======"
}

# Function to test Emacs and ripgrep installations
test_installations() {
    if command -v emacs >/dev/null 2>&1; then
        echo "Emacs is installed."
        emacs --version
    else
        echo "Emacs is not installed."
    fi

    if command -v rg >/dev/null 2>&1; then
        echo "ripgrep is installed."
        rg --version
    else
        echo "ripgrep is not installed."
    fi

    if [ -d "$HOME/.emacs.d" ] && [ -x "$HOME/.emacs.d/bin/doom" ]; then
        echo "Doom Emacs is installed."
    else
        echo "Doom Emacs is not installed."
    fi
}

# Main function to run the script
run_script() {
    detect_os
    check_init
    test_installations
}

run_script
