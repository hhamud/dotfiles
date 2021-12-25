
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
export PATH=~/bin:$PATH

export PATH="$PATH:`yarn global bin`"
export PATH="$HOME/.gem/ruby/3.0.0/bin:$PATH"
export PATH="/home/user/.local/share/gem/ruby/3.0.0/bin:$PATH"
. "$HOME/.cargo/env"
if [ -e /home/user/.nix-profile/etc/profile.d/nix.sh ]; then . /home/user/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
