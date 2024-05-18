# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
#case $- in *i*) ;;
#      *) return;;
#esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
#HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
#shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
#HISTSIZE=1000
#HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
#shopt -s checkwinsize

# enable color support of ls and also add handy aliases
#if [ -x /usr/bin/dircolors ]; then
#    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
#    alias ls='ls --color=auto'
#    #alias dir='dir --color=auto'
#    #alias vdir='vdir --color=auto'
#
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
#    alias egrep='egrep --color=auto'
#fi
#
# Text utils
ESC_RESET="\[$(tput sgr0)\]"
ESC_BLUE_FG="\[$(tput setaf 4)\]"

# Prompt
PS_DIR="${ESC_BLUE_FG}\W${ESC_RESET}"
PS_INDICATOR="\$"

export PS1="${PS_DIR} ${PS_INDICATOR} "

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
#alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
#if ! shopt -oq posix; then
#  if [ -f /usr/share/bash-completion/bash_completion ]; then
#    . /usr/share/bash-completion/bash_completion
#  elif [ -f /etc/bash_completion ]; then
#    . /etc/bash_completion
#  fi
#fi

# Custom envvars
#export PATH="$PATH:/home/seryiza/.local/bin:/home/seryiza/.nix-profile/bin"
export EDITOR="nvim"
export LEDGER_FILE="/home/seryiza/code/sfinances/main.journal"
#export NIX_PATH=${NIX_PATH:+$NIX_PATH:}$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels

# Custom aliases
alias gst="git status"
alias gs="git status"
alias ga="git add"
alias gd="git diff"
alias gdca="git diff --staged"
alias gup="git pull --rebase"
alias gp="git push"
alias gr="git restore"
alias grs="git restore --staged"
alias gco="git checkout"
alias gcmsg="git commit -m"
alias gcl="git clone"
alias glo="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias gsu="git submodule update"
alias dc="docker-compose"
alias docker-compose="docker compose"
alias ubup="sudo apt-get update && sudo apt-get upgrade"
alias tt="tmuxinator"
alias nvi="neovide --frameless"
alias e="emacs -nw"
alias n="nvim"
alias httphere="http-server -c5"
alias m="make"
alias vinix="sudo $EDITOR /etc/nixos/configuration.nix"
alias nr="sudo nixos-rebuild"
alias nrs="sudo nixos-rebuild switch"
alias nc="sudo nix-channel --update"
alias ncu="sudo nix-channel --update"
alias ghpr="gh pr create -a @me && gh pr merge --auto --delete-branch --rebase"
alias xobsidian="WAYLAND_DISPLAY= obsidian"

# Tmux here
function th() {
    currentdir=$(pwd)
    currentdirname=$(basename $currentdir)
    tmux new -As $currentdirname -c $currentdir
}

#function asdfig() {
#    asdf install $1 $2 && asdf global $1 $2
#}

# Autojump
#. /usr/share/autojump/autojump.bash

# Asdf
# . /opt/asdf-vm/asdf.sh

# . /etc/profile.d/google-cloud-sdk.sh
