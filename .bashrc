#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Formatting -------------------------------------------------------------------
PS1='[\u@\h \W]\$ '
export TERMINAL='/usr/bin/termite'
export VISUAL='emacs'
export EDITOR='emacs'

# Aliases ----------------------------------------------------------------------
## Cli defaults
alias ls='ls -hFo --color=auto'
alias lsa='ls -a'
alias cp="rsync -ah --inplace --no-whole-file --info=progress2"
alias wdmesg="dmesg -wH"

## Copy and paste
# # https://stackoverflow.com/a/4208191/2860744
alias xcopy="xclip -selection c"
alias xpaste="xclip -selection clipboard -o"

## Applications
alias R="R --quiet --no-restore --no-save"
alias n2="dvtm nnn nnn"

magit() { emacsclient -nw -e '(progn (magit-status)(delete-other-windows))'; }
madit() { emacs -nw -f madit-status -f fit-window-to-buffer $1; }
eout() { emacs $1 & disown; }
tout() { termite -d $(pwd) & disown; }

## Pacman
alias pacmans='sudo pacman -Syu'
## https://wiki.archlinux.org/index.php/Pacman#Removing_packages
alias pacmanr='sudo pacman -Rscn'
## https://wiki.archlinux.org/index.php/Pacman/Tips_and_tricks#List_of_installed_packages
alias pacmanq='pacman -Qte'

## AUR
alias makepkgsi='makepkg --clean --cleanbuild --install --log --syncdeps --rmdeps'

# Dot file tracking
## https://wiki.archlinux.org/title/Dotfiles#Tracking_dotfiles_directly_with_Git
## https://news.ycombinator.com/item?id=11071754
alias dit='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# Autocomplete
complete -cf sudo # https://bbs.archlinux.org/viewtopic.php?id=45613

# Settings ---------------------------------------------------------------------
## SSH
## https://wiki.archlinux.org/index.php/SSH_keys#SSH_agents
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi

if [[ ! "$SSH_AUTH_SOCK" ]]; then
  eval "$(<"$XDG_RUNTIME_DIR/ssh-agent.env")" &>/dev/null
fi

## Autojump
source /usr/share/autojump/autojump.bash
