#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# https://wiki.archlinux.org/index.php/Environment_variables#Per_user
export PATH="${PATH}:${HOME}/.local/bin"

# https://wiki.archlinux.org/index.php/XDG_Base_Directory
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"

# https://github.com/rstudio/rstudio/issues/9854#issuecomment-972033704
export RSTUDIO_CHROMIUM_ARGUMENTS="--no-sandbox"
