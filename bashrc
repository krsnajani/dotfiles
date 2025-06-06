#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='lsd'
alias grep='grep --color=auto'
alias umbrel='ssh umbrel@umbrel'
export PATH="/home/kjani/.local/bin:$PATH"
alias d="cd ~/Documents/ && lf"
alias c="cd ~/Code/ && lf"
alias p="cd ~/Docs/Projects && lf"
alias bin="cd ~/.local/bin"
#alias vim="helix"
alias wc="wc -w"
alias wg="wordgrinder"
export EDITOR="vim"
alias e="$EDITOR"
PS1='[\u@\h \W]\$ '
#cat ~/.cache/wal/sequences
pfetch


function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}

alias lf="y"
alias yazi="y"

