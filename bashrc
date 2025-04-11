#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='lsd'
alias grep='grep --color=auto'
alias umbrel='ssh umbrel@umbrel'
export PATH="/home/kjani/.local/bin:$PATH"
alias d="cd ~/docs/ && lf"
alias c="cd ~/code/ && lf"
alias p="cd ~/docs/projects && lf"
alias r="cd ~/Seafile/Research\ Paper\ Library/ && lf"
alias bin="cd ~/.local/bin"
alias sc="helix ~/sc.md"
#alias vim="helix"
alias wc="wc -w"
alias wg="wordgrinder"
export EDITOR="helix"
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

# Added by LM Studio CLI (lms)
export PATH="$PATH:/home/kjani/.lmstudio/bin"
