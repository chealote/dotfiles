packages=bash emacs git gnupg suckless tmux vim x

all:
	stow -d stow -t $(HOME) -v $(packages)
