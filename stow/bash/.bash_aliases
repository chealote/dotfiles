alias ls='ls -p -F --color=always'
alias ll='ls -l'

# quicly record with ffmpeg, but lately I've been using obs
alias rec='ffmpeg -f x11grab -y -framerate 30 -s 1920x1080 -i :0.0 -c:v libx264 -preset superfast -crf 18 out.mp4'

# create or attach a tmux session
alias t='tmux -u new-session -s al -A'

# echo to system's clipboard
alias xc='xclip -selection clipboard'
# save copied image to system: xi image.png
alias xi='xclip -selection clipboard -t image/png -o > '
# copy image to clipboard: ix image.png and paste somewhere else
alias ix='xclip -selection clipboard -t image/png -i < '

# check my open.c program
alias o='open'

# simple grep and recursive grep avoiding some folders
alias grep='grep --color=yes -iIs'
alias gr='grep --color=yes -niIr --exclude-dir={.git,node_modules,venv,.next}'

alias f="feh --scale-down -B \"#000000\" -d --edit"

# python!
alias python="python3"
alias p="python"

source ~/.bash_aliases.priv
