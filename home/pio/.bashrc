# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
alias py='source ~/pyenv/bin/activate'
alias emacs-py='source ~/pyenv/bin/activate;emacs -q -l ~/.emacs.d-py/init.el'
alias emacs-org='emacs -q -l ~/.emacs.d-org/init.el'
alias emacs-elm='emacs -q -l ~/.emacs.d-elm/init.el'
alias emacs-lisp-clj='emacs -q -l ~/.emacs.d-lisp-clj/init.el'


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
