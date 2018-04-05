# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
alias py='source ~/pyenv/bin/activate'
alias emacs-py='(nohup $(source ~/pyenv/bin/activate;emacs -q -l ~/.emacs.d-py/init.el &)) & exit'
alias emacs-org='(nohup $(emacs -q -l ~/.emacs.d-org/init.el &)) & exit'
alias emacs-lisp-clj='(nohup $(emacs -q -l ~/.emacs.d-lisp-clj/init.el &)) & exit'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
