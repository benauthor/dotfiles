# -*-Shell-Script-*-
alias ll='ls -lh'
alias la='ls -lah'
alias l='ls -CF'

alias chrome='open -a Google\ Chrome'
alias ff='open -a Firefox'

alias py='ipython'
alias pvm="pythonbrew"

alias gitk='gitx'
alias gitg='git log --pretty=format:"%h|%an|%s" --graph'
alias cached='git diff --cached'
alias st='git status'
alias status='git status'


alias irc="irssi -c irc.usnews.com -n evan"
alias tmux="TERM=screen-256color-bce tmux"
alias bitlbee="~/local/sbin/bitlbee -D"
alias screen="/usr/local/Cellar/screen/4.0.3/bin/screen"

alias startmysql="sudo mysqld_safe &"
alias starttomcat="~/local/apache-tomcat-7.0.39/bin/startup.sh"
alias stoptomcat="~/local/apache-tomcat-7.0.39/bin/shutdown.sh"

alias servehere="python -m SimpleHTTPServer 8181"

alias redis-redeye="redis-cli -h 10.1.1.11 -p 6379"
alias redis-melrose="redis-cli -h 10.1.1.12 -p 6379"

alias ppjson="python -mjson.tool"
alias sbcl="rlwrap sbcl"
alias ccl="rlwrap ccl64"
