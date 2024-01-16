# -*-Shell-Script-*-
alias cached='git diff --cached'
alias st='git status'
alias status='git status'
alias top='htop'

# ls in color
alias ls='ls -G'

# git diff ftw
# alias diff='diff -u'
alias diff='git diff --no-index'

# go
alias gco='go test -coverprofile=coverage.out && go tool cover -html=coverage.out'

#alias cl='rlwrap sbcl'

# k8s
alias k='kubectl'
alias kcc='kubectl config current-context'

kssh() {
    local pod=$1
    shift
    local cmd="${@:-/bin/bash}"
    kubectl exec $pod -it -c toolbox -- $cmd
}
