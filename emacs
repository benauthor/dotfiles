;; -*-Emacs-Lisp-*-

;;; Commentary:

;;; This is my .emacs file.  There are many like it, but this one is mine.

;;; Code:

(defun ensure-packages-installed (packages)
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
           (package-install package)
         package)))
   packages))

(setq packages-i-use `(async
                       auto-complete
                       cider
                       clojure-mode
                       company
                       company-go
                       concurrent
                       ctable
                       dash
                       deferred
                       epc
                       epl
                       erlang
                       exec-path-from-shell
                       evil
                       evil-nerd-commenter
                       evil-surround
                       flx
                       flx-ido
                       flycheck
                       flycheck-clojure
                       git-commit
                       goto-chg
                       ido-vertical-mode
                       jedi
                       jedi-core
                       json-mode
                       json-reformat
                       json-snatcher
                       lusty-explorer
                       macrostep
                       magit
                       magit-popup
                       markdown-mode
                       markdown-preview-mode
                       meghanada
                       pkg-info
                       popup
                       powerline
                       py-isort
                       python-environment
                       queue
                       rainbow-delimiters
                       slime
                       smartparens
                       smex
                       solarized-theme
                       spinner
                       undo-tree
                       uuidgen
                       web-server
                       websocket
                       with-editor
                       yaml-mode))

;; where we get our packages from
(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ))

(package-initialize)

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-packages-installed packages-i-use)

;; make path sane
(exec-path-from-shell-initialize)

;; recompile .emacs.d on open
;; sometimes this is useful. it just takes a while -- usually keep it commented
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; (load-theme 'solarized-light t)
(load-theme 'solarized-dark t)

;; don't warn when following symbolic link to version controlled file
(setq vc-follow-symlinks nil)

;; turn off goddam bell!
;; (setq visible-bell 1)
(setq ring-bell-function 'ignore)

;; evil
(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)

;; colooors
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; typeface
(set-frame-font "Menlo 13" nil t)

;; do we want parens to match? depends on how I feel today
;; (smartparens-global-mode 1)

;; ido mode
(require 'ido)
(require 'flx-ido)  ; better fuzzy matching
(require 'ido-vertical-mode)
(ido-mode 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background "#073642"
                    :foreground "#b58900")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background "#dc322f"
                    :foreground "#b58900")
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground "#b58900")
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

;; magic meta-x that works with ido
(require 'smex)
(global-set-key [(meta x)] 'smex)

;; on the fly linting
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;(require 'midnight)
;;(setq clean-buffer-list-delay-general 1)

;; visual cue when I go over 80 columns
;;(require 'column-marker)
;;(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; sort of mimic vim's powerline
(require 'powerline)
(powerline-default-theme)

;; window navigation
(winner-mode 1)

;; where am I?
(which-function-mode)

;; toolbars
(tool-bar-mode -1)
(menu-bar-mode -1)

;; cursors
(setq evil-default-cursor t)
(set-cursor-color "#fb0")
(blink-cursor-mode 0)

;; whitespace
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline `visit-save)
(setq mode-require-final-newline `visit-save)

;; enable disabled-by-default
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; default directory
(setq default-directory (concat (getenv "HOME") "/"))

;; backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      kept-old-versions 2               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 5               ; newest versions to keep when a new numbered backup is made (default: 2)
      )
(setq vc-make-backup-files t)
(setq create-lockfiles nil)

;; put stuff on path so we can use it
;; (defun set-exec-path-from-shell-PATH ()
;;   "Sets the exec-path to the same value used by the user shell"
;;   (let ((path-from-shell
;;          (replace-regexp-in-string
;;           "[[:space:]\n]*$" ""
;;           (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))
;; (set-exec-path-from-shell-PATH)

;; turn off the welcome screen
(setq inhibit-startup-message t)

;; tabs settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 80 4))

;; indenting
(load-file "~/.emacs.d/dumbdent/dumbdent.el")

;; turn off autosave
(setq auto-save-default nil)

;; start size
(setq default-frame-alist '(
                            (width . 100)
                            (height . 60) ))

;; (electric-pair-mode 0)


;;;;;;;;;;;;;;; keybindings

(defun very-evil-map (keys func)
  "define a keybinding for all evil modes"
  (define-key evil-normal-state-map keys func)
  (define-key evil-insert-state-map keys func)
  (define-key evil-visual-state-map keys func))

(defun way-down () (interactive) (evil-next-line 15))
(defun way-up () (interactive) (evil-previous-line 15))
(very-evil-map [next] 'way-down)
(very-evil-map [prior] 'way-up)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "C-=") 'iwb)

(very-evil-map "\C-y" 'yank)
(very-evil-map "\C-a" 'move-beginning-of-line)
(very-evil-map [home] 'move-beginning-of-line)
(very-evil-map "\C-e" 'move-end-of-line)
(very-evil-map [end] 'move-end-of-line)

;; stop foot-shooting with evil-downcase
(define-key evil-normal-state-map "gu" nil)
(define-key evil-normal-state-map "gd" 'evil-downcase)

;; get back emacs behavior so i can learn...
(very-evil-map "\C-b" 'evil-backward-char)
(very-evil-map "\C-f" 'evil-forward-char)
(very-evil-map "\C-n" 'evil-next-line)
(very-evil-map "\C-p" 'evil-previous-line)

;; move between windows with M-arrows
;; (windmove-default-keybindings 'meta)
(windmove-default-keybindings)

;; when typing, return does nice indenting too
;; note to self: C-m == RET
;; (define-key evil-insert-state-map (kbd "RET") 'reindent-then-newline-and-indent)

;; evil-nerd-comment
(define-key evil-normal-state-map "\M-;" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map "\M-;" 'evilnc-comment-or-uncomment-lines)

;; escape key gets you out of hell
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; window splits
(global-set-key (kbd "C-|") 'split-window-horizontally)
(global-set-key (kbd "C-\\") 'split-window-vertically)

;; flycheck
(very-evil-map "\M-N" 'flycheck-next-error)
(evil-define-key 'normal flycheck-mode-map (kbd "<M-N>") 'flycheck-next-error)
(very-evil-map "\M-P" 'flycheck-previous-error)
(evil-define-key 'normal flycheck-mode-map (kbd "<M-N>") 'flycheck-previous-error)
;; (evil-define-key 'insert slime-repl-map (kbd "<M-n>") 'slime-repl-backward-input)
;; (evil-define-key 'insert slime-repl-map (kbd "<M-p>") 'slime-repl-forward-input)
;; (evil-define-key 'insert slime-repl-map (kbd "<M-n>") 'slime-repl-backward-input)
;; (evil-define-key 'insert slime-repl-map (kbd "<M-p>") 'slime-repl-forward-input)

;; buffer switching
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-k") 'evil-switch-to-windows-last-buffer)
;; (very-evil-map "\M-n" 'next-buffer)
;; (very-evil-map "\M-p" 'previous-buffer)

;; fix common :W typo
(evil-ex-define-cmd "W[rite]" 'evil-write)

;; dumbdenting
(very-evil-map [C-tab] 'dumbdent-line-or-region)
(very-evil-map [S-tab] 'dumbdedent-line-or-region)
(very-evil-map [backtab] 'dumbdedent-line-or-region)

;; copy-paste like a normal person
(cua-mode t)
(setq select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)
;; but now my brain hurts because mac/linux
;; C-x for cut in particular interferes with shit
;; (define-key evil-insert-state-map (kbd "C-c") 'cua-copy-region)
;; (define-key evil-insert-state-map (kbd "C-v") 'cua-paste)
;; (define-key evil-insert-state-map (kbd "C-x") 'cua-cut-region)
(very-evil-map (kbd "s-c") 'cua-copy-region)
(very-evil-map (kbd "s-v") 'cua-paste)
(very-evil-map (kbd "s-x") 'cua-cut-region)

;; other mac idioms I can't remove from my fingers
(very-evil-map (kbd "s-s") 'save-buffer)
(very-evil-map (kbd "s-w") 'kill-this-buffer)
(very-evil-map (kbd "s-a") 'mark-whole-buffer)

;;;;;;;;;;;;;; languages

;; common lisp
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (add-to-list 'auto-mode-alist '("\\.ros\\'" . slime-mode))
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

;; racket scheme
(setq racket-program "/Applications/Racket v6.12/bin/racket")

;; Vagrant is ruby
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; yaml/raml
(add-to-list 'auto-mode-alist '("\\.raml$" . yaml-mode))
(add-hook 'yaml-mode-hook (lambda () (setq tab-width 2)))

;; js
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; jsp
(add-to-list 'auto-mode-alist '("\\.jsp$" . nxml-mode))

;; clojure

;; cider doesn't work with java 9 https://github.com/technomancy/leiningen/issues/2149
(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages t)
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'cider-mode-hook
  (lambda () (setq next-error-function #'flycheck-next-error-function)))

;; indent in html/xml/templates
(setq sgml-basic-offset 4)
;; might need to add this to a hook
(setq nxml-child-indent 4)

;; python
(require 'py-isort)
;; (load-file "~/.emacs.d/python-flake8/python-flake8.el")

;; (setq jedi:complete-on-dot t)
;; (setq jedi:get-in-function-call-delay 200)

(defun insert-pdb-trace ()
  "Why spend your whole life typing?"
  (interactive)
  (insert "import ipdb;ipdb.set_trace()"))

(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (setq tab-width 4)
              (jedi:setup)
              (define-key evil-insert-state-map (kbd "RET") 'evil-ret)
              (very-evil-map (kbd "C-x p") 'insert-pdb-trace)
              (very-evil-map (kbd "s-b") 'jedi:goto-definition))))

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'python-mode-hook
;;           (lambda () (define-key evil-insert-state-map (kbd "RET") 'evil-ret)))
;; (add-hook 'python-mode-hook
;;           (lambda () (very-evil-map (kbd "C-x p") 'insert-pdb-trace)))
;; (add-hook 'python-mode-hook
;;           (lambda () (very-evil-map (kbd "s-b") 'jedi:goto-definition)))



;; c
(setq c-default-style "linux"
      c-basic-offset 4)

;; json
(setq json-reformat:pretty-string? t)

;; js
(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))

;; golang
;; (setq gofmt-command "goimports")
(add-hook 'go-mode-hook
      (lambda ()
        (set (make-local-variable 'company-backends) '(company-go))
        (company-mode)
        ;; wide tab
        (setq tab-width 4 indent-tabs-mode 1)
        ;; always gofmt
        (add-hook 'before-save-hook 'gofmt-before-save)
        ;; disable go-build in flycheck because code doesn't actually
        ;; compile on my host... womp womp
        ;;(add-to-list 'flycheck-disabled-checkers 'go-build)
        ;; godef shortcuts
        (very-evil-map (kbd "s-b") 'godef-jump) ;; intellij muscle memory crutch
        (very-evil-map (kbd "s-.") 'godef-jump)
        (very-evil-map (kbd "s-,") 'pop-tag-mark)))

(setenv "GOPATH" "/Users/evan.bender/go")
(setenv "CGO_CXXFLAGS_ALLOW" "-lpthread")
(add-to-list 'exec-path "/Users/evan.bender/go/bin")
;; (add-hook 'before-save-hook 'gofmt-before-save)
;; (add-hook 'go-mode-hook (lambda () (setq tab-width 8 indent-tabs-mode 1)))

;; java
;; (require 'meghanada)
;; (load-file "~/.emacs.d/flycheck-infer/flycheck-infer.el")

;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             ;; meghanada-mode on
;;             (meghanada-mode t)
;;             (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
;; eclim
;; (require 'eclim)
;; (require 'eclimd)
;; (global-eclim-mode)
(require 'auto-complete-config)
(ac-config-default)
;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)
;; (custom-set-variables
;;   '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse/"))
;;   '(eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim"))
;; (require 'eclim)
;; (setq eclimd-autostart t)
;; (global-eclim-mode)

;; rust
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-to-list 'auto-mode-alist '("\\.md$" . jekyll-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.html" . jekyll-html-mode))

;;;;;;;;;;;;;;;;;;;;;; completion
(require 'company)
(require 'company-go)

;; (add-hook 'after-init-hook 'global-company-mode)
(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      ;;(dabbrev-expand nil)
      (hippie-expand nil)
      ;;(completion-at-point)
      ;;(company-complete)
      ;;(ac-complete nil)

    (indent-for-tab-command)
    ))
(define-key evil-insert-state-map [tab] 'indent-or-complete)
;; (define-key evil-insert-state-map [tab] 'company-complete)
;; (define-key evil-insert-state-map [C-tab] 'indent-for-tab-command)

;; TODO fuck around with hippie-expand, seems promising
(global-set-key "\M-/" 'hippie-expand)


;;;;;;;;;;;;;; utility
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c C-c") 'switch-to-previous-buffer) ;; like screen
;; (define-key erc-mode-map (kbd "C-c C-c") 'switch-to-previous-buffer)

(defun multiline-it ()
  (interactive)
  ;; narrow to region around current line
  (end-of-line)
  (set-mark-command nil)
  (beginning-of-line)
  (narrow-to-region (mark) (point))
  ;; replace in region
  (replace-string ", " ",\n")
  ;; indent in region
  (beginning-of-buffer)
  (set-mark-command nil)
  (end-of-buffer)
  (indent-region (mark) (point))
  ;; back to normal
  (widen))

(defun multiline-def ()
  (interactive)
  ;; narrow to region around current line
  (end-of-line)
  (set-mark-command nil)
  (beginning-of-line)
  (narrow-to-region (mark) (point))
  ;; replace in region
  ;; this is a littly janky as each replace
  ;; leaves mark at last occurence, but as long
  ;; as the order of these is right it works. :shrug:
  (replace-string "(" "(\n")
  (replace-string ", " ",\n")
  (replace-string ")" ",\n)")
  ;; indent in region
  (beginning-of-buffer)
  (set-mark-command nil)
  (end-of-buffer)
  (indent-region (mark) (point))
  ;; back to normal
  (widen))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))



;;;;;;;;;;;;; tidyness
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; fun times
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (## racket-mode go-autocomplete company-jedi exec-path-from-shell yaml-mode thrift solarized-theme smex smartparens slime rust-mode rjsx-mode rainbow-delimiters py-isort protobuf-mode powerline meghanada markdown-preview-mode magit lusty-explorer json-mode jinja2-mode jekyll-modes jedi ido-vertical-mode hy-mode graphviz-dot-mode flycheck-rust flycheck-clojure flx-ido evil-surround evil-nerd-commenter erlang company-go column-marker))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
