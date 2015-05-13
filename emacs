;; -*-Emacs-Lisp-*-


(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ))
(package-initialize)

;; recompile .emacs.d on open
;; sometimes this is useful. it just takes a while -- usually keep it commented
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(load-theme 'solarized-dark t)

(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'ido)
(ido-mode t)

(require 'smex)
(global-set-key [(meta x)] 'smex)

(require 'tramp)
(setq tramp-default-method "ssh")

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'midnight)
(setq clean-buffer-list-delay-general 1)

(require 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))

(require 'lusty-explorer)

(require 'powerline)
(powerline-default-theme)

;; slime
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime-autoloads)
(setq slime-contribs '(slime-repl))
(setq slime-contribs '(slime-fancy))

;; magit
(require 'magit)
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

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

;; copy-paste
(cua-mode t)

;; whitespace
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline `visit-save)
(setq mode-require-final-newline `visit-save)

;; enable disabled-by-default
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; don't warn when following symbolic link to version controlled file
(setq vc-follow-symlinks nil)

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
(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

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
                            (width . 140)
                            (height . 42) ))


;;;;;;;;;;;;;;; keybindings

(defun very-evil-map (keys func)
  "define a keybinding for all evil modes"
  (define-key evil-normal-state-map keys func)
  (define-key evil-insert-state-map keys func)
  (define-key evil-visual-state-map keys func))

(defun way-down () (interactive) (evil-next-line 15))

(defun way-up () (interactive) (evil-previous-line 15))

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
(very-evil-map [next] 'way-down)
(very-evil-map [prior] 'way-up)

;; get back emacs behavior so i can learn...
(very-evil-map "\C-b" 'evil-backward-char)
(very-evil-map "\C-f" 'evil-forward-char)
(very-evil-map "\C-n" 'evil-next-line)
(very-evil-map "\C-p" 'evil-previous-line)

;; so when the buffer menu opens you are in it
(global-set-key (kbd "\C-x\C-b") 'buffer-menu-other-window)

;; move between windows with M-arrows
;; (windmove-default-keybindings 'meta)
(windmove-default-keybindings)

;; when typing, return does nice indenting too
;; note to self: C-m == RET
(define-key evil-insert-state-map (kbd "RET") 'reindent-then-newline-and-indent)

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
(very-evil-map "\M-n" 'flycheck-next-error)
(evil-define-key 'normal flycheck-mode-map (kbd "<M-n>") 'flycheck-next-error)
(very-evil-map "\M-p" 'flycheck-previous-error)
(evil-define-key 'normal flycheck-mode-map (kbd "<M-p>") 'flycheck-previous-error)
;; (evil-define-key 'insert slime-repl-map (kbd "<M-n>") 'slime-repl-backward-input)
;; (evil-define-key 'insert slime-repl-map (kbd "<M-p>") 'slime-repl-forward-input)
;; (evil-define-key 'insert slime-repl-map (kbd "<M-n>") 'slime-repl-backward-input)
;; (evil-define-key 'insert slime-repl-map (kbd "<M-p>") 'slime-repl-forward-input)

;; lustyexplorer
(global-set-key (kbd "C-x C-b") 'lusty-buffer-explorer)
;; (global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
;; (global-set-key (kbd "C-x b") 'lusty-buffer-explorer)
(global-set-key (kbd "C-x f") 'lusty-file-explorer)

;; fix common :W typo
(evil-ex-define-cmd "W[rite]" 'evil-write)

;; dumbdenting
(very-evil-map [C-tab] 'dumbdent-line-or-region)
(very-evil-map [S-tab] 'dumbdedent-line-or-region)

;;;;;;;;;;;;;; languages

;; a very basic scss mode
(define-derived-mode scss-mode css-mode "SCSS"
  ;; Add the single-line comment syntax ('//', ends with newline)
  ;; as comment style 'b' (see "Syntax Flags" in elisp manual)
  (modify-syntax-entry ?/ ". 124" css-mode-syntax-table)
  (modify-syntax-entry ?* ". 23b" css-mode-syntax-table)
  (modify-syntax-entry ?\n ">" css-mode-syntax-table))
(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))

;; Vagrant is ruby
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; js
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; jsp
(add-to-list 'auto-mode-alist '("\\.jsp$" . nxml-mode))

;; clojure
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages t)

;; scala
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; thrift mode indents 4
(setq thrift-indent-level 4)

;; indent in html/xml/templates
;;(setq sgml-basic-offset 4)
;; might need to add this to a hook
;; (setq nxml-child-indent 4)

;; python
(defun insert-pdb-trace ()
  "Why spend your whole life typing?"
  (interactive)
  (insert "import ipdb;ipdb.set_trace()"))
(very-evil-map (kbd "C-x p") 'insert-pdb-trace)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (lambda () (define-key
                                        evil-insert-state-map
                                        (kbd "RET")
                                        'evil-ret)))

(setq jedi:complete-on-dot t)
(load-file "~/.emacs.d/python-flake8/python-flake8.el")

;; c
(setq c-default-style "linux"
      c-basic-offset 4)


;;;;;;;;;;;;;;;;;;;;;; completion

(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (dabbrev-expand nil)
      ;;(completion-at-point)
    (indent-for-tab-command)
    ))
(define-key evil-insert-state-map [tab] 'indent-or-complete)

;; TODO fuck around with hippie-expand, seems promising
(global-set-key "\M-/" 'hippie-expand)


;;;;;;;;;;;;;;;;;;; ansi-shell stuff
;; fix unicode in ansi-term
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

;; kill buffer if you kill the term process
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; use bash
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)


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
