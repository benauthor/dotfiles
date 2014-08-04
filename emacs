;; -*-Emacs-Lisp-*-


(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
;; recompile .emacs.d on open
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(load-theme 'solarized-dark t)

(require 'rainbow-delimiters)
(require 'evil)
(require 'ido)
(require 'smex)
(require 'powerline)
;; (require 'erc)
;; (require 'flycheck)
(require 'midnight)
(require 'column-marker)
;; (require 'js-comint)
(require 'lusty-explorer)
(require 'zencoding-mode)
(require 'yasnippet)
;; (require 'cider)
;; (load "~/.emacs.d/nxhtml/autostart.el")

;;(setq inferior-lisp-program "/Users/bendere/local/bin/lein repl")
;; (setq inferior-lisp-program "/Users/bendere/local/bin/clisp")
;; (setq inferior-lisp-program "/Users/bendere/local/bin/sbcl")
;; (add-to-list 'load-path "~/.emacs.d/slime")
;; (require 'slime-autoloads)
;; (setq slime-contribs '(slime-repl))
;; (setq slime-contribs '(slime-fancy))

(evil-mode 1)
(ido-mode t)
(yas-global-mode 1)

(load "~/.emacs.d/evil-surround/surround.el")
(require 'surround)
(global-surround-mode 1)

(powerline-default-theme)
(global-rainbow-delimiters-mode)
(setq evil-default-cursor t)
(set-cursor-color "#fb0")
(blink-cursor-mode 0)
(tool-bar-mode -1)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))
(cua-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; (global-linum-mode 1)
;; (fringe-mode 1)
;; (setq linum-format " %d ")
(setq vc-follow-symlinks nil)



;;;;;;;;;;;;; sane defaults
;; exec path
;;(setq exec-path (append exec-path '("/Users/bendere/local/bin" "/Users/bendere/.nvm/v0.10.15/bin")))

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

;; put stuff on path so we can use it
(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
;; call function now
(set-exec-path-from-shell-PATH)

;; turn off the welcome screen
(setq inhibit-startup-message t)

;; whitespace
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 4 80 4))

;; turn off autosave
(setq auto-save-default nil)

;; start size
(setq default-frame-alist '(
                            (width . 140)
                            (height . 50) ))

;;;;;;;;;;;;; habits
;; tramp
(setq tramp-default-method "ssh")



;;;;;;;;;;;;;;; keybindings

;; crazy idea
;;(evil-insert-state-map (kbd "jk") 'evil-next-line)
;;(key-chord-define evil-insert-state-map ",," 'evil-normal-state)


(defun very-evil-map (keys func)
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
;; semicolon can do what colon does, this is nice
;;(define-key evil-normal-state-map ";" 'evil-ex)
;;;;(define-key evil-normal-state-map ";" 'evil-repeat-find-char)
;; buffer switching
(global-set-key (kbd "C-n") 'next-buffer)
(global-set-key (kbd "C-p") 'previous-buffer)
(very-evil-map "\C-n" 'next-buffer)
(very-evil-map "\C-p" 'previous-buffer)
;; behave in shell too
(add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map (kbd "C-n") 'next-buffer)
            (define-key comint-mode-map (kbd "C-p") 'previous-buffer)
            ))
;; so when the buffer menu opens you are in it
(global-set-key (kbd "\C-x\C-b") 'buffer-menu-other-window)
;; move between windows with M-arrows
(windmove-default-keybindings 'meta)
;;smex
(global-set-key [(meta x)] 'smex)
;; when typing, return does nice indenting too
;; note to self: C-m == RET
(define-key evil-insert-state-map (kbd "RET") 'reindent-then-newline-and-indent)

;; evil-nerd-comment

(define-key evil-normal-state-map "\M-;" 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map "\M-;" 'evilnc-comment-or-uncomment-lines)
;; kinda like screen
;;(global-set-key (kbd "C-c C-c") 'ansi-term)
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
;; splits
(global-set-key (kbd "C-|") 'split-window-horizontally)
(global-set-key (kbd "C-\\") 'split-window-vertically)
;; flycheck
;;(very-evil-map "\M-n" 'flycheck-next-error)
;; (evil-define-key 'normal flycheck-mode-map (kbd "<M-n>") 'flycheck-next-error)
;;(very-evil-map "\M-p" 'flycheck-previous-error)
;; (evil-define-key 'normal flycheck-mode-map (kbd "<M-p>") 'flycheck-previous-error)
(evil-define-key 'insert slime-repl-map (kbd "<M-n>") 'slime-repl-backward-input)
(evil-define-key 'insert slime-repl-map (kbd "<M-p>") 'slime-repl-forward-input)
;; (evil-define-key 'insert slime-repl-map (kbd "<M-n>") 'slime-repl-backward-input)
;; (evil-define-key 'insert slime-repl-map (kbd "<M-p>") 'slime-repl-forward-input)

;; lustyexplorer
(global-set-key (kbd "C-x C-b") 'lusty-buffer-explorer)
;;(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
;;(global-set-key (kbd "C-x b") 'lusty-buffer-explorer)
(global-set-key (kbd "C-x f") 'lusty-file-explorer)

;; fix common :W typo
(evil-ex-define-cmd "W[rite]" 'evil-write)



;;;;;;;;;;;;;; highlighting
;; a very basic scss mode
(define-derived-mode scss-mode css-mode "SCSS"
  ;; Add the single-line comment syntax ('//', ends with newline)
  ;; as comment style 'b' (see "Syntax Flags" in elisp manual)
  (modify-syntax-entry ?/ ". 124" css-mode-syntax-table)
  (modify-syntax-entry ?* ". 23b" css-mode-syntax-table)
  (modify-syntax-entry ?\n ">" css-mode-syntax-table))
(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))
;; markdown mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; js
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; pt gets highlighting
(setq auto-mode-alist
      (append '((".*\\.pt\\'" . xml-mode))
              auto-mode-alist))
;; jsp
(add-to-list 'auto-mode-alist '("\\.jsp$" . nxml-mode))
;; pt
(add-to-list 'auto-mode-alist '("\\.pt$" . nxml-mode))
;; clojure
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;; clojurecript
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
;; (add-hook 'cider-mode-hook
;;           '(lambda ()
;;              (define-key cider-mode-map "RET" 'cider-repl-return)))
(evil-define-key 'normal clojure-mode-map (kbd "<C-return>") 'cider-eval-expression-at-point)
(evil-define-key 'insert clojure-mode-map (kbd "<C-return>") 'cider-eval-expression-at-point)
(evil-define-key 'visual clojure-mode-map (kbd "<C-return>") 'cider-eval-region)
;(evil-define-key 'insert cider-repl-mode-map (kbd "<C-return>") 'cider-repl-return)
;(evil-define-key 'insert cider-repl-mode-map (kbd "<return>") 'cider-repl-return)

;; clojure
(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map
               "\C-c\C-e" '(lambda ()
                             (interactive)
                             (let ((curr (point)))
                               (end-of-defun)
                               (lisp-eval-last-sexp)
                               (goto-char curr))))
             (define-key clojure-mode-map
               "\C-x\C-e" 'lisp-eval-last-sexp)
             (define-key clojure-mode-map
               "\C-c\C-r" 'lisp-eval-region)
             (define-key clojure-mode-map
               "\C-c\C-c" '(lambda ()
                             (interactive)
                             (lisp-eval-string (buffer-string))))
             (define-key clojure-mode-map
               "\C-c\C-z" 'run-lisp)))



;; completion
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



;; erc
;;(erc-autojoin-mode t)
;;(setq erc-autojoin-channels-alist
;;  '(("irc.usnews.com" "#usnews")))



;;;;;;;;;;;;;; utility
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))



;;;;;;;;;;;;; tidyness
;; midnight-mode: 1 day
(setq clean-buffer-list-delay-general 1)
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

;; js-comint
(setenv "NODE_NO_READLINE" "0")
(setq inferior-js-program-command "node")
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))

;; multi-web-mode
;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;;  (setq mweb-tags '((js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;;  (setq mweb-filename-extensions '("php" "htm" "html"))
;;  (multi-web-global-mode 1)

;; web-mode
;; (load-file "~/.emacs.d/web-mode/web-mode.el")
;; (require 'web-mode)
;; (setq web-mode-css-indent-offset 4)
;; (setq web-mode-code-indent-offset 4)

(load-file "~/.emacs.d/dumbdent/dumbdent.el")
(load-file "~/.emacs.d/python-flake8/python-flake8.el")

(very-evil-map [C-tab] 'dumbdent-line-or-region)
(very-evil-map [S-tab] 'dumbdedent-line-or-region)

;; buffer switch
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c C-c") 'switch-to-previous-buffer)
(define-key erc-mode-map (kbd "C-c C-c") 'switch-to-previous-buffer)

;; zencoding on markup modes
;; TODO this doesn't work on nxml?
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'xml-mode-hook 'zencoding-mode)
(add-hook 'nxml-mode-hook 'zencoding-mode)

;; indent in html/xml/templates
;;(setq sgml-basic-offset 4)
;; might need to add this to a hook
(setq nxml-child-indent 4)

;; python
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (lambda () (define-key
                                         evil-insert-state-map
                                         (kbd "RET")
                                         'evil-ret)))

(setq jedi:complete-on-dot t)


;; shut up annoying mumamo warnings
(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))
