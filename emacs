;; -*-Emacs-Lisp-*-


(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ))
(package-initialize)

;; recompile .emacs.d on open
;; sometimes this is useful. it just takes a while -- usually keep it commented
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(load-theme 'solarized-light t)
;; (load-theme 'solarized-dark t)

;; don't warn when following symbolic link to version controlled file
(setq vc-follow-symlinks nil)

(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;(smartparens-global-mode 1)

(require 'ido)
(require 'flx-ido)
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

(require 'smex)
(global-set-key [(meta x)] 'smex)

(require 'tramp)
(setq tramp-default-method "ssh")

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;(require 'midnight)
;;(setq clean-buffer-list-delay-general 1)

(require 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))

(require 'powerline)
(powerline-default-theme)

;; slime
(require 'slime)
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq inferior-lisp-program "/usr/local/bin/ccl64 -K utf-8")
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-fancy))
;; ?
;; (require 'slime-autoloads)

;; magit
;; (require 'magit)
;; (setq magit-auto-revert-mode nil)
;; (setq magit-last-seen-setup-instructions "1.4.0")

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

;; copy-paste like a normal person
(cua-mode t)
(setq select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)
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

;;;;;;;;;;;;;; languages

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
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages t)
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'cider-mode-hook
  (lambda () (setq next-error-function #'flycheck-next-error-function)))

;; indent in html/xml/templates
;;(setq sgml-basic-offset 4)
;; might need to add this to a hook
;; (setq nxml-child-indent 4)

;; python
(defun insert-pdb-trace ()
  "Why spend your whole life typing?"
  (interactive)
  ;; (insert "import pdb;pdb.set_trace()"))
  (insert "import ipdb;ipdb.set_trace()"))
(very-evil-map (kbd "C-x p") 'insert-pdb-trace)

(add-hook 'python-mode-hook (lambda () (setq tab-width 4)))
;; (add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (lambda () (define-key
                                        evil-insert-state-map
                                        (kbd "RET")
                                        'evil-ret)))
(require 'py-isort)

;; (setq jedi:complete-on-dot t)
;; (setq jedi:get-in-function-call-delay 200)
(load-file "~/.emacs.d/python-flake8/python-flake8.el")

;; c
(setq c-default-style "linux"
      c-basic-offset 4)

;; json
(setq json-reformat:pretty-string? t)

;; go
(setq gofmt-command "goimports")
(setenv "GOPATH" "/Users/evanbender/go")
;; (add-hook 'before-save-hook 'gofmt-before-save)
;; (add-hook 'go-mode-hook (lambda () (setq tab-width 8 indent-tabs-mode 1)))

;; java
;; eclim
;; (require 'eclim)
;; (require 'eclimd)
;; (global-eclim-mode)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)
;; (custom-set-variables
;;   '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse/"))
;;   '(eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim"))
;; (require 'eclim)
;; (setq eclimd-autostart t)
;; (global-eclim-mode)

;;;;;;;;;;;;;;;;;;;;;; completion

;; (add-hook 'after-init-hook 'global-company-mode)
(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (dabbrev-expand nil)
      ;;(completion-at-point)
      ;; (company-complete)
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
