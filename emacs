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

(setq packages-i-use `(
                       column-enforce-mode ;; reminder at 80
                       company             ;; completion
                       company-go          ;;   for go
                       evil                ;; vi mode
                       evil-nerd-commenter ;; fast block commenting
                       evil-surround       ;; surround.vim
                       exec-path-from-shell ;; path mangling
                       flx-ido             ;; fuzzy matching in ido mode
                       flycheck            ;; inline code checks
                       ;;go-eldoc-mode       ;; eldoc for go
                       graphviz-dot-mode   ;; dot file mode
                       ido-vertical-mode   ;; ido mode look nicer
                       json-mode           ;; json mode
                       lsp-mode            ;; language server for many langs
                       magit               ;; git history browsing
                       markdown-mode       ;; markdown mode
                       neotree             ;; left-sidebare file nav
                       projectile          ;; it understands projects
                       protobuf-mode       ;; proto mode
                       rainbow-delimiters  ;; easier to see stacks of parens
                       racer               ;; lighter-weight-than-lsp rust completion and navigation
                       ;; rustic              ;; rust mode extended
                       smartparens         ;; magic paren matching
                       smex                ;; ido for M-x
                       solarized-theme     ;; best color scheme
                       toml-mode           ;; toml mode
                       undo-tree           ;; navigate undo history
                       use-package         ;; fancy way to load packages
                       yaml-mode           ;; yaml mode
                       yasnippet           ;; snippets
                       ))


;; where we get our packages from
(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))


(package-initialize)
(setq package-enable-at-startup nil)

(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-packages-installed packages-i-use)

;; make path sane
(exec-path-from-shell-initialize)

;; recompile .emacs.d on open
;; sometimes this is useful. it just takes a while -- usually keep it commented
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(load-theme 'solarized-light t)
;; (load-theme 'solarized-dark t)

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
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)

;; colooors
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; typeface
(set-frame-font "Menlo 13" nil t)

;; do we want parens to match? depends on how I feel today
;;(smartparens-global-mode 1)

;; don't save too much history
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

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
;;(add-hook 'after-init-hook #'global-flycheck-mode)
;; (add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
;; (require 'go-flymake)

;;(require 'midnight)
;;(setq clean-buffer-list-delay-general 1)

;; visual cue when I go over 80 columns
(global-column-enforce-mode t)
;;(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; window navigation
(winner-mode 1)

;; where am I?
(which-function-mode)

;; shush warning when working within file tree symlinked into gopath
(setq find-file-suppress-same-file-warnings 1)

;; file tree navigation
(require 'neotree)
(setq neo-smart-open t)
(setq neo-window-width 55)
;; (global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;; project navigation
(projectile-mode +1)
(setq projectile-switch-project-action 'neotree-projectile-action)


(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))
(global-set-key [f8] 'neotree-project-dir)


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


(defun very-local-map (keys func)
  "define a buffer-local keybinding for all evil modes"
  (define-key evil-normal-state-local-map keys func)
  (define-key evil-insert-state-local-map keys func)
  (define-key evil-visual-state-local-map keys func))


(defun way-down () (interactive) (evil-next-line 15))
(defun way-up () (interactive) (evil-previous-line 15))
(very-evil-map [next] 'way-down)
(very-evil-map [prior] 'way-up)

;; fml my mac's down key is borked
(very-evil-map "\C-d" 'way-down)
(very-evil-map "\C-u" 'way-up)


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
;; (very-evil-map "\M-N" 'flymake-goto-next-error)
;; (very-evil-map "\M-P" 'flymake-goto-prev-error)

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

;; muscle memory from intellij
(very-evil-map (kbd "s-O")'projectile-find-file)


;;;;;;;;;;;;;; languages

(use-package flycheck
;;  :hook (prog-mode . flycheck-mode))
  :hook (go-mode . flycheck-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 5))

;; Vagrant is ruby
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; markdown mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; yaml/raml
(add-to-list 'auto-mode-alist '("\\.raml$" . yaml-mode))
(add-hook 'yaml-mode-hook (lambda () (setq tab-width 2)))

;; indent in html/xml/templates
(setq sgml-basic-offset 4)
;; might need to add this to a hook
(setq nxml-child-indent 4)

;; go
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-enable-file-watchers nil)
  :hook
  ((go-mode) . lsp))
;; To set the garbage collection threshold to high (100 MB) since LSP client-server communication generates a lot of output/garbage
(setq gc-cons-threshold 100000000)
;; To increase the amount of data Emacs reads from a process
(setq read-process-output-max (* 1024 1024))

(setq exec-path (append exec-path '("/Users/evan.bender/go/bin/")))
;; (use-package company-go
;;   :config
;;   (add-to-list 'company-backends 'company-go))
;;(setq gofmt-command "goimports")
(add-hook 'go-mode-hook
      (lambda ()
        ;; completion
        ;; https://emacs.stackexchange.com/questions/64038/how-to-use-multiple-backends-in-priority-for-company-mode
        (set (make-local-variable 'company-backends) '((company-go company-dabbrev-code company-keywords)))

        (define-key evil-insert-state-local-map [tab] #'company-indent-or-complete-common)
        ;; wide tab
        (setq tab-width 8 indent-tabs-mode 1)
        ;; always gofmt
        (add-hook 'before-save-hook 'gofmt-before-save)
        ;; (go-eldoc-setup)
        ;; megacheck makes my laptop start to melt
        (setq flycheck-disabled-checkers '(go-test go-errcheck go-unconvert go-staticcheck))

        (if (not (string-match "go" compile-command))
            (set (make-local-variable 'compile-command) "go test -v"))


        ;; godef shortcuts
        ;;(very-local-map (kbd "s-.") 'xref-find-definitions)
        (very-local-map (kbd "s-.") 'godef-jump)
        (very-local-map (kbd "s-,") 'pop-tag-mark)))


;; (setenv "GOPATH" "/Users/evan.bender/go")
(setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:/opt/homebrew/lib/pkgconfig:/opt/homebrew/Cellar/rocksdb@6.20.3/6.20.3/lib/pkgconfig:/opt/homebrew/opt/openssl/lib/pkgconfig")
;; (setenv "CPPFLAGS" "-I/opt/homebrew/opt/openssl@3/include -I/opt/homebrew/opt/gperftools/include -I/opt/homebrew/Cellar/rocksdb@6.20.3/6.20.3/include -I/opt/homebrew/Cellar/foundationdb-headers@6.2.30/6.2.30/include")
(setenv "CGO_CFLAGS_ALLOW" "-ltcmalloc")
(setenv "CGO_CXXFLAGS_ALLOW" "-lpthread|-ltcmalloc")
(setenv "CGO_CPPFLAGS" "-I/opt/homebrew/opt/openssl@3/include -I/opt/homebrew/opt/gperftools/include -I/opt/homebrew/Cellar/rocksdb@6.20.3/6.20.3/include -I/opt/homebrew/Cellar/foundationdb-headers@6.2.30/6.2.30/include -DCMAKE_EXE_LINKER_FLAG")
;; (setenv "CPATH" "/opt/homebrew/include")
;; (setenv "LIBRARY_PATH" "/opt/homebrew/lib")


(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (setq tab-width 4)
              (define-key evil-insert-state-map (kbd "RET") 'evil-ret)
              (define-key evil-insert-state-local-map [tab] #'company-indent-or-complete-common))))



;; (add-to-list 'exec-path "/Users/evan.bender/go/bin")
;; (add-hook 'before-save-hook 'gofmt-before-save)
;; (add-hook 'go-mode-hook (lambda () (setq tab-width 8 indent-tabs-mode 1)))

;; rust
;; (require 'lsp-mode)
;; (add-hook 'rust-mode-hook #'lsp)
(require 'toml-mode)
(require 'rust-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            ;; (setq rustic-lsp-setup-p nil)
            ;; (setq rustic-lsp-client nil)
            (setq indent-tabs-mode nil)
            (setq rust-format-on-save t)
            (setq rust-rustfmt-bin "rustfmt-nightly")
            ;; (setq flycheck-disabled-checkers '(rust-cargo))
            (setq compile-command "echo disabled")
            (very-local-map (kbd "s-.") 'racer-find-definition)
            (very-local-map (kbd "s-,") 'pop-tag-mark)
            (define-key evil-insert-state-local-map [tab] #'company-indent-or-complete-common)))
;; EB TODO https://evil.readthedocs.io/en/latest/keymaps.html
;; evil does have a buffer-local implementation

;; rust attempt II
;; (use-package rustic
;;   :ensure
;;   :bind (:map rustic-mode-map
;;               ("M-j" . lsp-ui-imenu)
;;               ("M-?" . lsp-find-references)
;;               ("C-c C-c l" . flycheck-list-errors)
;;               ("C-c C-c a" . lsp-execute-code-action)
;;               ("C-c C-c r" . lsp-rename)
;;               ("C-c C-c q" . lsp-workspace-restart)
;;               ("C-c C-c Q" . lsp-workspace-shutdown)
;;               ("C-c C-c s" . lsp-rust-analyzer-status))
;;   :config
;;   ;; uncomment for less flashiness
;;   ;; (setq lsp-eldoc-hook nil)
;;   ;; (setq lsp-enable-symbol-highlighting nil)
;;   ;; (setq lsp-signature-auto-activate nil)

;;   ;; comment to disable rustfmt on save
;;   (setq rustic-format-on-save t)
;;   (setq rustic-rustfmt-bin "rustfmt-nightly"))
;;   ;;(add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; (use-package lsp-mode
;;   :ensure
;;   :commands lsp
;;   :custom
;;   ;; what to use when checking on-save. "check" is default, I prefer clippy
;;   (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   (lsp-eldoc-render-all t)
;;   (lsp-idle-delay 0.6)
;;   ;; enable / disable the hints as you prefer:
;;   (lsp-rust-analyzer-server-display-inlay-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
;;   (lsp-rust-analyzer-display-chaining-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
;;   (lsp-rust-analyzer-display-closure-return-type-hints t)
;;   (lsp-rust-analyzer-display-parameter-hints nil)
;;   (lsp-rust-analyzer-display-reborrow-hints nil)
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package lsp-ui
;;   :ensure
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-doc-enable nil))



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
;;(define-key evil-insert-state-map [tab] 'indent-or-complete)
;;(define-key evil-insert-state-map [tab] 'company-complete)
;; (define-key evil-insert-state-map [C-tab] 'company-complete)


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

(defun oops-that-might-error ()
  (interactive)
  (back-to-indentation)
  (insert "if err := ")
  (end-of-line)
  (insert "; err != nil {\n // do something \n}")
  (indent-for-tab-command)
  (forward-line -1)
  (indent-for-tab-command))


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


;; emacs, please don't put customizations noise here!
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
