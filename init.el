;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASE EMACS SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set some sane UI defaults.
(when (display-graphic-p)
  (setq inhibit-startup-message t)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (menu-bar-mode -1)
  (global-hl-line-mode))

;; Handle # properly on a mac.
(if (eq system-type 'darwin)
    (global-set-key (kbd "M-3") #'(lambda () (interactive) (insert "#"))))

;; Add some brew paths for mac.
(if (eq system-type 'darwin)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/texbin")))
  (setq exec-path (append exec-path '("/usr/bin")))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

;; Set font - TODO: set size.
(set-face-attribute 'default nil :font "Fira Code Retina" :height 110)

;; C-[ maps to ESC, which is annoyingly close to C-p, meaning it's easy
;; to accidentally close all windows.
(global-unset-key (kbd "ESC ESC ESC"))

;; Match parens, quotations etc.
(electric-pair-mode 1)

;; Nuke trailing whitespace on save.
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Tabs are evil.
(setq-default indent-tabs-mode nil)

;; Helper for reloading config.
(defun jh/reload-config ()
  "Reload .emacs.d/init.el"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Don't store backup files with sources - it's a pain with git.
(setq backup-directory-alist
  `(("." . , (expand-file-name "backups" temporary-file-directory))))

;; Ditto for autosaves.
(setq auto-save-file-name-transforms `((".*" , temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE MANAGEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap straight.
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "11:00"))

;; Make sure env is sane on Mac.
;; This should be in the above section, but relies on use-package.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THEMES & APPEARANCE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom-acario-dark
;; (use-package doom-themes
;;   :init (load-theme 'doom-badger t))
(use-package nord-theme
  :init (load-theme 'nord t))

;; Colour coded parenthesis etc.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Line & column numbers. Disabled for org & term.
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Lines showing indentation.
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method 'bitmap))


;; Setup nice modeline.
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

;; Fancy icons.
(use-package all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Ligatures.
(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package speed-type)

(use-package key-quiz)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice
        (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 10)
                          (projects . 10)
                          (registers . 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COUNSEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :init (global-company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
         ("<tab>" . company-indent-or-complete-common)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELISP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO verify this usage of with-eval-after-load
(with-eval-after-load 'emacs-lisp-mode
    (add-to-list 'company-backends 'company-elisp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IVY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-partial-or-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

;; Command descriptions & key combos in ivy.
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELP & LEARNING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shows possible key bindings given a partial sequence.
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Better docs for C-h v etc
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECTILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  :custom
  ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/GitHub")
    (setq projectile-project-search-path '("~/GitHub")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASNIPPET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAGIT & DIFF-HL
;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Check out the other buffers.
;; https://magit.vc/manual/magit/Switching-Buffers.html
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :hook
  ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
   (magit-post-refresh-hook .diff-hl-magit-post-refresh)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :init (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLYSPELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell
  :init
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  :config
  (setq ispell-dictionary "british"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO verify this usage of with-eval-after-load
(with-eval-after-load 'c++-mode
  (add-to-list 'company-backends 'company-clang))

(add-hook 'c++-mode-hook
  (lambda () (setq flycheck-clang-language-standard "c++20")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package anaconda-mode
  :hook
  ((python-mode-hook . anaconda-mode)
   (python-mode-hook . anaconda-eldoc-mode)))

(use-package company-anaconda
  :after (anaconda-mode company)
  :config
  (add-to-list 'company-backends 'company-anaconda))

;;(setenv "WORKON_HOME" (concat (getenv "CONDA_PREFIX") "/envs"))
;; As a hack, just symlink conda env dir to ~/.virtualenvs for now
(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R & STAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ess)

(use-package stan-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SHELL SCRIPTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sh-basic-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HASKELL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package haskell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auctex
  :defer t
  :hook
  ((LaTeX-mode-hook . visual-line-mode)
   (LaTeX-mode-hook . flyspell-mode)
   (LaTeX-mode-hook . flycheck-mode)
   (LaTeX-mode-hook . LaTeX-math-mode))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq TeX-indent-level 2)
  (setq LaTeX-item-indent 2)
  (setq TeX-brace-indent-level 2))

(use-package reftex
  :after auctex
  :hook (LaTeX-mode-hook . turn-on-reftex)
  :config
  (setq reftex-plug-into-AUCTeX t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TERMINALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIRED
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dired
  :straight (:type built-in)
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
  :commands (dired dired-jump))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nicer bullets.
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Babel for literate programming/notebooks.
(use-package jupyter)

;; Needs https://github.com/FooSoft/anki-connect
(use-package anki-editor
  :straight (:fork "orgtre")
  :config
  (setq anki-editor-create-decks t)
  (defun jh/anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))))

;; Function to generate a css file from the current theme for org export.
(defun jh/theme-to-css (filename)
  "Generate a CSS file based on current theme for Org HTML export."
  (interactive "FEnter the output CSS file name: ")
  (with-temp-file filename
    ;; Document background & foreground.
    (let ((default-bg (face-background 'default))
          (default-fg (face-foreground 'default)))
      (insert (format "body {\n  background-color: %s;\n  color: %s;\n}\n" default-bg default-fg)))

    ;; Headings.
    (let ((level-1-bg (face-background 'org-level-1))
          (level-1-fg (face-foreground 'org-level-1)))
      (insert (format "h1 {\n  background-color: %s;\n  color: %s;\n}\n" level-1-bg level-1-fg)))
    (let ((level-2-bg (face-background 'org-level-2))
          (level-2-fg (face-foreground 'org-level-2)))
      (insert (format "h2 {\n  background-color: %s;\n  color: %s;\n}\n" level-2-bg level-2-fg)))
    (let ((level-3-bg (face-background 'org-level-3))
          (level-3-fg (face-foreground 'org-level-3)))
      (insert (format "h3 {\n  background-color: %s;\n  color: %s;\n}\n" level-3-bg level-3-fg)))
    (let ((level-4-bg (face-background 'org-level-4))
          (level-4-fg (face-foreground 'org-level-4)))
      (insert (format "h4 {\n  background-color: %s;\n  color: %s;\n}\n" level-4-bg level-4-fg)))
    ))

(defun jh/org-archive-done-kill-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "1/DONE|1/KILL" 'file))

(defun jh/org-last-modified-update ()
  "Update '#+last_modified:' if it exists in an org buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+last_modified: .*" nil t)
      (replace-match (concat "#+last_modified: " (format-time-string "[%Y-%m-%d %a %H:%M]"))))))

(defun jh/org-mode-setup ()
  ;; Update roam timestamps.
  (add-hook 'before-save-hook 'jh/org-last-modified-update nil 'local)

  ;; TODO states.
  (setq org-todo-keywords
        '((sequence "TODO" "STRT" "IDEA" "WAIT" "|" "DONE" "KILL")))

  (setq org-todo-keyword-faces
      '(("TODO" . (:foreground "cyan" :weight bold))
        ("STRT" . (:foreground "yellow" :weight bold))
        ("IDEA" . (:foreground "pink" :weight bold))
        ("WAIT" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "green" :strike-through t))
        ("KILL" . (:foreground "red" :strike-through t))))


  ;; Make source blocks look a bit nicer.
  (setq org-edit-src-content-indentation 0
    org-src-tab-acts-natively t
    org-src-preserve-indentation t
    org-src-fontify-natively t)

  ;; (setq org-src-window-setup 'current-window)
  (setq org-ellipsis "⤵")

  ;; Enable spell checking.
  (add-hook 'org-mode-hook 'flyspell-mode)

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t)
      (shell . t)
      (jupyter . t)))

  ;;(setq org-export-with-smart-quotes t)
  (setq org-confirm-babel-evaluate nil)

  ;; Enable inline images and make sure they get updated.
  (add-hook 'org-mode-hook 'org-display-inline-images)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (setq org-display-remote-inline-images 'cache)
)

(add-hook 'org-mode-hook 'jh/org-mode-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG ROAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacsql)

(use-package emacsql-sqlite)

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/GitHub/org-roam-notes/"))
  (org-roam-db-location (file-truename "~/GitHub/org-roam-notes/org-roam.sqlite3"))
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'ivy)
  (org-roam-capture-templates
   ;; TODO: See if the headers can be in the org files.
   '(("d" "Default" plain
      (file "~/.emacs.d/org/roam_templates/default.org")
      :target (file+head "%<%Y%m%d%H%M%>-${slug}.org"
                         "\n#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: :some_tag:\n\n")
      :unnarrowed t)
     ("m" "Maths" plain
      (file "~/.emacs.d/org/roam_templates/maths.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "\n#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: :maths:\n\n")
      :unnarrowed t)
     ("i" "Idea" plain
      (file "~/.emacs.d/org/roam_templates/idea.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "\n#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: :idea:\n\n")
      :unnarrowed t)
     ("p" "Paper" plain
      (file "~/.emacs.d/org/roam_templates/paper.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "\n#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: :paper:\n\n")
      :unnarrowed t)
     ("c" "Code Snippet" plain
      (file "~/.emacs.d/org/roam_templates/code_snippet.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "\n#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: :code_snippet:\n\n")
      :unnarrowed t)
     ("r" "Random Thought" plain
      (file "~/.emacs.d/org/roam_templates/random_thought.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "\n#+title: ${title}\n#+created: %U\n#+filetags: :random:\n\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:30}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))
