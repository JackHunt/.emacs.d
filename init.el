
;; Set some sane UI defaults.
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; Set font - TODO: set size.
(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)

;; Setup use-package
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Theme - TODO: get from xresources.
(use-package doom-themes
  :init (load-theme 'doom-material-dark))

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
 

;; Setup nice modeline - TODO: customize height
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Keep command history.
(use-package command-log-mode)

;; Setup Ivy.
(use-package ivy
  :diminish
;;  :bind (("C-s" . swiper)
;;         :map ivy-minibuffer-map
;;         ("TAB" . ivy-alt-done)	
;;         ("C-l" . ivy-alt-done)
;;         ("C-j" . ivy-next-line)
;;         ("C-k" . ivy-previous-line)
;;         :map ivy-switch-buffer-map
;;         ("C-k" . ivy-previous-line)
;;         ("C-l" . ivy-done)
;;         ("C-d" . ivy-switch-buffer-kill)
;;         :map ivy-reverse-i-search-map
;;         ("C-k" . ivy-previous-line)
;;         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Command descriptions & key combos in ivy.
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)) 

;; Setup counsel.
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))) 
 
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

;; Projectile.
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
;;  :bind-keymap
;;  ("C-c p" . projectile-command-map)
  :init
  ;; TODO: Update dirs.
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit
;; TODO: Check out the other buffers.
;; https://magit.vc/manual/magit/Switching-Buffers.html
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" default))
 '(package-selected-packages
   '(counsel-projectile yasnippet-snippets yaml-mode which-key use-package smart-mode-line rainbow-delimiters py-autopep8 projectile org-bullets neotree magit lsp-haskell key-quiz ivy-rich irony-eldoc helpful haskell-snippets flycheck-irony exec-path-from-shell doom-themes doom-modeline diff-hl counsel company-jedi company-irony command-log-mode auto-compile auctex atom-one-dark-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
