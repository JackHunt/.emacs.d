(package-initialize)
;; Load Emacs Config from Org file.
  (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
  (garbage-collect))
