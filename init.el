;; -*- lexical-binding: t; -*-

(setq my/org-config
      (expand-file-name "config.org" user-emacs-directory))
(org-babel-load-file my/org-config)
