;; -*- lexical-binding: t; -*-

;;; remove GUI elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; otherwise this file will get poluted with custom variables
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;;; start in scratch mode directly
(setq inhibit-startup-screen t)

;;; please. no ~backup or #autosave# files
(setq make-backup-files nil
      auto-save-default nil)

;;; straight.el bootstrap
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; core packages
(straight-use-package 'use-package)
(use-package doom-themes
  :config
  ;; defaults
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t))
(use-package org)
(use-package projectile
  :config
  (projectile-mode 1))
(use-package evil
  :config
  (evil-mode 1))

;;; magit
(use-package magit)
(use-package evil-magit)

;;; ivy
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))
(use-package ivy-rich
  :config
  (ivy-rich-mode 1))
;; don't forget to run M-x all-the-icons-install-fonts on first install
(use-package all-the-icons-ivy-rich
  :config (all-the-icons-ivy-rich-mode 1))
(use-package counsel
  :config
  (counsel-mode 1))
(use-package counsel-projectile)

;;; LSP support
(use-package company)
(use-package flycheck)
(use-package lsp-mode)
(use-package lsp-ui)
(use-package lsp-ivy)
(use-package company-lsp)

;;; languages
(use-package rustic
  :config
  (setq rustic-lsp-format t))
