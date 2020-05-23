;; Setting up Package manager
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

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

;; Installing and configuring packages 

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config 
  ;;(require 'evil)
  (evil-mode 1))

(use-package counsel
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d"))

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night))

(use-package general
  :config
  ;;(require 'general)
  (general-evil-setup))

;; (use-package vterm)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter)

  ;;(straight-use-package
;; '(el-patch :type git :host github :repo "your-name/el-patch"))

(use-package evil-embrace
  :config
  (evil-embrace-enable-evil-surround-integration)
  :straight
  (evil-embrace
   :type git
   :host github
   :repo "cute-jumper/evil-embrace.el"))

(use-package magit)

(use-package evil-magit
  :config
  ;;(require 'evil-magit)
  :straight
  (evil-magit
   :type git
   :host github
   :repo "emacs-evil/evil-magit"))


(use-package evil-vimish-fold
  :config
  (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  (global-evil-vimish-fold-mode 1)
  :straight
  (evil-vimish-fold
   :type git
   :host github
   :repo "alexmurray/evil-vimish-fold"))

(use-package evil-snipe
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode 1)
  :straight
  (evil-snipe
   :type git
   :host github
   :repo "hlissner/evil-snipe"))

(use-package undo-tree)

(use-package smartparens-config
  :config
  ;;(require 'smartparens-config)
  :straight
  (smartparens-config
   :type git
   :host github
   :repo "Fuco1/smartparens"))

(use-package restart-emacs)

(use-package projectile
  :init
  (projectile-mode +1))

(use-package prescient)

(use-package ivy-prescient)

(use-package org-superstar)

(use-package ob-async)

(use-package ivy-rich
  :straight
  (ivy-rich
   :type git
   :host github
   :repo "Yevgnen/ivy-rich"))

(use-package counsel-projectile)

(use-package avy)

(use-package all-the-icons)

(use-package dashboard
  :ensure t
  :init
  (setq
   dashboard-set-heading-icons nil
   dashboard-set-file-icons t
   dashboard-startup-banner nil
   dashboard-items '((recents  . 20)
		     (projects . 5)
		     (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package evil-lion
  :ensure t
  :bind (:map evil-normal-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right)
         :map evil-visual-state-map
         ("g l " . evil-lion-left)
         ("g L " . evil-lion-right)))

(use-package eyebrowse)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
