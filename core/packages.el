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

(use-package ns-auto-titlebar
  :config
  (ns-auto-titlebar-mode))

;; Installing and configuring packages
(use-package no-littering
  :config
  (require 'no-littering))

(defmacro rubicon/github-package (package-name repo)
  `(use-package  ,package-name
     :straight (,package-name :type git :host github :repo ,repo)))

(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil)
  :config 
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1))

(use-package counsel
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-height 17
	ivy-wrap t
	ivy-magic-slash-non-match-action nil
	ivy-fixed-height-minibuffer t
	projectile-completion-system 'ivy
	ivy-use-virtual-buffers nil
	ivy-virtual-abbreviate 'full
	ivy-on-del-error-function #'ignore
	ivy-use-selectable-prompt t
	ivy-count-format "%d/%d")
  (defun rubicon--ivy-open-dir (x)
    (interactive)
    (dired (or (file-name-directory x)
	       default-directory)))
  (dolist (fn '(counsel-file-jump counsel-projectile-find-file))
    (ivy-add-actions fn '(("a" rubicon--ivy-open-dir "open dir"))))
  (setf (alist-get 't ivy-format-functions-alist)
	#'ivy-format-function-line))

(use-package general
  :config
  (general-evil-setup))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :commands 'evilnc-comment-operator
  :config
  (evilnc-default-hotkeys nil t))

(use-package evil-embrace
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))


;; (use-package libgit)

(use-package tuareg)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package all-the-icons-ivy-rich)

;; (use-package evil-vimish-fold
;;   :hook ((prog-mode . evil-vimish-fold-mode))
;;   :config
;;   (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode)))

(use-package evil-snipe
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode 1))

(use-package undo-tree)

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)))

(use-package restart-emacs)

(use-package projectile
  :init
  (projectile-mode +1))

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode))

(use-package ob-async)

(use-package wgrep)

(use-package counsel-projectile)

(use-package avy)

(use-package evil-indent-plus
  :config
  (evil-indent-plus-default-bindings))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(;; For things that need to be done, just not today.
	  ("TODO" warning bold)
	  ;; For problems that will become bigger problems later if not
	  ;; fixed ASAP.
	  ("FIXME" error bold)
	  ;; For tidbits that are unconventional and not intended uses of the
	  ;; constituent parts, and may break in a future update.
	  ("HACK" font-lock-constant-face bold)
	  ;; For things that were done hastily and/or hasn't been thoroughly
	  ;; tested. It may not even be necessary!
	  ("REVIEW" font-lock-keyword-face bold)
	  ;; For especially important gotchas with a given implementation,
	  ;; directed at another user other than the author.
	  ("NOTE" success bold)
	  ;; For things that just gotta go and will soon be gone.
	  ("DEPRECATED" font-lock-doc-face bold))))

(use-package highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character 
	highlight-indent-guides-responsive t))

(use-package which-key
  :config
  (which-key-mode))

(use-package undo-fu
  :config
  (global-undo-tree-mode -1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "r" 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

(use-package go-mode)
(use-package yaml-mode)

(use-package vterm
  :ensure t)

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode))

(use-package dired-rsync
  :hook (dired-mode . diredfl-mode)
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)
	 (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-margin-mode))

(use-package fd-dired
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired))

(use-package haskell-mode)

(use-package  highlight-thing
  :hook (prog-mode . highlight-thing-mode)
  :config
  (setq highlight-thing-delay-seconds 0.9))

(use-package company
  :hook ((text-mode prog-mode) . company-mode))

(use-package git-timemachine)


(use-package highlight-parentheses
  :config
  (setq hl-paren-colors (cons "#FFFF00" hl-paren-colors))
  :hook (prog-mode . highlight-parentheses-mode))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package perspective
  :after ivy
  :config
  (setq persp-initial-frame-name "1")
  (persp-mode))

(use-package docker
  :ensure t)

(use-package treemacs
  :config
  (treemacs-resize-icons 16))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package esh-autosuggest
  :hook ((eshell-mode . esh-autosuggest-mode)))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.08))

(use-package clojure-mode)

(use-package sass-mode)

(use-package org-superstar
  :hook ((org-mode . (lambda () (org-superstar-mode 1)))))

(use-package evil-org
  :ensure t
  :after org
  :hook ((org-mode . evil-org-mode)
	 ('evil-org-mode . (lambda ()
			     (evil-org-set-key-theme))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package ob-async)
(use-package dockerfile-mode)
(use-package  replel
  :straight
  (replel
   :type git
   :host github
   :repo "abdulbahajaj/repl.el"))

(use-package eshell-fringe-status
  :hook ((eshell-mode . eshell-fringe-status-mode)))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

(setq lsp-keymap-prefix "s-l")
(use-package lsp-mode
  :hook ((java-mode . lsp)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (css-mode . lsp)
	 (html-mode . lsp)
	 (sh-mode . lsp)
	 (json-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package company-lsp
  :config
  (setq company-lsp-enable-snippet t
	company-lsp-cache-candidates t)
  (push 'company-lsp company-backends))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)

(use-package fish-completion
  :config
  (when (executable-find "fish") (global-fish-completion-mode)))


(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/org")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))


(use-package orgit)
(use-package org-tree-slide)
(use-package org-noter)
(use-package org-cliplink)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package solaire-mode
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  :hook (change-major-mode . turn-on-solaire-mode)
  ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
  ;; itself off every time Emacs reverts the file
  :hook (after-revert . turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  :hook (ediff-prepare-buffer . solaire-mode)
  ;; Highlight the minibuffer when it is activated:
  :hook (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  ;; The bright and dark background colors are automatically swapped the first
  ;; time solaire-mode is activated. Namely, the backgrounds of the `default` and
  ;; `solaire-default-face` faces are swapped. This is done because the colors
  ;; are usually the wrong way around. If you don't want this, you can disable it:
  (setq solaire-mode-auto-swap-bg nil)

  (solaire-global-mode +1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  (doom-themes-visual-bell-config)

  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-colors") 
  (doom-themes-treemacs-config)

  (doom-themes-org-config))

(use-package doom-snippets
  :straight
  (doom-snippets
   :type git
   :host github
   :repo "hlissner/doom-snippets")
  :after yasnippet)

(use-package magit-popup
  :after magit)

(use-package
  magit-todos
  :after magit
  :config (magit-todos-mode))

(use-package better-jumper)

(use-package magit-gitflow)


(use-package dired-collapse
  :hook ((dired-mode . dired-collapse-mode)))

(use-package rainbow-identifiers)

(use-package rainbow-mode
  :hook ((prog-mode . rainbow-mode)))

(use-package bufler
  :straight
  (bufler :fetcher github :repo "alphapapa/bufler.el"
                  :files (:defaults (:exclude "helm-bufler.el"))))

(use-package helm-org-ql
  :straight
  (helm-org-ql
   :host github
   :repo "alphapapa/org-ql"
   :files ("helm-org-ql.el")))

(use-package calfw
  :config
  ;; (require 'calfw-org)
  :straight
  (calfw
   :host github
   :repo "kiwanami/emacs-calfw"))

(use-package helm-org-rifle)

;; (use-package org-sticky-header
;;   :config
;;   (setq org-sticky-header-full-path 'full
;; 	org-sticky-header-heading-star ">>>>"
;; 	org-sticky-header-outline-path-separator "/")
;;   (add-hook 'org-mode-hook 'org-sticky-header-mode))

(use-package iedit)

(use-package dired-git-info
  ;; :hook ((dired-after-readin . dired-git-info-auto-enable))
  :config
  (setq dgi-auto-hide-details-p nil)
  (define-key dired-mode-map [backtab] 'dired-git-info-mode))

(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :config (setq deft-directory "~/org"
		deft-extensions '("org" "md" "txt")
		deft-default-extension "org"
		deft-recursive t
		deft-use-filename-as-title nil
		deft-use-filter-string-for-filename t
		deft-file-naming-rules '((nospace . "-"))
		deft-extensions '("md" "org")))

(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode))
(use-package aggressive-indent
  ;; :hook (prog-mode . aggressive-indent-mode)
  :config
  (global-aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'dockerfile-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'makefile-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'makefile-bsdmake-mode)
  (dolist (fn '(undo-fu-only-redo undo-fu-only-undo))
    (add-to-list 'aggressive-indent-protected-commands fn)))

(use-package flycheck
  :hook (emacs-lisp-mode . flycheck-mode))

(use-package pkg-info)
(use-package macrostep)
(use-package volatile-highlights
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
			'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil))

(use-package move-text
  :bind (("M-j" . move-text-down)
	 ("M-k" . move-text-up)))

(use-package evil-numbers
  :bind (("C-c +" . evil-numbers/inc-at-pt)
	 ("C-c -" . evil-numbers/dec-at-pt)))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))


(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package lispy
  :hook ((clojure-mode emacs-lisp-mode) . lispy-mode))

(use-package highlight-quoted
  :hook ((clojure-mode emacs-lisp-mode) . highlight-quoted-mode))

(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (ivy-rich-mode 1)
  (all-the-icons-ivy-rich-mode)
  (setcdr
   (assq t ivy-format-functions-alist)
   #'ivy-format-function-line))

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode)))

(use-package ctrlf
  :config
  (ctrlf-mode 1))

(use-package magit-delta
  :hook ((magit-mode . magit-delta-mode)))

(provide 'packages)
