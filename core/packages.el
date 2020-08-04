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
(use-package no-littering
  :config
  (require 'no-littering))


(defmacro rubicon/github-package (package-name repo)
  `(use-package  ,package-name
     :straight (,package-name :type git :host github :repo ,repo)))

(use-package dash)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config 
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search))

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
  (setf (alist-get 't ivy-format-functions-alist)
	#'ivy-format-function-line))

(use-package doom-themes
  :config
  (load-theme 'doom-dark+))

(use-package general
  :config
  ;;(require 'general)
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

(use-package smartparens
  :config
  (smartparens-global-mode))

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

(use-package org-superstar)

(use-package ob-async)

(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (ivy-rich-mode 1)
  (setcdr
   (assq t ivy-format-functions-alist)
   #'ivy-format-function-line))

(use-package wgrep)

(use-package counsel-projectile)

(use-package avy)

(use-package all-the-icons)

;; (use-package evil-lion
;;   :ensure t
;;   :bind (:map evil-normal-state-map
;; 	      ("g l " . evil-lion-left)
;; 	      ("g L " . evil-lion-right)
;; 	      :map evil-visual-state-map
;; 	      ("g l " . evil-lion-left)
;; 	      ("g L " . evil-lion-right)))

;; (use-package eyebrowse
;;   :config
;;   (eyebrowse-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config)

(use-package  evil-indent-plus
  :config
  (evil-indent-plus-default-bindings)
  :straight
  (evil-indent-plus
   :type git
   :host github
   :repo "TheBB/evil-indent-plus"))




(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (global-hl-todo-mode)
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
  (setq highlight-indent-guides-method 'fill))

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


;; (use-package lsp-mode
;;   :config
;;   (lsp-mode)
;;   )


;; (use-package lsp-ui
;;   :config
;;   (setq lsp-ui-doc-max-height 8
;; 	lsp-ui-doc-max-width 35
;; 	lsp-ui-sideline-ignore-duplicate t
;; 	;; lsp-ui-doc is redundant with and more invasive than
;; 	;; `+lookup/documentation'
;; 	lsp-ui-doc-enable nil
;; 	;; Don't show symbol definitions in the sideline. They are pretty noisy,
;; 	;; and there is a bug preventing Flycheck errors from being shown (the
;; 	;; errors flash briefly and then disappear).
;; 	lsp-ui-sideline-show-hover nil))

;; (use-package lsp-ivy
;;   :after lsp-mode)

(use-package go-mode)
(use-package yaml-mode)

(use-package vterm
  :ensure t)

;; (use-package all-the-icons-ivy
;;   :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package dired-rsync
  :hook (dired-mode . diredfl-mode)
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package diff-hl
  :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; use margin instead of fringe
  (diff-hl-margin-mode))

;; (use-package all-the-icons-dired
;;   :config
;;   (defvar +wdired-icons-enabled -1))

;; (use-package dired-x
;;   :hook (dired-mode . dired-omit-mode)
;;   :config
;;     (setq dired-omit-verbose nil
;; 	  dired-clean-confirm-killing-deleted-buffers nil))


(use-package fd-dired
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired))

;; (use-package multiple-cursors)

;; (rubicon/github-package doom-snippets "hlissner/doom-snippets")

;; (use-package flycheck
;;   :config
;;   (flycheck-mode)
;;   (setq flycheck-emacs-lisp-load-path 'inherit)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))
;;   (setq flycheck-buffer-switch-check-intermediate-buffers t)
;;   (setq flycheck-display-errors-delay 0.25))

;; (use-package lsp-java
;;   :config
;;   (setq lsp-jt-root (concat lsp-java-server-install-dir "java-test/server/")
;;           dap-java-test-runner (concat lsp-java-server-install-dir "test-runner/junit-platform-console-standalone.jar")))

(use-package haskell-mode)

(rubicon/github-package highlight-thing "fgeller/highlight-thing.el")

(use-package  highlight-thing
  :hook (prog-mode . highlight-thing-mode)
  :straight
  (highlight-thing
   :type git
   :host github
   :repo "fgeller/highlight-thing.el")
  :config
  (setq highlight-thing-delay-seconds 0.9))

;; (rubicon/github-package goto-line-preview "jcs-elpa/goto-line-preview")

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package git-timemachine)

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

(use-package forge
  :after magit)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; (use-package rainbow-mode
;;   :ensure t
;;   :init (rainbow-mode 1))

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

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package esh-autosuggest
  :config
  (add-hook 'eshell-mode-hook 'esh-autosuggest-mode))

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
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package evil-org
  :straight
  (evil-org
   :type git
   :host github
   :repo "hlissner/evil-org-mode")

  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
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

(use-package eshell-z)
(use-package eshell-prompt-extras)
(use-package esh-help)
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-fringe-status
  :config
  (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

(setq lsp-keymap-prefix "s-l")
(use-package lsp-mode
    :hook ((python-mode . lsp)
            (java-mode . lsp)
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
