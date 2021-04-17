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

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; EVIL
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

(use-package dash
  :config
  (global-evil-mc-mode 1))

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
(use-package evil-snipe
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode 1))

(use-package magit
  :config
  (transient-define-prefix magit-file-dispatch ()
    "Invoke a Magit command that acts on the visited file.
When invoked outside a file-visiting buffer, then fall back
to `magit-dispatch'."
    :info-manual "(magit) Minor Mode for Buffers Visiting Files"
    ["Actions"
     [("s" "Stage"      magit-stage-file)
      ("u" "Unstage"    magit-unstage-file)
      ("c" "Commit"     magit-commit)
      ("e" "Edit line"  magit-edit-line-commit)]
     [("D" "Diff..."    magit-diff)
      ("d" "Diff"       magit-diff-buffer-file)
      ("g" "Status"     magit-status-here)]
     [("L" "Log..."     magit-log)
      ("l" "Log"        magit-log-buffer-file)
      ("t" "Trace"      magit-log-trace-definition)]
     [("B" "Blame..."   magit-blame)
      ("b" "Blame"      magit-blame-addition)
      ("r" "...removal" magit-blame-removal)
      ("f" "...reverse" magit-blame-reverse)
      ("m" "Blame echo" magit-blame-echo)
      ("q" "Quit blame" magit-blame-quit)]
     [("p" "Prev blob"  magit-blob-previous)
      ("n" "Next blob"  magit-blob-next)
      ("v" "Goto blob"  magit-find-file)
      ("V" "Goto file"  magit-blob-visit-file)]
     [(5 "C-c r" "Rename file"   magit-file-rename)
      (5 "C-c d" "Delete file"   magit-file-delete)
      (5 "C-c u" "Untrack file"  magit-file-untrack)
      (5 "C-c c" "Checkout file" magit-file-checkout)]]
    (interactive)
    (transient-setup
     'magit-file-dispatch))

  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))


(use-package restart-emacs)

(use-package prescient
  :config
  (prescient-persist-mode))

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

(use-package which-key
  :config
  (which-key-mode))

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "r" 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

(use-package vterm
  :ensure t)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package company
  :hook ((text-mode prog-mode) . company-mode))

(use-package highlight-parentheses
  :config
  (setq hl-paren-colors (cons "#FFFF00" hl-paren-colors))
  :hook (prog-mode . highlight-parentheses-mode))

(use-package perspective
  :hook (emacs-startup . persp-mode)
  :config
  (setq persp-initial-frame-name "1"))

(use-package docker
  :ensure t)

(use-package esh-autosuggest
  :hook ((eshell-mode . esh-autosuggest-mode)))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.08))

(use-package  replel
  :straight
  (replel
   :type git
   :host github
   :repo "abdulbahajaj/repl.el"))

(use-package treemacs
  :config
  (treemacs-resize-icons 16))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package counsel
  :hook (emacs-startup . ivy-mode)
  :config
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

(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (ivy-rich-mode 1)
  (setcdr
   (assq t ivy-format-functions-alist)
   #'ivy-format-function-line))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode))




(use-package ob-async)

(use-package  highlight-thing
  :hook (prog-mode . highlight-thing-mode)
  :config
  (setq highlight-thing-delay-seconds 0.9))

(use-package treemacs
  :config
  (treemacs-resize-icons 16))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package org-superstar
  :hook ((org-mode . (lambda () (org-superstar-mode 1)))))

(use-package eshell-fringe-status
  :hook ((eshell-mode . eshell-fringe-status-mode)))

(use-package dired-git-info
  ;; :hook ((dired-after-readin . dired-git-info-auto-enable))
  :config
  (setq dgi-auto-hide-details-p nil)
  (define-key dired-mode-map [backtab] 'dired-git-info-mode))

(use-package evil-lion
  :hook (emacs-startup . evil-lion-mode)
  :ensure t)

(use-package lispy
  :hook ((clojure-mode emacs-lisp-mode) . lispy-mode))

(use-package projectile)

(use-package counsel-projectile)

(use-package move-text
  :bind (("M-j" . move-text-down)
	 ("M-k" . move-text-up)))

(use-package evil-numbers
  :bind (("C-c +" . evil-numbers/inc-at-pt)
	 ("C-c -" . evil-numbers/dec-at-pt)))



(use-package clojure-mode)

(use-package flycheck
  :hook (emacs-lisp-mode . flycheck-mode))

(use-package volatile-highlights
  :config
  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
			'evil-paste-pop 'evil-move)
  (vhl/install-extension 'evil))

(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (ivy-rich-mode 1)
  (setcdr
   (assq t ivy-format-functions-alist)
   #'ivy-format-function-line))

(use-package evil-org
  :ensure t
  :after org
  :hook ((org-mode . evil-org-mode)
	 ('evil-org-mode . (lambda ()
			     (evil-org-set-key-theme))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)) 

(use-package org-cliplink)

;; IDE 

(use-package eglot
  :hook ((c-mode c++-mode python-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd")))

(use-package dap-mode
  :config
  (require 'dap-python)

  (setq-default dap-python-debugger 'debugpy)
  (setq-default dap-python-executable "python3")
  (require 'dap-lldb))

(use-package pyvenv)
