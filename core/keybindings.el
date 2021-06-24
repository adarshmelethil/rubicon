;;; -*- lexical-binding: t; -*-

(general-nmap
  "TAB" 'evil-jump-item
  "u" 'undo-fu-only-undo
  "r" 'undo-fu-only-redo
  "g c" 'evilnc-comment-operator)

(general-define-key
 :states rubicon/nvm-states
 :keymaps 'override

 "#" 'swiper-isearch-thing-at-point

 "+" 'evil-window-increase-width
 "_" 'evil-window-decrease-width
 "M-=" 'evil-window-increase-height
 "M--" 'evil-window-decrease-height

 "L"  (ilm (right-char 10))
 "H"  (ilm (left-char 10))

 "/" 'swiper
 "?" 'swiper-all
 "*" 'evil-embrace-evil-surround-region
 "z g" 'evil-scroll-line-to-bottom
 "<DEL>" 'counsel-M-x
 "M-x" 'counsel-M-x
 [backspace] 'counsel-M-x

 "S-<SPC>"    'counsel-rg
 "s-<SPC>"    'counsel-locate

 ;; navigation
 "("          'evil-scroll-down
 ")"          'evil-scroll-up

 ;; split
 "<down>"     'evil-window-down
 "<left>"     'evil-window-left
 "<up>"       'evil-window-up
 "<right>"    'evil-window-right)

(eval
 `(rubicon/leader-SPC
    "w d" #'delete-window
    "w c" #'delete-window

    "q q" 'restart-emacs

    ;; Workspaces
    "TAB d" 'rubicon/workspace-delete
    "TAB TAB" 'rubicon/workspace-show-all
    "TAB ." 'persp-switch 
    "TAB `" 'persp-switch-last
    "TAB n" 'rubicon/workspace-new
    "TAB r" 'persp-rename

    ;; TODO: persp-switch-by-number
    ,@(-flatten-n 1 (--map (let ((str-num (number-to-string it)))
			     (list (concat "TAB " str-num) `(ilm (rubicon/workspace-switch ,str-num))))
			   (number-sequence 1 9)))

    
    "n" #'replel-start-repl
    "N" #'replel-overview
    "D" #'docker
    "T" #'vterm
    "R" #'ielm

    ;; File
    "f r" #'counsel-recentf

    ;; Buffer
    "b n" #'evil-buffer-new
    "b s" #'save-buffer
    "b b" 'persp-counsel-switch-buffer
    "b r" #'dashboard-refresh-buffer
    "b e" 'rubicon/eshell-here
    "b k" 'rubicon/workspace-kill-invisible-buffers
    "b K" 'rubicon/workspace-kill-other-buffers

    ;; TODO: what is this?
    "r r" #'browse-at-remote
    
    "." 'counsel-find-file

    ;; yanking
    "y r" 'browse-kill-ring
    ;; "y 1" (ilm (yank 2))
    "y f" 'rubicon/copy-path-to-buffer-file
    "Y g" 'git-link-homepage
    "Y y" 'rubicon/copy-git-link-at-point
    
    "h y" 'yas-describe-tables
    "h w" #'what-cursor-position
    "h o" #'find-function-on-key
    "h f" #'find-function
    "h v" #'find-variable
    "h r" #'describe-variable
    "h k" #'describe-key
    "h m" #'describe-mode
    "h e" #'describe-face
    "h n" #'describe-font
    "h c" #'describe-char
    "h t" #'describe-theme
    "h a" #'describe-keymap
    "h s" #'describe-symbol
    "h b" #'describe-binding
    "h F" #'describe-fontset
    "a a" #'org-agenda-list
    "a t" #'org-todo-list
    "c" #'org-capture
    "C" #'comint-run
    
    "g" #'magit-file-dispatch
    "G" #'magit-dispatch
    "l" #'magit-log-current

    ;; Projectile
    "p a" 'projectile-add-known-project
    "p p" 'rubicon/projectile-switch-project
    "p SPC" '+ivy/projectile-find-file
    "p r" 'projectile-replace
    "p s" 'projectile-save-project-buffers
    "p d" 'projectile-dired
    "p k" 'projectile-kill-buffers
    
    ;; SMERGE
    "k u" #'smerge-keep-upper
    "k l" #'smerge-keep-lower
    "k a" #'smerge-keep-all
    "k n" #'smerge-next
    "k p" #'smerge-prev
    "k m" #'smerge-keep-mine

    "P" #'proced

    "'" 'ivy-resume

    ;; Scrape
    "s d" 'rubicon/create-disposable-dir

    ;; Narrow and widen
    "w r" #'narrow-to-region
    "w p" #'narrow-to-page
    "w w" #'widen
    "w f" #'narrow-to-defun
    "w b" #'org-narrow-to-block
    "w s" #'org-narrow-to-subtree
    "w e" #'org-narrow-to-element

    "z"  (ilm (evil-edit "."))
    "Z" #'treemacs-select-window
    "<right>" (ilm (rubicon/split-window "right"))
    "<up>" (ilm (rubicon/split-window "up"))
    "<left>" (ilm (rubicon/split-window "left"))
    "<down>" (ilm (rubicon/split-window "down"))

    ;; surround 
    "<f1> e" #'evil-embrace-evil-surround-change
    "<f1> d" #'evil-embrace-evil-surround-delete
    "<f1> r" #'evil-embrace-evil-surround-region
    

    "i" #'rubicon/edit-last-kill

    ;; Debugger 
    ;; TODO: abdul - how to use this
    "d r" #'dap-debug
    "d n" #'dap-next
    "d t" #'dap-disconnect
    "d i" #'dap-step-in
    "d o" #'dap-step-out
    "d c" #'dap-continue
    "d l" #'dap-ui-locals
    "d R" #'dap-ui-repl
    "d B" #'dap-ui-breakpoints-list
    "d b" #'dap-breakpoint-toggle))


(rubicon/leader-<f13>
  "<right>" #'+evil/window-move-right
  "<up>" #'+evil/window-move-up
  "<left>" #'+evil/window-move-left
  "<down>" #'+evil/window-move-down)

(general-define-key
 :keymaps 'org-mode-map
 :prefix "<f14>"
 "a" #'org-archive-subtree-default
 "t" #'counsel-org-tag
 "r" #'org-refile
 "i" #'org-date-from-calendar
 "l" #'org-cliplink
 "L" #'org-toggle-link-display
 "d" #'org-deadline
 "s" #'org-schedule
 "c" #'org-goto-calendar
 "C" #'count-words
 "h" #'org-insert-heading-respect-content)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "<return>" '+org/dwim-at-point)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(general-define-key
 :keymaps 'magit-mode-map
 :prefix "<f14>"
 "a" #'magit-commit-amend
 "s" #'magit-stash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vterm 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :states '(normal)
 :keymaps 'vterm-copy-mode-map
 "a" 'my-vterm-append
 "i" 'my-vterm-insert)

(general-define-key
 :states '(normal insert)
 :keymaps 'vterm-mode-map
 "<f1>" 'my-vterm-clear
 "<f14> e" (ilm (vt-insert-command "echo "))
 "<f14> x" 'vt-add-chmod
 "<f14> C" 'vterm-copy-mode-map
 "<f14> s" 'vt-add-sudo
 "<f14> H" (ilm (vt-cd-to "~") (vt-ls))
 "<f14> P" (ilm (vt-cd-to "~/projects") (vt-ls))
 "<f14> e" (ilm (vt-insert-command "echo "))
 "<f14> R" (ilm (vt-cd-to "~/repos") (vt-ls))
 "<f14> O" (ilm (vt-cd-to "~/org") (vt-ls))
 "<f14> B" (ilm (vt-cd-to "~/notebooks") (vt-ls))
 "<f14> S" (ilm (vt-cd-to "~/scrap") (vt-ls)))

(general-define-key
 :states 'normal
 :keymaps 'vterm-mode-map

 "{" (ilm (vt-pusdh "..") (vt-exec "ls"))
 "}" (ilm (vt-popd) (vt-exec "ls"))
 "[" (ilm (vt-pusdh "..") (vt-ls))
 "]" (ilm (vt-popd) (vt-ls))
 "o" (ilm (vt-ls))
 "O" (ilm (vt-exec "ls"))
 "c" (ilm (vt-insert-command "cd "))
 "C" (ilm (vt-insert-command "cat "))

 "x" (ilm (vt-insert-command "rm -rf "))
 "X" (ilm (vt-insert-command "sudo rm -rf "))

 "d" (ilm (vt-insert-command "mkdir "))
 "D" (ilm (vt-insert-command "touch "))

 "<f13> <f13>" 'vterm-send-C-c

 "R" 'vt-source-zshrc)


(general-define-key
 :states 'insert
 :keymaps 'vterm-mode-map

 "TAB" 'vterm-send-tab
 "S-<return>" 'vterm-run-and-go-up
 "M-<f13>" 'vt-rc
 "<f13>" 'vterm-send-C-c
 "<f14>" 'vterm-send-escape)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eshell 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :keymaps 'eshell-mode-map
 :states 'insert
 "<f1>" (ilm (eshell/clear-scrollback)
	     (eshell-send-input))
 "<f13>" #'evil-collection-eshell-interrupt-process)



(global-set-key [remap keyboard-quit] #'rubicon/escape)
(global-set-key [remap evil-force-normal-state] #'rubicon/escape)
(global-set-key [escape] #'rubicon/escape)

(general-define-key
 ;; :states 'normal
 :keymaps 'replel-mode-map
 "<f1>" 'replel-run)

(global-set-key [remap kill-current-buffer]
		'rubicon/workspace-kill-current-buffer)
(global-set-key [remap quit-window]
		'rubicon/workspace-quit-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; dirs/files navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; f15 maping
(rubicon/create-fs-map "Q" "~/repos")
(rubicon/create-fs-map "H" "~/" )
(rubicon/create-fs-map "P" "~/projects/" )
(rubicon/create-fs-map "S" "~/scrap/" )
(rubicon/create-fs-map "R" "~/repos/" )
(rubicon/create-fs-map "E" "~/.emacs.d" )
(rubicon/create-fs-map "e k" "~/.emacs.d/core/keybindings.el")
(rubicon/create-fs-map "e C" "~/.emacs.d/core/core.el")
(rubicon/create-fs-map "e c" "~/.emacs.d/core/config.el")
(rubicon/create-fs-map "e p" "~/.emacs.d/core/packages.el")
(rubicon/create-fs-map "e i" "~/.emacs.d/init.el")
(rubicon/create-fs-map "e e" "~/.emacs.d/early-init.el")
(rubicon/create-fs-map "e l" "~/.emacs.d/local.el")
(rubicon/create-fs-map "O" rubicon/org-dir-path)
(rubicon/create-fs-map "o" (format "%s/org.org" rubicon/org-dir-path))
(rubicon/create-fs-map "g" (format "%s/gist.org" rubicon/org-dir-path))
(rubicon/create-fs-map "n" (format "%s/notes.org" rubicon/org-dir-path))
(rubicon/create-fs-map "s" (format "%s/scrap.org" rubicon/org-dir-path))
(rubicon/create-fs-map "t" (format "%s/thoughts.org" rubicon/org-dir-path))
(rubicon/create-fs-map "b" (format "%s/notebooks.org" rubicon/org-dir-path))
(rubicon/create-fs-map "r" "~/.config/fish/config.fish")
(rubicon/create-fs-map "C" "~/.emacs.d/core")
(rubicon/create-fs-map "W" "~/work")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :keymaps 'clojure-mode-map
 :prefix "<f14>"
 "j" 'cider-jack-in


 "d d" 'cider-hydra-doc/body
 "d r" 'cider-hydra-repl/body
 "d e" 'cider-hydra-eval/body
 "d t" 'cider-hydra-test/body

 "t" 'cider-toggle-trace-var
 "T" 'cider-toggle-trace-ns

 "e" 'cider-eval-last-sexp
 "E" 'cider-eval-buffer

 "i" 'cider-debug-defun-at-point

 "r" 'cider-test-run-test
 
 "n" 'cider-ns-reload-all
 "R" 'hydra-cljr-help-menu/body

 "h" 'cider-debug-move-here)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :keymaps 'python-mode-map
 :prefix "<f14>"
 "v" #'pyvenv-activate
 "s" 'py-isort
 "i" 'pyimport-insert-missing
 "r" 'pyimport-remove-unused)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :keymaps 'dired-mode-map
 :prefix "<f14>"
 "c" 'dired-create-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :keymaps 'org-mode-map
 :prefix "<f14>"
 "w" #'writeroom-mode
 "f" #'org-roam-insert
 "a" #'org-archive-subtree-default
 "t" #'counsel-org-tag
 "r" #'org-refile
 "i" #'org-date-from-calendar
 "l" #'org-cliplink
 "L" #'org-toggle-link-display
 "d" #'org-deadline
 "s" #'org-schedule
 "c" #'org-goto-calendar
 "C" #'count-words
 "h" #'org-insert-heading-respect-content)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "<return>" '+org/dwim-at-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emacs lisp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :keymaps 'emacs-lisp-mode-map
 :prefix "<f14>"
 "e" #'eval-last-sexp
 "E" #'eval-buffer)

(provide 'keybindings)
;;; keybindings.el ends here
