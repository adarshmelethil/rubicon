;;; -*- lexical-binding: t; -*-

(general-nmap
  "TAB" 'evil-jump-item
  "u" 'undo-fu-only-undo
  "r" 'undo-fu-only-redo
  "g c" 'evilnc-comment-operator)

(general-define-key
 :states rubicon/nvm-states
 :keymaps 'override

 "+" 'evil-window-increase-width
 "_" 'evil-window-decrease-width
 "M-=" 'evil-window-increase-height
 "M--" 'evil-window-decrease-height

 "L"  (ilm (right-char 10))
 "H"  (ilm (left-char 10))

 "*" 'evil-embrace-evil-surround-region
 "z g" 'evil-scroll-line-to-bottom
 ;; "<DEL>" 'counsel-M-x
 ;; [backspace] 'counsel-M-x

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
    "<ESC>" #'delete-window

    ;; Workspaces
    "TAB d" 'rubicon/workspace-delete
    "TAB TAB" 'rubicon/workspace-show-all
    "TAB s" 'persp-switch 
    "o" 'rubicon/workspace-kill-invisible-buffers
    "O" 'rubicon/workspace-kill-other-buffers

    ,@(-flatten-n 1 (--map (let ((str-num (number-to-string it)))
			     (list str-num `(ilm (rubicon/workspace-switch ,str-num))))
			   (number-sequence 1 9)))
    
    "n" #'replel-start-repl
    "N" #'replel-overview
    "D" #'docker
    "s" #'save-buffer
    "t" 'rubicon/eshell-here

    "T" #'vterm
    "R" #'ielm
    "e" #'eval-last-sexp
    "E" #'eval-buffer
    "b" #'evil-buffer-new
    "y" 'rubicon/copy-path-to-buffer-file
    
    "." 'counsel-find-file
    "," 'persp-counsel-switch-buffer

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
    
    "g" #'magit-file-dispatch
    "G" #'magit-dispatch
    "l" #'magit-log-current
    
    ;; SMERGE
    "k u" #'smerge-keep-upper
    "k l" #'smerge-keep-lower
    "k a" #'smerge-keep-all
    "k n" #'smerge-next
    "k p" #'smerge-prev
    "k m" #'smerge-keep-mine

    "P" #'proced

    "SPC" '+ivy/projectile-find-file
    "'" 'ivy-resume

    "d" 'rubicon/create-disposable-dir

    "z"  (ilm (evil-edit "."))
    "Z" 'treemacs-select-window
    "<right>" (ilm (rubicon/split-window "right"))
    "<up>" (ilm (rubicon/split-window "up"))
    "<left>" (ilm (rubicon/split-window "left"))
    "<down>" (ilm (rubicon/split-window "down"))

    ;; Debugger 
    "TAB r" 'dap-debug
    "TAB n" 'dap-next
    "TAB i" 'dap-step-in
    "TAB o" 'dap-step-out
    "TAB c" 'dap-continue
    "TAB l" 'dap-ui-locals
    "TAB R" 'dap-ui-repl
    "TAB B" 'dap-ui-breakpoints-list
    "TAB b" 'dap-breakpoint-toggle))


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
 "<f15> e" (ilm (vt-insert-command "echo "))
 "<f15> x" 'vt-add-chmod
 "<f15> C" 'vterm-copy-mode-map
 "<f15> s" 'vt-add-sudo
 "<f15> H" (ilm (vt-cd-to "~") (vt-ls))
 "<f15> P" (ilm (vt-cd-to "~/projects") (vt-ls))
 "<f15> e" (ilm (vt-insert-command "echo "))
 "<f15> R" (ilm (vt-cd-to "~/repos") (vt-ls))
 "<f15> O" (ilm (vt-cd-to "~/org") (vt-ls))
 "<f15> B" (ilm (vt-cd-to "~/notebooks") (vt-ls))
 "<f15> S" (ilm (vt-cd-to "~/scrap") (vt-ls)))

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

(rubicon/create-fs-map "Q" "~/repos")
(rubicon/create-fs-map "H" "~/" )
(rubicon/create-fs-map "P" "~/projects/" )
(rubicon/create-fs-map "S" "~/scrap/" )
(rubicon/create-fs-map "R" "~/repos/" )
(rubicon/create-fs-map "E" "~/.emacs.d" )
(rubicon/create-fs-map "D" "~/.doom.d" )
(rubicon/create-fs-map "O" "~/org" )
(rubicon/create-fs-map "e k" "~/.emacs.d/core/keybindings.el")
(rubicon/create-fs-map "e C" "~/.emacs.d/core/core.el")
(rubicon/create-fs-map "e c" "~/.emacs.d/core/config.el")
(rubicon/create-fs-map "e p" "~/.emacs.d/core/packages.el")
(rubicon/create-fs-map "e i" "~/.emacs.d/init.el")
(rubicon/create-fs-map "e e" "~/.emacs.d/early-init.el")
(rubicon/create-fs-map "e l" "~/.emacs.d/local.el")
(rubicon/create-fs-map "o" "~/org/org.org")
(rubicon/create-fs-map "g" "~/org/gist.org")
(rubicon/create-fs-map "n" "~/org/notes.org")
(rubicon/create-fs-map "s" "~/org/scrap.org")
(rubicon/create-fs-map "r" "~/.config/fish/config.fish")
(rubicon/create-fs-map "b" "~/org/notebooks.org")
(rubicon/create-fs-map "C" "~/.emacs.d/core")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :keymaps 'dired-mode-map
 "[" 'dired-create-directory)

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
