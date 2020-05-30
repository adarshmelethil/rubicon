(setq mac-command-modifier 'super
      mac-option-modifier  'meta)

(defconst rubicon/nvm-states
  '(normal visual motion))

(defmacro rubicon/define-leader (prefix)
  (let ((rb-definer-name (intern
			  (concat "rubicon/leader-" prefix))))
    `(general-create-definer
       ,rb-definer-name
       :prefix ,prefix
       :keymaps 'override
       :states rubicon/nvm-states)))

(rubicon/define-leader "SPC")
(rubicon/define-leader "g")
(rubicon/define-leader "<f14>")
(rubicon/define-leader "<f13>")
(rubicon/define-leader "<f15>")
(rubicon/define-leader "M")

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


 "J"  (ilm (evil-next-line 10))
 "K"  (ilm (evil-previous-line 10))
 "L"  (ilm (right-char 10))
 "H"  (ilm (left-char 10))

 "/" 'swiper
 "?" 'swiper-all
 "e" 'evil-embrace-evil-surround-region
 "z g" 'evil-scroll-line-to-bottom
 "<DEL>" 'counsel-M-x
 "S-<SPC>" 'counsel-locate

 ;; split navigation
 "S-<down>" 'evil-scroll-down
 "S-<up>" 'evil-scroll-up

 
 "<down>" 'evil-window-down
 "<left>" 'evil-window-left
 "<up>" 'evil-window-up
 "<right>" 'evil-window-right )

(rubicon/leader-SPC

  "0" (ilm (rubicon/switch-workspace "0"))
  "1" (ilm (rubicon/switch-workspace "1"))
  "2" (ilm (rubicon/switch-workspace "2"))
  "3" (ilm (rubicon/switch-workspace "3"))
  "4" (ilm (rubicon/switch-workspace "4"))
  "5" (ilm (rubicon/switch-workspace "5"))
  "6" (ilm (rubicon/switch-workspace "6"))
  "7" (ilm (rubicon/switch-workspace "7"))
  "8" (ilm (rubicon/switch-workspace "8"))
  "9" (ilm (rubicon/switch-workspace "9"))

  ;; Workspaces
  "TAB d" 'rubicon/delete-workspace
  
  "TAB TAB" 'rubicon/show-workspaces
  "TAB s" 'persp-switch 
  
  ;; Navigation
  "c" 'avy-goto-char
  "l" 'avy-goto-line
  "\"" 'avy-resume

  "L" 'goto-line-preview
  "o" 'rubicon/kill-other-buffers
  "s" 'eshell
  "t" 'vterm
  "r" 'counsel-recentf
  "R" 'ielm
  "e" 'eval-last-sexp
  "E" 'eval-buffer
  "'" 'ivy-resume

  
  "SPC" 'counsel-projectile-find-file-dwim

  ;; Find functions, variables, etc
  "h w" 'what-cursor-position
  "h o" 'find-function-on-key
  "h f" 'find-function
  "h v" 'find-variable

  ;;Describe
  "d v" 'describe-variable
  "d k" 'describe-key
  "d m" 'describe-mode
  "d f" 'describe-face
  "d o" 'describe-font
  "d c" 'describe-char
  "d t" 'describe-theme
  "d a" 'describe-keymap
  "d s" 'describe-symbol
  "d b" 'describe-binding
  "d n" 'describe-fontset

  "." 'counsel-find-file
  
  "," 'persp-counsel-switch-buffer
  
  "g" 'magit-status
  "b" 'ibuffer

  "z"  (ilm (evil-edit "."))
  "<right>" (ilm (rubicon/split-window "right"))
  "<up>" (ilm (rubicon/split-window "up"))
  "<left>" (ilm (rubicon/split-window "left"))
  "<down>" (ilm (rubicon/split-window "down")))

(rubicon/leader-<f14>
  "<f14>" 'counsel-rg
  "h" 'hs-hide-level
  "p" 'proced
  "3" 'swiper-isearch-thing-at-point
  "c" 'org-goto-calendar
  "l" 'magit-log-all
  ;; "<f14> b" 'ibuffer
  "r" 'rename-buffer
  "<f1> c" 'org-schedule

  "<f1>" 'org-agenda)

(rubicon/leader-<f13>
  "<right>" #'+evil/window-move-right
  "<up>" #'+evil/window-move-up
  "<left>" #'+evil/window-move-left
  "<down>" #'+evil/window-move-down)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; dirs/files navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro create-folder-nmap (shortcut file-name)
  `(progn
     (defalias (intern (concat "cd-to-" (symbol-name (quote ,file-name))))
       (lambda ()
	 (interactive)
	 (cd
	  (symbol-name
	   (quote ,file-name)))))
     (general-nmap ,shortcut
       (intern
	(concat "cd-to-"
		(symbol-name (quote ,file-name)))))))


(create-folder-nmap "<f15> H" ~/                 )
(create-folder-nmap "<f15> P" ~/projects/        )
(create-folder-nmap "<f15> S" ~/scrap/           )
(create-folder-nmap "<f15> R" ~/repos/           )
(create-folder-nmap "<f15> S" ~/repos/scrap/     )
(create-folder-nmap "<f15> T" ~/repos/test-ngp/  )
(create-folder-nmap "<f15> N" ~/repos/norby/     )
(create-folder-nmap "<f15> E" ~/.emacs.d         )
(create-folder-nmap "<f15> D" ~/.doom.d          )
(create-folder-nmap "<f15> O" ~/org              )

(rubicon/leader-<f15>
  "l" (ilm (evil-edit "~/org/timeline.org"))
  "i" (ilm (evil-edit "~/org/triage.org"))
  "t" (ilm (evil-edit "~/org/todo.org"))
  "g" (ilm (evil-edit "~/org/gist.org"))
  "n" (ilm (evil-edit "~/org/notes.org"))
  "s" (ilm (evil-edit "~/org/scrap.org"))
  "r" (ilm (evil-edit "~/.zshrc"))
  "p" (ilm (evil-edit "~/org/projects.org"))
  "b" (ilm (evil-edit "~/org/books.org"))
  "C" (ilm (evil-edit "~/.emacs.d/core"))
  "c" (ilm (evil-edit "~/.emacs.d/init.el")))

(global-set-key [remap keyboard-quit] #'rubicon/escape)
(global-set-key [remap evil-force-normal-state] #'rubicon/escape)
(global-set-key [escape] #'rubicon/escape)


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
 "<f15> e" (lambda () (interactive) (vt-insert-command "echo "))
 "<f15> x" 'vt-add-chmod
 "<f15> C" 'vterm-copy-mode-map
 "<f15> s" 'vt-add-sudo
 "<f15> H" (lambda () (interactive) (vt-cd-to "~") (vt-ls))
 "<f15> P" (lambda () (interactive) (vt-cd-to "~/projects") (vt-ls))
 "<f15> e" (lambda () (interactive) (vt-insert-command "echo "))
 "<f15> R" (lambda () (interactive) (vt-cd-to "~/repos") (vt-ls))
 "<f15> O" (lambda () (interactive) (vt-cd-to "~/org") (vt-ls))
 "<f15> B" (lambda () (interactive) (vt-cd-to "~/books") (vt-ls))
 "<f15> S" (lambda () (interactive) (vt-cd-to "~/scrap") (vt-ls)))

(general-define-key
 :states 'normal
 :keymaps 'vterm-mode-map

 "{" (lambda () (interactive) (vt-pusdh "..") (vt-exec "ls"))
 "}" (lambda () (interactive) (vt-popd) (vt-exec "ls"))
 "[" (lambda () (interactive) (vt-pusdh "..") (vt-ls))
 "]" (lambda () (interactive) (vt-popd) (vt-ls))
 "o" (lambda () (interactive) (vt-ls))
 "O" (lambda () (interactive) (vt-exec "ls"))
 "c" (lambda () (interactive) (vt-insert-command "cd "))
 "C" (lambda () (interactive) (vt-insert-command "cat "))

 "x" (lambda () (interactive) (vt-insert-command "rm -rf "))
 "X" (lambda () (interactive) (vt-insert-command "sudo rm -rf "))

 "d" (lambda () (interactive) (vt-insert-command "mkdir "))
 "D" (lambda () (interactive) (vt-insert-command "touch "))

 "<f13> <f13>" 'vterm-send-C-c

 ;; "r" (lambda () (interactive))
 "R" 'vt-source-zshrc)


(general-define-key
 :states 'insert
 :keymaps 'vterm-mode-map

 "`" (lambda () (interactive) (vt-exec "fzf-history-widget"))
 "TAB" 'vterm-send-tab
 "S-<return>" 'vterm-run-and-go-up
 "M-<f13>" 'vt-rc
 "<f13>" 'vterm-send-C-c
 "<f14>" 'vterm-send-escape)

(global-set-key [remap goto-line] 'goto-line-preview)
