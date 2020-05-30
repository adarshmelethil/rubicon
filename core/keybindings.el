(setq mac-command-modifier 'super
      mac-option-modifier  'meta)

(defconst rubicon-nvm-states
  '(normal visual motion))

(defmacro rubicon-define-leader (prefix)
  (let ((rb-definer-name (intern
			  (concat "rubicon-leader-" prefix))))
    `(general-create-definer
       ,rb-definer-name
       :prefix ,prefix
       :keymaps 'override
       :states rubicon-nvm-states)))

(rubicon-define-leader "SPC")
(rubicon-define-leader "g")
(rubicon-define-leader "<f14>")
(rubicon-define-leader "<f13>")
(rubicon-define-leader "<f15>")
(rubicon-define-leader "M")

(general-define-key
 :states 'insert
 :keymaps 'org-mode-map
 "<tab>" 'completion-at-point)

(general-define-key
 :states rubicon-nvm-states
 :keymaps 'org-mode-map
 "<return>" '+org/dwim-at-point)


(general-nmap
  "u" 'undo-fu-only-undo
  "r" 'undo-fu-only-redo
  "g c" 'evilnc-comment-operator)


(general-define-key
 :states rubicon-nvm-states
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
 "TAB" 'evil-jump-item

 "/" 'swiper
 "?" 'swiper-all
 "e" 'evil-embrace-evil-surround-region
 "z g" 'evil-scroll-line-to-bottom
 "<DEL>" 'counsel-M-x
 "S-<SPC>" 'counsel-locate

 ;; split navigation
 "<down>" 'evil-window-down
 "<left>" 'evil-window-left
 "<up>" 'evil-window-up
 "<right>" 'evil-window-right )

(rubicon-leader-SPC
  "o" 'rubicon/kill-other-buffers
  "s" 'eshell
  "t" 'vterm
  "r" 'counsel-recentf
  "R" 'ielm
  "e" 'eval-last-sexp
  "E" 'eval-buffer
  "'" 'ivy-resume

  "w s" 'persp-switch 
  "w c" 'persp-add-new
  
  "n" 'eyebrowse-create-window-config
  "0" 'eyebrowse-switch-to-window-config-10
  "1" 'eyebrowse-switch-to-window-config-1
  "2" 'eyebrowse-switch-to-window-config-2
  "3" 'eyebrowse-switch-to-window-config-3
  "4" 'eyebrowse-switch-to-window-config-4
  "5" 'eyebrowse-switch-to-window-config-5
  "6" 'eyebrowse-switch-to-window-config-6
  "7" 'eyebrowse-switch-to-window-config-7
  "8" 'eyebrowse-switch-to-window-config-8
  "9" 'eyebrowse-switch-to-window-config-9
  
  "SPC" 'counsel-projectile-find-file-dwim

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
  
  "," 'counsel-switch-buffer
  "g" 'magit-status
  "b" 'ibuffer

  "z"  (ilm (evil-edit "."))
  "<right>" (ilm (rubicon/split-window "right"))
  "<up>" (ilm (rubicon/split-window "up"))
  "<left>" (ilm (rubicon/split-window "left"))
  "<down>" (ilm (rubicon/split-window "down")))

(rubicon-leader-<f14>
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

(rubicon-leader-<f13>
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

(rubicon-leader-<f15>
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
