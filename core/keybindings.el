(defconst rubicon-leader-key "SPC")

(defconst rubicon-nvm-states
  '(normal visual motion))

;; (general-create-definer
;;  rubicon-leader
;;   :prefix "<SPC>"
;;  :keymaps 'override
;;   :states rubicon-nvm-states)

(defmacro rubicon-define-leader (prefix)
  (let ((rb-definer-name (intern
			  (concat "rubicon-leader-" prefix))))
    `(general-create-definer
      ,rb-definer-name
      :prefix ,prefix
      :keymaps 'override
      :states rubicon-nvm-states)))

(rubicon-define-leader "SPC")
(rubicon-define-leader "<f14>")

(general-define-key
 :states rubicon-nvm-states
 :keymaps 'override

 "J"  (ilm (evil-next-line 10))
 "K"  (ilm (evil-previous-line 10))
 "L"  (ilm (right-char 10))
 "H"  (ilm (left-char 10))
 "TAB" 'evil-jump-item

 "/" 'swiper
 "e" 'evil-embrace-evil-surround-region
 "z g" 'evil-scroll-line-to-bottom
 "<DEL>" 'execute-extended-command
 "S-<SPC>" 'counsel-locate

 ;; split navigation
 "<down>" 'evil-window-down
 "<left>" 'evil-window-left
 "<up>" 'evil-window-up
 "<right>" 'evil-window-right )

(rubicon-leader-SPC
  "SPC" 'counsel-find-file

  "h o" 'find-function-on-key
  "h f" 'find-function
  "h v" 'find-variable

  "." 'counsel-find-file
  
  "s" 'counsel-switch-buffer
  "g" 'magit-status
  "b" 'ibuffer
  "k" 'kill-other-buffers

  "z"  (ilm (evil-edit "."))
  "<right>" (ilm (my-split-window "right"))
  "<up>" (ilm (my-split-window "up"))
  "<left>" (ilm (my-split-window "left"))
  "<down>" (ilm (my-split-window "down")))

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

  "<f1>" 'org-agenda

  ;; moving windows
  ;; "<f13> <right>" #'+evil/window-move-right
  ;; "<f13> <up>" #'+evil/window-move-up
  ;; "<f13> <left>" #'+evil/window-move-left
  ;; "<f13> <down>" #'+evil/window-move-down
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; dirs/files navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-folder-nmap "<f15> H" ~/                 )
(create-folder-nmap "<f15> P" ~/projects/        )
(create-folder-nmap "<f15> S" ~/scrap/           )
(create-folder-nmap "<f15> R" ~/repos/           )
(create-folder-nmap "<f15> S" ~/repos/scenario/  )
(create-folder-nmap "<f15> T" ~/repos/test-ngp/  )
(create-folder-nmap "<f15> N" ~/repos/norby/     )
(create-folder-nmap "<f15> E" ~/.emacs.d         )
(create-folder-nmap "<f15> D" ~/.doom.d          )
(create-folder-nmap "<f15> O" ~/org              )

(general-nmap
  "<f15> l" (lambda () (interactive) (evil-edit "~/org/timeline.org"))
  "<f15> i" (lambda () (interactive) (evil-edit "~/org/triage.org"))
  "<f15> t" (lambda () (interactive) (evil-edit "~/org/todo.org"))
  "<f15> g" (lambda () (interactive) (evil-edit "~/org/gist.org"))
  "<f15> n" (lambda () (interactive) (evil-edit "~/org/notes.org"))
  "<f15> s" (lambda () (interactive) (evil-edit "~/org/scrap.org"))
  "<f15> r" (lambda () (interactive) (evil-edit "~/.zshrc"))
  "<f15> p" (lambda () (interactive) (evil-edit "~/org/projects.org"))
  "<f15> b" (lambda () (interactive) (evil-edit "~/org/books.org"))
  "<f15> c" (lambda () (interactive) (evil-edit "~/.emacs.d/init.el")))

(global-set-key [remap keyboard-quit] #'doom/escape)
(global-set-key [remap evil-force-normal-state] #'doom/escape)
(global-set-key [escape] #'doom/escape)


;;;###autoload





