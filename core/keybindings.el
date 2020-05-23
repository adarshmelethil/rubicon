(global-set-key (kbd "<f1>") nil)
(global-set-key (kbd "<f12>") nil)
(global-set-key (kbd "<f13>") nil)
(global-set-key (kbd "<f14>") nil)

;; (defconst rubicon-leader-key "SPC")

;;(general-create-definer rubicon-leader
;;  :prefix rubicon-leader-key)

;; (general-define-key
;;  :states nil
;;  :wk-full-keys nil
;;  :keymaps rubicon-leader-map)


(general-nmap
  "e" 'evil-embrace-evil-surround-region
  "z g" 'evil-scroll-line-to-bottom
  "r"  'undo-fu-only-redo
  "-"  'counsel-M-x

  ;; In Buffer movement
  "J"  (lambda () (interactive) (evil-next-line 10))
  "K"  (lambda () (interactive) (evil-previous-line 10))
  "L"  (lambda () (interactive) (right-char 10))
  "H"  (lambda () (interactive) (left-char 10))
  "l"  'right-char
  "h"  'left-char

  ;; other
  "TAB" 'evil-jump-item
  "M-b" 'evil-buffer-new
  "<f14> t" '+vterm/here
  "<f14> T" 'evil-collection-vterm-toggle-send-escape
  "<f14> s" 'save-buffer
  "<f14> d" '+doom-dashboard/open
  "<SPC> z" (lambda () (interactive) (evil-edit "."))
  "<f14> h" 'hs-hide-level
  "<f14> <f14>" '+default/search-project
  "<f14> P" 'proced
  "<f14> p" 'helm-top
  "<f14> 3" 'swiper-isearch-thing-at-point
  "<f14> f" 'helm-google-suggest
  "<f14> F" '+lookup/online-select
  "<f14> c" 'org-goto-calendar
  "<f14> C" '=calendar
  "<f14> M-c" 'org-date-from-calendar
  "<f14> l" 'magit-log-all
  "<f14> b" 'ibuffer
  "<f14> e" '+eshell/here
  "<f14> m" 'my/make-run
  "<f14> r" 'rename-buffer
  ;; "<f14> z" 'my-open-neotree-at-root
  "<f14> z" '+neotree/open
  "<f14> <f1> c" 'org-schedule
  "S-<SPC> <SPC>" 'counsel-locate
  ;; "<f1>" 'org-agenda
  "<f12>" 'switch-window

  ;; Resizing
  "+" 'evil-window-increase-width
  "_" 'evil-window-decrease-width
  "M-=" 'evil-window-increase-height
  "M--" 'evil-window-decrease-height

  ;; split navigation
  "<down>" 'evil-window-down
  "<left>" 'evil-window-left
  "<up>" 'evil-window-up
  "<right>" 'evil-window-right

  ;; moving windows
  "<f13> <right>" #'+evil/window-move-right
  "<f13> <up>" #'+evil/window-move-up
  "<f13> <left>" #'+evil/window-move-left
  "<f13> <down>" #'+evil/window-move-down

  ;; split creation
  "<SPC> <right>" (lambda () (interactive) (my-split-window "right"))
  "<SPC> <up>" (lambda () (interactive) (my-split-window "up"))
  "<SPC> <left>" (lambda () (interactive) (my-split-window "left"))
  "<SPC> <down>" (lambda () (interactive) (my-split-window "down")))



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
