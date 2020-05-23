(setq gc-cons-threshold most-positive-fixnum)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(load (concat user-emacs-directory "core/packages"))
(load (concat user-emacs-directory "core/core"))
(load (concat user-emacs-directory "core/keybindings"))



;; (setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-threshold 16777216)

;; (add-to-list 'load-path 
;; 	(concat user-emacs-directory "core"))



;; 
;; (tool-bar-mode -1)
;; 
;; (setq display-line-numbers-type 'relative)
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'conf-mode-hook #'display-line-numbers-mode)
;; (add-hook 'text-mode-hook #'display-line-numbers-mode)
;; 
;; ;; Packages initializing
;; (require 'package)
;; 
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)
;; 
;; ;;;;;;;;;;;;;;;;
;; ;;; Packages
;; ;;;;;;;;;;;;;;;;
;; ;; (load-theme 'wombat)
;; 
;; 
;; ;;(package-refresh-contents)
;; 
;; ;; Download Evil
;; (unless (package-installed-p 'evil)
;;   (package-install 'evil))
;; 
;; (unless (package-installed-p 'counsel)
;;   (package-install 'counsel))
;; 
;; (unless (package-installed-p 'doom-themes)
;;   (package-install 'doom-themes))
;; 
;; (unless (package-installed-p 'general)
;;   (package-install 'general))
;; 
;; (unless (package-installed-p 'vterm)
;;   (package-install 'vterm))
;; 
;; (require 'general)
;; (general-evil-setup)
;; 
;; ;; Enable Evil
;; (require 'evil)
;; (evil-mode 1)
;; (ivy-mode 1)
;; 
;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "%d/%d")
;; 
;; (add-hook 'org-mode-hook 'org-indent-mode)
;; 
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    (quote
;;     ("e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" default)))
;;  '(horizontal-scroll-bar-mode nil)
;;  '(package-selected-packages (quote (vterm doom-themes general counsel evil))))
;; 
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;; 
;; (load-theme 'doom-tomorrow-night)
;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;; Genearal binding
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-set-key (kbd "<f1>") nil)
;; (global-set-key (kbd "<f12>") nil)
;; (global-set-key (kbd "<f13>") nil)
;; (global-set-key (kbd "<f14>") nil)
;; (global-set-key (kbd "<f15>") nil)
;; 
;;  (setq visible-bell -1)
;; (setq ring-bell-function 'ignore)
;; 
;; ;;(global-set-key (kbd "SPC") nil)
;; (scroll-bar-mode -1)
;; (customize-set-variable 'horizontal-scroll-bar-mode nil)
;; 
;; (general-nmap
;;   "e" 'evil-embrace-evil-surround-region
;;   "z g" 'evil-scroll-line-to-bottom
;;   "r"  'undo-fu-only-redo
;;   "-"  'counsel-M-x
;; 
;;   ;; In Buffer movement
;;   "J"  (lambda () (interactive) (evil-next-line 10))
;;   "K"  (lambda () (interactive) (evil-previous-line 10))
;;   "L"  (lambda () (interactive) (right-char 10))
;;   "H"  (lambda () (interactive) (left-char 10))
;;   "l"  'right-char
;;   "h"  'left-char
;; 
;;   ;; tabs
;;   ;; "<SPC> 1" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 1))
;;   ;; "<SPC> 2" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 2))
;;   ;; "<SPC> 3" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 3))
;;   ;; "<SPC> 4" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 4))
;;   ;; "<SPC> 5" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 5))
;;   ;; "<SPC> 6" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 6))
;;   ;; "<SPC> 7" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 7))
;;   ;; "<SPC> 8" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 8))
;;   ;; "<SPC> 9" (lambda () (interactive) (centaur-tabs-select-visible-nth-tab 9))
;;   ;; "<f14> g" 'centaur-tabs-toggle-groups
;;   ;; "<f13> t" 'centaur-tabs-mode
;; 
;;   ;; other
;;   "TAB" 'evil-jump-item
;;   "M-b" 'evil-buffer-new
;;   "<f14> t" '+vterm/here
;;   "<f14> T" 'evil-collection-vterm-toggle-send-escape
;;   "<f14> s" 'save-buffer
;;   "<f14> d" '+doom-dashboard/open
;;   "<SPC> z" (lambda () (interactive) (evil-edit "."))
;;   "<f14> h" 'hs-hide-level
;;   "<f14> <f14>" '+default/search-project
;;   "<f14> P" 'proced
;;   "<f14> p" 'helm-top
;;   "<f14> 3" 'swiper-isearch-thing-at-point
;;   "<f14> f" 'helm-google-suggest
;;   "<f14> F" '+lookup/online-select
;;   "<f14> c" 'org-goto-calendar
;;   "<f14> C" '=calendar
;;   "<f14> M-c" 'org-date-from-calendar
;;   "<f14> l" 'magit-log-all
;;   "<f14> b" 'ibuffer
;;   "<f14> e" '+eshell/here
;;   "<f14> m" 'my/make-run
;;   "<f14> r" 'rename-buffer
;;   ;; "<f14> z" 'my-open-neotree-at-root
;;   "<f14> z" '+neotree/open
;;   "<f14> <f1> c" 'org-schedule
;;   "S-<SPC> <SPC>" 'counsel-locate
;;   ;; "<f1>" 'org-agenda
;;   "<f12>" 'switch-window)
;; 
;; 
;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;; Splits
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; ;; split creation and navigation
;; (defun my-split-window (pos)
;;   (cond
;;    ((string= pos "right")
;;     (progn
;;       (split-window-horizontally)
;;       (evil-window-right 1)))
;;    ((string= pos "left")
;;     (split-window-horizontally))
;;    ((string= pos "up")
;;     (split-window-vertically))
;;    ((string= pos "down")
;;     (progn
;;       (split-window-vertically)
;;       (evil-window-down 1)))))
;; 
;; (general-nmap
;;   ;; Resizing
;;   "+" 'evil-window-increase-width
;;   "_" 'evil-window-decrease-width
;;   "M-=" 'evil-window-increase-height
;;   "M--" 'evil-window-decrease-height
;; 
;;   ;; split navigation
;;   "<down>" 'evil-window-down
;;   "<left>" 'evil-window-left
;;   "<up>" 'evil-window-up
;;   "<right>" 'evil-window-right
;; 
;;   ;; moving windows
;;   "<f13> <right>" #'+evil/window-move-right
;;   "<f13> <up>" #'+evil/window-move-up
;;   "<f13> <left>" #'+evil/window-move-left
;;   "<f13> <down>" #'+evil/window-move-down
;; 
;;   ;; split creation
;;   "<SPC> <right>" (lambda () (interactive) (my-split-window "right"))
;;   "<SPC> <up>" (lambda () (interactive) (my-split-window "up"))
;;   "<SPC> <left>" (lambda () (interactive) (my-split-window "left"))
;;   "<SPC> <down>" (lambda () (interactive) (my-split-window "down")))
;; 
;; 
;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;; dirs/files navigation
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; (defmacro create-folder-nmap (shortcut file-name)
;;   `(progn
;;      (defalias (intern (concat "cd-to-" (symbol-name (quote ,file-name))))
;;        (lambda ()
;;          (interactive)
;;          (cd
;;           (symbol-name
;;            (quote ,file-name)))))
;;      (general-nmap ,shortcut
;;        (intern
;;         (concat "cd-to-"
;;                 (symbol-name (quote ,file-name)))))))
;; 
;; (create-folder-nmap "<f15> H" ~/                 )
;; (create-folder-nmap "<f15> P" ~/projects/        )
;; (create-folder-nmap "<f15> S" ~/scrap/           )
;; (create-folder-nmap "<f15> R" ~/repos/           )
;; (create-folder-nmap "<f15> S" ~/repos/scenario/  )
;; (create-folder-nmap "<f15> T" ~/repos/test-ngp/  )
;; (create-folder-nmap "<f15> N" ~/repos/norby/     )
;; (create-folder-nmap "<f15> E" ~/.emacs.d         )
;; (create-folder-nmap "<f15> D" ~/.doom.d          )
;; (create-folder-nmap "<f15> O" ~/org              )
;; 
;; (general-nmap
;;   ;;files/directory management
;;   ;; "<f15> P" (lambda () (interactive) (cd "~/projects/"))
;;   ;; "<f15> S" (lambda () (interactive) (cd "~/scrap/"))
;;   ;; "<f15> R" (lambda () (interactive) (cd "~/repos/"))
;;   "<f15> l" (lambda () (interactive) (evil-edit "~/org/timeline.org"))
;;   "<f15> i" (lambda () (interactive) (evil-edit "~/org/triage.org"))
;;   "<f15> t" (lambda () (interactive) (evil-edit "~/org/todo.org"))
;;   "<f15> g" (lambda () (interactive) (evil-edit "~/org/gist.org"))
;;   "<f15> n" (lambda () (interactive) (evil-edit "~/org/notes.org"))
;;   "<f15> s" (lambda () (interactive) (evil-edit "~/org/scrap.org"))
;;   "<f15> r" (lambda () (interactive) (evil-edit "~/.zshrc"))
;;   "<f15> p" (lambda () (interactive) (evil-edit "~/org/projects.org"))
;;   "<f15> b" (lambda () (interactive) (evil-edit "~/org/books.org"))
;;   ;; "<f15> P" 'goto-raspberry-pi
;; 
;;   "<f15> c" (lambda () (interactive) (evil-edit "~/.emacs.d/init.el"))
;;   )
;; 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;; Macros
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; (fset 'mcs/lisp-insert-title
;;       [?4 ?5 ?a ?\; ?\; escape ?y ?y ?p ?p ?k ?l ?l ?l ?l ?l ?l ?h ?h ?d ?$ ?a ?  ?q backspace escape])
;; 
;; 
;; (defvar doom-escape-hook nil
;;   "A hook run when C-g is pressed (or ESC in normal mode, for evil users).
;; 
;; More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
;; all hooks after it are ignored.")
;; 
;; (defun doom/escape ()
;;   "Run `doom-escape-hook'."
;;   (interactive)
;;   (cond ((minibuffer-window-active-p (minibuffer-window))
;;          ;; quit the minibuffer if open.
;;          (abort-recursive-edit))
;;         ;; Run all escape hooks. If any returns non-nil, then stop there.
;;         ((run-hook-with-args-until-success 'doom-escape-hook))
;;         ;; don't abort macros
;;         ((or defining-kbd-macro executing-kbd-macro) nil)
;;         ;; Back to the default
;;         ((keyboard-quit))))
;; 
;; (global-set-key [remap keyboard-quit] #'doom/escape)
;; (global-set-key [remap evil-force-normal-state] #'doom/escape)
;; (global-set-key [escape] #'doom/escape)
