(require 'org-tempo)
(require 'replel)

(set-face-attribute 'default nil :height 130)
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'org-mode-hook 'hl-line-mode)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(add-hook 'text-mode-hook 'flyspell-mode)
(dirtrack-mode)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(when (executable-find "fish")
  (setq-default explicit-shell-file-name "/usr/local/bin/fish"))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'e 'evil-edit)
(fringe-mode '(nil . 0))

(show-paren-mode 1)

(defun rubicon--disable-paren ()
  (setq-local show-paren-mode nil))

(add-hook 'org-mode-hook
	  #'rubicon--disable-paren)

(with-eval-after-load 
    'highlight-thing
  (set-face-attribute 'highlight-thing nil :background "#FFF" :foreground "#000"))

(setq-default fringes-outside-margins t)

(setq-default display-line-numbers-width 3)
(setq-default org-agenda-span 'month)

(define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
      nil nil 'bottom)

(defgroup rubicon-faces nil "Faces for my config" :group 'faces)

(setq rubicon--modeline-format
      (list ""
	    '(:eval
	      (rubicon/relative-default-dir))
	    "%b @ %p"
	    " | "
	    'mode-name
	    " |"
	    'vc-mode
	    " | "
	    'display-time-string
	    " | "
	    'battery-mode-line-string))

(setq-default mode-line-format nil)

(defun rubicon--modeline-face (inherits background-color)
  `((t :inherit ,inherits
       :background ,background-color
       :box (:line-width 5 :color ,background-color)
       :height 144)))

(let ((background-color "#1d2026"))
  (defface rubicon-modeline-active
    (rubicon--modeline-face 'mode-line background-color)
    "Face used when modeline is enabled and active"
    :group 'rubicon-faces)
  (defface rubicon-modeline-inactive
    (rubicon--modeline-face 'mode-line-inactive background-color)
    "Face used when modeline is enabled and inactive"
    :group 'rubicon-faces))

(dolist (remapping '((mode-line-inactive . rubicon-modeline-inactive)
		     (mode-line . rubicon-modeline-active)))
  (add-to-list 'face-remapping-alist remapping t))

(defun rubicon/enable-modeline ()
  (setq mode-line-format rubicon--modeline-format))

(defun rubicon/disable-modeline ()
  (setq mode-line-format nil))

(dolist (enable-modeline-mode-hook (list 'prog-mode-hook
					 'yaml-mode-hook
					 'org-mode-hook))
  (add-hook enable-modeline-mode-hook 'rubicon/enable-modeline))

(setq rubicon-home-path (expand-file-name "~"))
(setq rubicon--home-path-rg-starts-with (concat "^" rubicon-home-path))

(defun rubicon/relative-default-dir ()
  (s-replace-regexp  rubicon--home-path-rg-starts-with
		     "~"
		     default-directory))



(setq
 org-default-notes-file  "~/org/notes.org"
 org-habit-show-habits-only-for-today nil
 window-divider-default-bottom-width 1
 window-divider-default-right-width 1
 display-time-24hr-format t
 display-battery-mode t
 display-time-mode t
 dired-use-ls-dired nil
 dired-listing-switches "-alh"
 enable-recursive-minibuffers t
 evil-snipe-smart-case t
 evil-snipe-scope 'line
 evil-snipe-repeat-scope 'visible
 frame-resize-pixelwise t
 ns-use-native-fullscreen t
 ns-auto-hide-menu-bar t
 org-agenda-files '("~/org/")
 evil-snipe-char-fold t
 company-show-numbers t
 evil-snipe-spillover-scope t
 org-indent-indentation-per-level 1
 org-src-preserve-indentation t
 org-agenda-include-deadlines t
 org-src-tab-acts-natively t
 org-confirm-babel-evaluate nil
 org-link-elisp-confirm-function nil
 inhibit-startup-screen t
 org-hide-leading-stars t
 org-adapt-indentation t
 ns-use-native-fullscreen nil
 org-odd-levels-only t     
 backup-inhibited t
 visible-bell -1
 ring-bell-function 'ignore
 display-line-numbers-type 'relative
 +ivy-buffer-preview t
 org-use-property-inheritance t
 wdired-allow-to-change-permissions t
 show-paren-style 'parenthesis)

(customize-set-variable 'horizontal-scroll-bar-mode nil)
(display-battery-mode t)
(display-time-mode t)
(menu-bar-bottom-and-right-window-divider)
(set-face-attribute 'show-paren-match nil :background "#FFFF00")
(set-face-attribute 'evil-ex-lazy-highlight nil :background "#11888c")

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; split creation and navigation
(defun rubicon/split-window (pos)
  (cond
   ((string= pos "right")
    (progn
      (split-window-horizontally)
      (evil-window-right 1)))
   ((string= pos "left")
    (split-window-horizontally))
   ((string= pos "up")
    (split-window-vertically))
   ((string= pos "down")
    (progn
      (split-window-vertically)
      (evil-window-down 1)))))




(defun rubicon/escape ()
  (interactive)
  (evil-ex-nohighlight)
  (cond ((minibuffer-window-active-p (minibuffer-window))
	 ;; quit the minibuffer if open.
	 (abort-recursive-edit))
	;; don't abort macros
	((or defining-kbd-macro executing-kbd-macro) nil)
	;; Back to the default
	((keyboard-quit))))

(defmacro ilm (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

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

;; Doom helper functions
(defun +evil--window-swap (direction)
  "Move current window to the next window in DIRECTION.
If there are no windows there and there is only one window, split in that
direction and place this window there. If there are no windows and this isn't
the only window, use evil-window-move-* (e.g. `evil-window-move-far-left')."
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
	 (this-buffer (current-buffer))
	 (that-window (windmove-find-other-window direction nil this-window))
	 (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
	      (window-dedicated-p this-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
	(funcall (pcase direction
		   ('left  #'evil-window-move-far-left)
		   ('right #'evil-window-move-far-right)
		   ('up    #'evil-window-move-very-top)
		   ('down  #'evil-window-move-very-bottom)))
      (unless that-window
	(setq that-window
	      (split-window this-window nil
			    (pcase direction
			      ('up 'above)
			      ('down 'below)
			      (_ direction))))
	(setq that-buffer (window-buffer that-window)))
      (with-selected-window this-window
	(switch-to-buffer that-buffer))
      (with-selected-window that-window
	(switch-to-buffer this-buffer))
      (select-window that-window))))


;;;###autoload
(defun +evil/window-move-left ()
  "Swap windows to the left."
  (interactive) (+evil--window-swap 'left))
;;;###autoload
(defun +evil/window-move-right ()
  "Swap windows to the right"
  (interactive) (+evil--window-swap 'right))
;;;###autoload
(defun +evil/window-move-up ()
  "Swap windows upward."
  (interactive) (+evil--window-swap 'up))
;;;###autoload
(defun +evil/window-move-down ()
  "Swap windows downward."
  (interactive) (+evil--window-swap 'down))


;; Eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook 'eshell-smart-initialize)



(defun rubicon/eshell-here ()
  (interactive)
  (let ((new-buffer (generate-new-buffer "*eshell*")))
    (switch-to-buffer new-buffer)
    (eshell-mode)))

;; Dired
(setq dired-auto-revert-buffer t  ; don't prompt to revert; just do it
      dired-dwim-target t  ; suggest a target for moving/copying intelligently
      dired-hide-details-hide-symlink-targets nil
      ;; Always copy/delete recursively
      dired-recursive-copies  'always
      dired-recursive-deletes 'top)

;; ORG mode

(add-hook 'org-mode-hook (lambda () (text-scale-adjust 1)))

(with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) ""))

(setq org-superstar-headline-bullets-list '("#")
      org-todo-keywords
      '((sequence
	 "TODO(t)"  ; A task that needs doing & is ready to do
	 "PROJ(p)"  ; A project, which usually contains other tasks
	 "ON-GOING(s)"  ; A task that is in progress
	 "BLOCKED(w)"  ; Something external is holding up this task
	 "HOLD(h)"  ; This task is paused/on hold because of me
	 "|"
	 "DONE(d)"  ; Task successfully completed
	 "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
	(sequence
	 "[ ](T)"   ; A task that needs doing
	 "[-](S)"   ; Task is in progress
	 "[?](W)"   ; Task is being held up or paused
	 "|"
	 "[X](D)")) ; Task was completed
      org-todo-keyword-faces
      '(
	("STRT" . +org-todo-active)
	("[?]"  . +org-todo-onhold)
	("WAIT" . +org-todo-onhold)
	("HOLD" . +org-todo-onhold)
	("PROJ" . +org-todo-project)))


(setq org-log-done 'time)

(defun +org--refresh-inline-images-in-subtree ()
  "Refresh image previews in the current heading/tree."
  (if (> (length org-inline-image-overlays) 0)
      (org-remove-inline-images)
    (org-display-inline-images
     t t
     (if (org-before-first-heading-p)
	 (line-beginning-position)
       (save-excursion (org-back-to-heading) (point)))
     (if (org-before-first-heading-p)
	 (line-end-position)
       (save-excursion (org-end-of-subtree) (point))))))


(defun +org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point.
If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (let* ((context (org-element-context))
	 (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
	    type (org-element-type context)))
    (pcase type
      (`headline
       (cond ((and (fboundp 'toc-org-insert-toc)
		   (member "TOC" (org-get-tags)))
	      (toc-org-insert-toc)
	      (message "Updating table of contents"))
	     ((string= "ARCHIVE" (car-safe (org-get-tags)))
	      (org-force-cycle-archived))
	     ((or (org-element-property :todo-type context)
		  (org-element-property :scheduled context))
	      (org-todo
	       (if (eq (org-element-property :todo-type context) 'done)
		   (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
		       'todo)
		 'done)))
	     (t
	      (+org--refresh-inline-images-in-subtree)
	      (org-clear-latex-preview)
	      (org-latex-preview '(4)))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-reference
       (org-footnote-goto-definition (org-element-property :label context)))

      (`footnote-definition
       (org-footnote-goto-previous-reference (org-element-property :label context)))

      ((or `planning `timestamp)
       (org-follow-timestamp-link))

      ((or `table `table-row)
       (if (org-at-TBLFM-p)
	   (org-table-calc-current-TBLFM)
	 (ignore-errors
	   (save-excursion
	     (goto-char (org-element-property :contents-begin context))
	     (org-call-with-arg 'org-table-recalculate (or arg t))))))

      (`table-cell
       (org-table-blank-field)
       (org-table-recalculate arg)
       (when (and (string-empty-p (string-trim (org-table-get-field)))
		  (bound-and-true-p evil-local-mode))
	 (evil-change-state 'insert)))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies arg)))

      ((or `src-block `inline-src-block)
       (org-babel-execute-src-block arg))

      ((or `latex-fragment `latex-environment)
       (org-latex-preview arg))

      (`link
       (let* ((lineage (org-element-lineage context '(link) t))
	      (path (org-element-property :path lineage)))
	 (if (or (equal (org-element-property :type lineage) "img")
		 (and path (image-type-from-file-name path)))
	     (+org--refresh-inline-images-in-subtree)
	   (org-open-at-point arg))))

      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
	 (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (_
       (if (or (org-in-regexp org-ts-regexp-both nil t)
	       (org-in-regexp org-tsr-regexp-both nil  t)
	       (org-in-regexp org-link-any-re nil t))
	   (call-interactively #'org-open-at-point)
	 (+org--refresh-inline-images-in-subtree))))))

(defun +org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
	     in (cl-remove-if-not #'listp org-todo-keywords)
	     for keywords =
	     (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
				     (match-string 1 x)
				   x))
		     keyword-spec)
	     if (eq type 'sequence)
	     if (member keyword keywords)
	     return keywords)))

(add-hook 'org-mode-hook 'org-indent-mode)

;;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)
			     (awk . t)
			     (clojure . t)
			     (emacs-lisp . t)
			     (haskell . t)
			     (gnuplot . t)
			     (lisp . t)
			     (makefile . t)
			     (org . t)
			     (ruby . t)
			     (sqlite . t)
			     (sql . t)
			     (C . t)))

(add-hook 'org-mode-hook 'org-overview)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vterm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun vterm-send-escape()
  (interactive)
  (vterm-send-key "<escape>"))

(with-eval-after-load 'vterm
  (defun vterm (&optional buffer-name)
    "Create a new vterm."
    (interactive)
    (let ((buffer (generate-new-buffer (or buffer-name "vterm"))))
      (with-current-buffer buffer
        (vterm-mode))
      (switch-to-buffer buffer))))

(defun vterm-run-and-go-up ()
  (interactive)
  (vterm-send-return)
  (vterm-send-up))

(defun my-vterm-normal-mode ()
  (interactive)
  (evil-force-normal-state)
  (vterm-copy-mode))

(defun my-vterm-insert ()
  (interactive)
  (vterm-copy-mode -1)
  (evil-insert 1))

(defun my-vterm-append ()
  (interactive)
  (vterm-copy-mode -1)
  (evil-append 1))

(defun my-vterm-clear ()
  (interactive)
  (vterm-clear-scrollback)
  (vterm-clear))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Vterm auto configurations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vt-exec (str)
  (vterm-send-string str)
  (vterm-send-return))

(defun vt-eq (key val)
  (vt-exec
   (format "%s=\"%s\"" key val)))

(defun vt-alias (key val)
  (vt-exec
   (format "alias %s=\"%s\"" key val)))

(defun vt-export (key val)
  (vt-exec
   (format "export %s=%s" key val)))

(defun vt-append-path (path)
  (vt-export
   "PATH"
   (format "%s:$PATH" path)))

(defun vt-source-zshrc ()
  (interactive)
  (vt-exec "source ~/.zshrc"))

(defun vt-cd-to (path)
  (interactive)
  (vt-exec
   (format "cd %s" path)))

(defun vt-pusdh (path)
  (vt-exec
   (format "pushd %s" path)))

(defun vt-popd ()
  (vt-exec
   (format "popd")))

(defun vt-insert-command (cmd)
    (vterm-send-string cmd)
    (evil-insert 1))

(defun vt-ls ()
    (vt-exec "ls -la"))

(defun vt-clear-current-command ()
  (vterm-send-escape)
  (vterm-send-string "dd")
  (vterm-send-string "i"))

(defun vt-insert-at-start (cmd) ;; requires vi mode
  (vterm-send-escape)
  (vterm-send-string "m")
  (vterm-send-string "p")
  (vterm-send-string "0i")
  (vterm-send-string cmd)
  (vterm-send-escape)
  (vterm-send-string "`p")
  (let ((cmd-size (length cmd))
        (cursor 0))
    (while (< cursor cmd-size)
      (vterm-send-string "l")
      (setq cursor (+ cursor 1))))
  (vterm-send-string "a"))

(defun vt-inset-at-point (cmd)
  (vterm-send-escape)
  (vterm-send-string "i")
  (vterm-send-string cmd))

(defun vt-add-sudo ()
    (interactive)
    (vt-insert-at-start "sudo "))

(defun vt-add-chmod ()
  (interactive)
  (vt-insert-at-start "chmod u+x "))

(defun vt-rc ()
  (interactive)
  (vt-append-path "~/bin/")
  (vt-exec "bindkey -v")
  (vt-eq "PROMPT" "%n %5~# ")
  (vt-alias "l" "ls")
  (vt-alias "c" "clear")
  (vt-alias "ktl" "kubectl")
  ;; (vt-alias "la" "ls -lAh")
  (vt-alias "la" "ls -lAh")
  (vt-alias "ll" "ls -lh")
  (vt-alias "pod" "popd")
  (vt-alias "pd" "pushd")
  (vt-alias "...." "cd ../../..")
  (vt-alias "..." "cd ../..")
  (vt-alias ".." "cd .."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rubicon/workspace-switch (name)
  (interactive)
  (persp-switch name)
  (rubicon/workspace-show-all))

(defun rubicon/workspace-delete ()
  (interactive)
  (persp-kill (persp-current-name))
  (rubicon/workspace-show-all))

(defun rubicon/workspace-get-marked-list ()
  (--map
   (if (string= (persp-current-name) it)
       (concat ">" it "<")
     (concat " " it " "))
   (persp-names)))

(defun rubicon/workspace-show-all ()
  (interactive)
  (message
   (string-join (rubicon/workspace-get-marked-list) " ")))

(defun rubicon/workspace-is-last-buffer? ()
  (interactive)
  (not (> (length (persp-current-buffer-names)) 1)))

(defun rubicon/workspace-kill-current-buffer ()
  (interactive)
  (if (not (rubicon/workspace-is-last-buffer?))
      (kill-current-buffer)
    (message "Can't delete last workspace buffer")))

(defun rubicon/workspace-quit-window ()
  (interactive)
  (if (not (rubicon/workspace-is-last-buffer?))
      (quit-window)
    (message "Can't delete last workspace buffer")))

(defun rubicon/visible-buffers ()
  "Returns a list of all currently visible buffers"
  (mapcar 'window-buffer (window-list)))

(defun rubicon/workspace-current-get-all-buffers ()
  "Returns a list of all buffers in current perspective"
  (persp-current-buffers))

(defun rubicon/workspace-current-get-invisible-buffers ()
  "returns a list of all invisible buffers in current perspective"
  (-difference (rubicon/workspace-current-get-all-buffers)
	       (rubicon/visible-buffers)))

(defun rubicon/kill-selected-buffers (selected-buffers)
  "Kills all buffers given to it"
  (--map (kill-buffer it) selected-buffers))


(defun rubicon/workspace-current-get-other-buffers ()
  "Get buffers other than the current buffers in the current perspective"
  (-difference
   (rubicon/workspace-current-get-all-buffers)
   (list (current-buffer))))

(defun rubicon/workspace-kill-invisible-buffers ()
  "Kills all invisible buffers in perspective"
  (interactive)
  (rubicon/kill-selected-buffers
   (rubicon/workspace-current-get-invisible-buffers)))

(defun rubicon/workspace-kill-other-buffers ()
  "Kills all buffers other than current one in perspective"
  (interactive)
  (delete-other-windows)
  (rubicon/kill-selected-buffers
   (rubicon/workspace-current-get-other-buffers)))

(defun rubicon/copy-path-to-buffer-file ()
  (interactive)
  (let ((path-to-file (or (buffer-file-name) default-directory)))
    (message path-to-file)
    (kill-new path-to-file)))

;; TODO
(defun rebuicon/workspace-delete-all ()
  (interactive)
  (rubicon/kill-selected-buffers (buffer-list)))

(defun me/bash-history ()
  (interactive)
  (let ((command
	 (with-temp-buffer
	   (insert-file-contents-literally "~/.zsh_history")
	   (let ((history-list (split-string (buffer-string) "\n" t)))
	     (completing-read "Command: " history-list )))))
    (when command
      (insert command))))



(defun rubicon/get-zsh-history ()
  (with-temp-buffer
    (insert-file-contents-literally "~/.zsh_history")
    (reverse (split-string (s-replace-regexp ": .*:0;" "" (buffer-string)) "\n" t))))

(defun rubicon/zsh-history ()
  (interactive)
  (insert
   (completing-read
    "Command: " (rubicon/get-zsh-history))))

(defun rubicon/gen-random-str ()
 (--reduce (format "%s%d" acc (random 10000)) (number-sequence 0 8)))

(defun rubicon/create-disposable-dir ()
  (interactive)
  (let ((path (format "~/scrap/%s" (rubicon/gen-random-str))))
    (dired-create-directory path)
    (e path)))

(defun rubicon/run-repl ()
  (interactive)
  (comint-run
   (completing-read
   "Select REPL"
   '("python" "node" "clj" "bash"))))

(defun +ivy/projectile-find-file ()
  "A more sensible `counsel-projectile-find-file', which will revert to
`counsel-find-file' if invoked from $HOME or /, `counsel-file-jump' if invoked
from a non-project, `projectile-find-file' if in a big project (more than
`ivy-sort-max-size' files), or `counsel-projectile-find-file' otherwise.

The point of this is to avoid Emacs locking up indexing massive file trees."
  (interactive)
  ;; Spoof the command so that ivy/counsel will display the (well fleshed-out)
  ;; actions list for `counsel-find-file' on C-o. The actions list for the other
  ;; commands aren't as well configured or are empty.
  (let ((this-command 'counsel-find-file))
    (call-interactively
     (cond ((or (file-equal-p default-directory "~")
                (file-equal-p default-directory "/"))
            #'counsel-find-file)

           ((projectile-project-p default-directory)
            (let ((files (projectile-current-project-files)))
              (if (<= (length files) ivy-sort-max-size)
                  #'counsel-projectile-find-file
                #'projectile-find-file)))
           (#'counsel-file-jump)))))
