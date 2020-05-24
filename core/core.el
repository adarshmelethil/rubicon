(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq visible-bell -1)
(setq ring-bell-function 'ignore)
(setq display-line-numbers-type 'relative)
(setq +ivy-buffer-preview t)
(setq org-use-property-inheritance t)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

(display-battery-mode t)
(display-time-mode t)

(setq show-paren-style 'parenthesis)
(with-eval-after-load 'hl-line
  (set-face-attribute 'hl-line nil :background "#333333"))
(set-face-attribute 'show-paren-match nil :background "#FFFF00")
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)


;; split creation and navigation
(defun my-split-window (pos)
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

(defun doom/escape ()
  "Run `doom-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'doom-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(defmacro ilm (&rest body)
  `(lambda ()
     (interactive)
     ,@body))



(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer) 
	  (when (eq 'dired-mode (buffer-local-value 'major-mode buffer)) 
	    (kill-buffer buffer))) 
	(buffer-list)))


(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (kill-dired-buffers)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
