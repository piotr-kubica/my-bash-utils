;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/  for a explanation of color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html  for a more technical explanation.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(setq my-color-themes
      (list
       'tomorrow-night-bright
       'klere))

(setq theme-current my-color-themes)

(defun load-next-theme ()            
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current my-color-themes))
  (load-theme (car theme-current))
  (message "%S" (car theme-current)))
    
(global-set-key [f4] 'load-next-theme)

;; setup theme at startup
(load-theme 'tomorrow-night-bright t)

;; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; setup keywords and colors
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN PROGRESS(i)" "|" "DONE(d)" "DISMISSED(s)")))
(setq org-todo-keyword-faces
      '(("IN PROGRESS" . (:foreground "red" :weight bold))))


(setq org-agenda-files (list "~/Dropbox/org/notes.org"
			     "~/Dropbox/org/work.org"))
      
;; open notes.org
(find-file "~/Dropbox/org/notes.org")

;; goto by percent
(defun goto-percent (percent)
  "Goto PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(define-minor-mode sticky-buffer-mode
  "Make the current windows always display this buffer"
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;; prevents from opening new frames
(setq-default display-buffer-reuse-frames t)

;; moving between windows with <shift + arrow_key>
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; binding key for magic for .git repository
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(defun kill-invisible-buffers ()
  "Kill all buffers not currently shown in a window somewhere."
  (interactive)
  (dolist (buf  (buffer-list))
    (unless (get-buffer-window buf 'visible) (kill-buffer buf))))

;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-c l") 'mc/edit-lines)
(global-set-key (kbd "C-c ^") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c $") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c .") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c #") 'mc/insert-numbers)
(global-set-key (kbd "C-c s") 'mc/sort-regions)
(global-set-key (kbd "C-c r") 'mc/reverse-regions)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "c7cc498270cd943b0e82e184e91adc671f5cde467940af36da48688fb1763f0c" default)))
 '(org-agenda-files (quote ("~/Dropbox/org/notes.org")))
 '(package-selected-packages
   (quote
    (klere-theme projectile shell-here dired+ smex magit elpy))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'ido)
(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
               "*Messages*" "Async Shell Command"))

;; line numbers
(global-linum-mode t)
(set-face-foreground 'linum "gray")

;; open shell in current window
(defun shell-same-window-advice (orig-fn &optional buffer)
  "Advice to make `shell' reuse the current window. Intended as :around advice."
  (let* ((buffer-regexp
          (regexp-quote
           (cond ((bufferp buffer)  (buffer-name buffer))
                 ((stringp buffer)  buffer)
                 (:else             "*shell*"))))
         (display-buffer-alist
          (cons `(,buffer-regexp display-buffer-same-window)
                display-buffer-alist)))
    (funcall orig-fn buffer)))
;; add advice
(advice-add 'shell :around #'shell-same-window-advice)

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;; overwrite selected text while typing
(delete-selection-mode 1)

;; auto complete
;; (ac-config-default)

;; save emacs session
(require 'desktop)
;; save desktop file to fixed directory
(setq desktop-path (list "~/.emacs.d"))
(desktop-save-mode 1)

;; python elpy mode
(elpy-enable)

;; use ipython in shell
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(defun elpy-shell()
  (interactive)
  (previous-multiframe-window)
  (elpy-shell-switch-to-shell))

;; y for yes, n for no
(defalias 'yes-or-no-p 'y-or-n-p)

;; show parens mode
(show-paren-mode 1)

;; projectile
(projectile-global-mode 1)
