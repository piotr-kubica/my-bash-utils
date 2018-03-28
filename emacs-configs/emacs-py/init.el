
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

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
;; multiple cursors alternative mapping
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (shell-here dired+ smex magit elpy))))
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

;; auto complete
;; (ac-config-default)

;; save emacs session
(desktop-save-mode 1)

;; restore desktop
(desktop-read)

;; python elpy mode
(elpy-enable)

;; use ipython in shell
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
