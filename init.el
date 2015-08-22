;; This is for all emaxen. 
;; Aquaemacs preferences file at: ~/Library/Preferences/Aquamacs Emacs/Preferences.el
;; Recursively add files to load path
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; This was here before 
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(setq org-src-fontify-natively t)

                                        ;Disable opening message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)  

(blink-cursor-mode 0)
(setq visible-bell t)

;; IDO Interactive Mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(setq evil-find-skip-newlines t)
(require 'evil)
(evil-mode 1)
;;(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
;;(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
;;(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
;;(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;;;;Tree Mode (for reddit mode)
;;(eval-after-load "tree-widget"
;;  '(if (boundp 'tree-widget-themes-load-path)
;;       (add-to-list 'tree-widget-themes-load-path "~/.emacs.d/")))
;;(autoload 'imenu-tree "imenu-tree" "Imenu tree" t)
;;(autoload 'tags-tree "tags-tree" "TAGS tree" t)

;;(load-library "reddit.el")
;;(require 'reddit)

;;Ace-Jump-Mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

                                        ;Make Ace-Jumps vim-like
(evil-define-motion evil-ace-jump-char-mode (count)
  :type exclusive
  (ace-jump-mode 5)
  (recursive-edit))

(evil-define-motion evil-ace-jump-line-mode (count)
  :type line
  (ace-jump-mode 9)
  (recursive-edit))

(evil-define-motion evil-ace-jump-word-mode (count)
  :type exclusive
  (ace-jump-mode 1)
  (recursive-edit))

(evil-define-motion evil-ace-jump-char-direct-mode (count)
  :type inclusive
  (ace-jump-mode 5)
  (forward-char 1)
  (recursive-edit))

(add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-char-mode)

(defun open-ledger-project ()
  ;;  (let ((default-directory "~/projects/ledger/ionic-ledger"))
  (split-window-right)
  (other-window 1)
  (shell)
  (rename-buffer "ionic server")
  (insert "cd ~/projects/ledger/ionic-ledger; ionic serve ")
  (split-window-below)
  (other-window 1)
  (shell)
  (rename-buffer "flask server")
  (insert "cd ~/projects/ledger; source venv/bin/activate; cd backend; python ledger_server.py ")
  (switch-to-buffer-other-window "app.js"))

(setq org-startup-indented t)
(find-file "~/.projects.org")

;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '((python . t)))
;;
;;(setq python-shell-interpreter "ipython"
;;            python-shell-interpreter-args "-i")

(require 'evil-leader)
(add-to-list 'load-path "~/.emacs.d/lisp/evil-org-mode")
(require 'evil-org)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(autoload 'extempore-mode "/usr/local/Cellar/extempore/0.59/extras/extempore.el" "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))
(setq user-extempore-directory "/path/to/extempore/")

(condition-case nil 
    ;;  (progn

    ;; Clean up that GUI
    (scroll-bar-mode -1)
  (tool-bar-mode 0)
  (menu-bar-mode 0)

  (interactive error))

;; Load packages so Custom can use them to theme
(setq package-enable-at-startup nil) (package-initialize)

                                        ; (custom-set-variables
                                        ;  ;; custom-set-variables was added by Custom.
                                        ;  ;; If you edit it by hand, you could mess it up, so be careful.
                                        ;  ;; Your init file should contain only one such instance.
                                        ;  ;; If there is more than one, they won't work right.
                                        ;  '(custom-enabled-themes (quote (solarized-light)))
                                        ;  '(custom-safe-themes
                                        ;    (quote
                                        ;     ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
                                        ;  '(mac-command-modifier nil)
                                        ;  '(mac-option-modifier (quote meta)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'diary-list-entries-hook 'diary-sort-entries t)

(require 'calfw)
(require 'calfw-cal)
(require 'calfw-ical)
(require 'calfw-org)
(defun google-cal ()
  (interactive)
  (cfw:open-ical-calendar"https://www.google.com/calendar/ical/dlazzeri1%40gmail.com/private-a4cb26c2b4d502bf59693bc7986450fc/basic.ics"))

(defun page-count ()
  "How many pages in GoogleDocs will this fill w/ Times, 12, doublespace"
  (interactive)
  (message "Page Count: %f" (/ (buffer-size) 1638.0)))

(defun buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-solarized-light)))
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(mac-command-modifier (quote super))
 '(mac-mouse-wheel-mode t)
 '(mac-option-modifier (quote meta)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(global-set-key (kbd "s-v") 'evil-paste-after)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

(defun spaniard()
  (interactive)
  (switch-to-buffer-other-frame "*Writing*")
  (setq modeline-format nil)
  (setq left-fringe-width 24)
  (setq right-fringe-width 24)
  (set-frame-parameter nil 'internal-border-width 30)
  (set-window-buffer 'BUFFER-OR-NAME "*Writing*"))

(setq enable-recursive-minibuffers t)

(global-set-key (kbd "C-x C-g") 'magit-status)

(setq vc-follow-symlinks nil)

(setq magit-last-seen-setup-instructions "1.4.0")
(tool-bar-mode -1)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;;;; flycheck
;;(add-hook 'after-init-hook #'global-flycheck-mode)
;;
;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
;;;;(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
;;;;(setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
;;;;(setq flycheck-standard-error-navigation nil)
;;
;;(global-flycheck-mode t)

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized))))) ;; start maximized

(setq magit-diff-options '("-w"))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(setq-default cursor-type 'hollow)
(setq-default x-stretch-cursor t)

;; Wrap lines with indentation. Fuck this is nice.
(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))
