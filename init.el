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

;;Disable opening message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)  

(blink-cursor-mode 0)
(setq visible-bell t)

;; IDO Interactive Mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(require 'calfw)
(require 'calfw-org)
(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode +1)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "a" 'org-agenda
  "b" 'switch-to-buffer
  "c" 'cfw:open-org-calendar
  "s" 'toggle-frame-fullscreen
  "k" (lambda () (interactive) (shell-command "/usr/local/bin/brightness 0"))
  "j" (lambda () (interactive) (shell-command "/usr/local/bin/brightness .70")))



(setq evil-find-skip-newlines t)
(evil-mode 1)
                                        ;(require 'evil-magit)

(add-hook 'org-agenda-mode-hook 
          (lambda () 
            (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
            (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)))

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
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(condition-case nil 
    (scroll-bar-mode -1)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (interactive))

;; Load packages so Custom can use them to theme
(setq package-enable-at-startup nil)
(package-initialize)

(defun page-count ()
  "How many pages in GoogleDocs will this fill w/ Times, 12, doublespace"
  (interactive)
  (message "Page Count: %f" (/ (buffer-size) 1638.0)))

(defun buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(mac-mouse-wheel-mode t)
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote super))
 '(org-agenda-files
   (quote
    ("~/.mjolner/map.org" "~/.mjolner/agenda.org" "~/.mjolner/prof_dates.org" "~/.mjolner/crazyhorse.org" "~/schedule.org" "~/.mjolner/classes/political_science_research.org" "~/.mjolner/classes/numerical_analysis.org" "~/.mjolner/classes/linear_algebra.org" "~/.mjolner/classes/darwin_and_god.org" "~/.mjolner/correspondence.org" "~/.mjolner/major.org" "~/.mjolner/recommendations.org" "~/.mjolner/work.org" "~/.mjolner/virgil.org")))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-span (quote day))
 '(org-agenda-todo-ignore-deadlines (quote all))
 '(org-agenda-todo-ignore-scheduled (quote all))
 '(org-deadline-warning-days 3)
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-todo-keywords (quote ((sequence "TODO" "DOING" "DONE"))))
 '(package-selected-packages
   (quote
    (goto-chg goto-last-change evil-leader evil yaml-mode web-mode solarized-theme sass-mode request rainbow-delimiters ox-twbs magit helm haskell-mode hackernews git-rebase-mode git-commit-mode flymake-gjshint flycheck-tip discover debbugs csv-mode color-theme-sanityinc-solarized adaptive-wrap)))
 '(visible-bell nil))

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

;; start maximized

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
(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-startup-indented t)
;;(shell)
;;(split-window-right)
;;(other-window 1)
;;(find-file "~/.map.org")
;;(split-window-below)
;;(other-window 1)
;;(find-file "~/.projects.org")
;;(other-window 1)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'show-paren-mode )
(org-agenda nil "n")
(delete-other-windows)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'evil-magit)

;;Adding this gets rid of several org problems 
;; including not clocking or exporting 
(org-reload)
