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
  "q" (lambda () (interactive) 'evil-record-macro
  "w" (lambda () (interactive) (list-buffers) (other-window 1))
  "k" (lambda () (interactive) (shell-command "/usr/local/bin/brightness 0"))
  "j" (lambda () (interactive) (shell-command "/usr/local/bin/brightness .70"))))

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
 '(blink-cursor-mode nil)
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(mac-mouse-wheel-mode t)
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote super))
 '(org-agenda-files
   (quote
    ("~/.mjolner/facillitator.org" "~/.mjolner/agenda.org" "~/.mjolner/pietas.org" "~/.mjolner/classes/physics.org" "~/.mjolner/pensieve.org" "~/.mjolner/budget.org" "~/.mjolner/map.org" "~/.mjolner/prof_dates.org" "~/schedule.org" "~/.mjolner/correspondence.org" "~/.mjolner/major.org" "~/.mjolner/recommendations.org" "~/.mjolner/work.org" "~/.mjolner/virgil.org")))
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-span (quote day))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-todo-ignore-deadlines (quote all))
 '(org-agenda-todo-ignore-scheduled (quote all))
 '(org-archive-location "~/.mjolner/archive.org::* From %s")
 '(org-deadline-warning-days 3)
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-todo-keywords (quote ((sequence "TODO" "DOING" "|" "DONE" "SKIPPED"))))
 '(package-selected-packages
   (quote
    (goto-chg goto-last-change evil-leader evil yaml-mode web-mode solarized-theme sass-mode request rainbow-delimiters ox-twbs magit helm haskell-mode hackernews git-rebase-mode git-commit-mode flymake-gjshint flycheck-tip discover debbugs csv-mode color-theme-sanityinc-solarized adaptive-wrap)))
 '(visible-bell nil)
 '(x-stretch-cursor nil))

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
(define-key evil-normal-state-map (kbd "q") (lambda () 
                                              (interactive) 
                                              (if (eq 1 (count-windows))
                                                      (next-buffer)
                                                      (delete-window))))
;(define-key evil-normal-state-map (kbd "/") 'avy-goto-word-or-subword-1)
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
(find-file "~/.mjolner/agenda.org")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Menlo"))))
 '(mode-line ((t (:background "grey90" :foreground "black" :overline t :weight normal :height 0.8 :width normal))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray100" :foreground "grey20" :weight light)))))

(require 'evil-magit)

;;Adding this gets rid of several org problems 
;; including not clocking or exporting 
(org-reload)
(evil-set-initial-state 'org-agenda-mode 'emacs)
(evil-set-initial-state 'magit-popup-mode 'emacs)

;;Cleaner margins
(set-face-attribute 'fringe nil :background "#FFFFFF" :foreground "#2E2920")


(setq org-clock-persist t)
(org-clock-persistence-insinuate)
