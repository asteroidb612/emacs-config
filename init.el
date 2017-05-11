;; Drew Lazzeri's emacs init file. 
;; This is a simplification reflecting a shift away from emacs.
;; I actually use the following

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;; Org files. I use emacs for org files. Especially this one
(find-file "~/.mjolner/agenda.org")
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;;Appearence
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)  ;;Disable opening message
(set-face-attribute 'fringe nil :background "#FFFFFF" :foreground "#2E2920") ;;Cleaner margins
(tool-bar-mode -1)


;; Evil Mode. Crucial. 
(add-to-list 'load-path "~/.emacs.d/lisp/evil-org-mode")
(require 'evil)
(require 'evil-org)
(require 'recentf)
(setq evil-find-skip-newlines t)
(evil-mode 1)


;; Recent mode. 
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'helm-recentf)

;; enable recent files mode.
(recentf-mode t)

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
