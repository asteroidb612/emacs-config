;;; flymake-gjshint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "flymake-gjshint" "flymake-gjshint.el" (21919
;;;;;;  7293 0 0))
;;; Generated autoloads from flymake-gjshint.el

(defvar flymake-gjshint 'both "\
Tool(s) to check syntax Javascript source code.

Must be one of `both', `jshint', `gjslint' or nil.
Set `both' to enable both `jshint' and `gjslint'.
If This variable is nil, flymake is disabled.")

(custom-autoload 'flymake-gjshint "flymake-gjshint" t)

(defvar flymake-gjshint:jshint-configuration-path nil "\
Absolute Path to a JSON configuration file for JSHint.

If you locate `.jshintrc` in home directory, you need not to set this variable.
JSHint will look for this file in the current working directory
and, if not found, will move one level up the directory tree all
the way up to the filesystem root.")

(custom-autoload 'flymake-gjshint:jshint-configuration-path "flymake-gjshint" t)

(defvar flymake-gjshint:gjslint-flagfile-path nil "\
Absolute Path to a configuration file for Closure Linter.")

(custom-autoload 'flymake-gjshint:gjslint-flagfile-path "flymake-gjshint" t)

(defvar flymake-gjshint:jshint-command "jshint" "\
Name (and optionally full path) of jshint executable.")

(custom-autoload 'flymake-gjshint:jshint-command "flymake-gjshint" t)

(defvar flymake-gjshint:gjslint-command "gjslint" "\
Name (and optionally full path) of gjslint executable.")

(custom-autoload 'flymake-gjshint:gjslint-command "flymake-gjshint" t)

(defvar flymake-gjshint:fixjsstyle-command "fixjsstyle" "\
Name (and optionally full path) of fixjsstyle executable.")

(custom-autoload 'flymake-gjshint:fixjsstyle-command "flymake-gjshint" t)

(autoload 'flymake-gjshint:fixjsstyle "flymake-gjshint" "\
Fix many of the glslint errors in current buffer by fixjsstyle.

\(fn)" t nil)

(autoload 'flymake-gjshint:load "flymake-gjshint" "\
Configure flymake mode to check the current buffer's javascript syntax.

This function is designed to be called in `js-mode-hook' or
equivalent; it does not alter flymake's global configuration, so
function `flymake-mode' alone will not suffice.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flymake-gjshint-autoloads.el ends here
